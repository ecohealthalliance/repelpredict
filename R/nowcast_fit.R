# define generic fit
#' @export
repel_fit <- function(x, ...){
  UseMethod("repel_fit")
}

#' Fit nowcast BART model object
#' @return list containing predicted count and whether disease is expected or not (T/F)
#' @import dplyr tidyr
#' @importFrom dbarts bart2
#' @importFrom readr write_rds
#' @importFrom assertthat assert_that
#' @importFrom here here
#' @export
repel_fit.nowcast_bart <- function(model_object,
                                   augmented_data,
                                   output_directory,
                                   n_chains = dbarts::guessNumCores(),
                                   verbose = interactive()) {

  # set threads for computing
  existing_blas <- RhpcBLASctl::blas_get_num_procs()
  RhpcBLASctl::blas_set_num_threads(1)
  on.exit(RhpcBLASctl::blas_set_num_threads(existing_blas))

  # Status model ------------------------------------------------------------
  # modify data prior to running
  modified_disease_status_data <- augmented_data %>%
    drop_na(disease_status) %>%
    select(-cases, -report_year)

  # confirm no NAs or Inf
  assertthat::assert_that(all(map_lgl(modified_disease_status_data, ~all(!is.na(.)))))
  assertthat::assert_that(all(map_lgl(modified_disease_status_data, ~all(!is.infinite(.)))))
  assertthat::assert_that(all(map_lgl(modified_disease_status_data, ~all(!is.nan(.)))))
  assertthat::assert_that(all(map_lgl(modified_disease_status_data, ~all(!is.null(.)))))

  # run model
  message("Running disease status (binary) model")
  bart_mod_disease_status <- dbarts::bart2(formula = as.formula(paste("disease_status", "~.")),
                                           data = modified_disease_status_data,
                                           test = select(modified_disease_status_data, -disease_status),
                                           n.chains = n_chains,
                                           keepTrees = TRUE,
                                           combineChains = TRUE,
                                           verbose = verbose)
  ## note: test = Explanatory variables for test (out of sample) data. Should have same column structure as x.train. bart will generate draws of f(x) for each x which is a row of x.test.

  # save model
  ## note: "To do this, one must “touch” the sampler's state object before saving"
  invisible(bart_mod_disease_status$fit$state)
  write_rds(list(bart_mod_disease_status, modified_disease_status_data), here::here(paste0(output_directory, "/bart_mod_", "disease_status", ".rds")))

  # Case model ------------------------------------------------------------
  # modify data prior to running (only running on positive counts)
  modified_cases_data <- augmented_data %>%
    drop_na(cases) %>%
    filter(cases > 0) %>%
    select(-disease_status, -report_year)

  # confirm no NAs or Inf
  assertthat::assert_that(all(map_lgl(modified_cases_data, ~all(!is.na(.)))))
  assertthat::assert_that(all(map_lgl(modified_cases_data, ~all(!is.infinite(.)))))
  assertthat::assert_that(all(map_lgl(modified_cases_data, ~all(!is.nan(.)))))
  assertthat::assert_that(all(map_lgl(modified_cases_data, ~all(!is.null(.)))))

  # run model
  message("Running cases (count) model")
  bart_mod_cases <- dbarts::bart2(formula = as.formula(paste("cases", "~.")),
                                  data = modified_cases_data,
                                  test = select(modified_cases_data, -cases),
                                  n.chains = n_chains,
                                  keepTrees = TRUE,
                                  combineChains = TRUE,
                                  verbose = verbose)
  ## note: test = Explanatory variables for test (out of sample) data. Should have same column structure as x.train. bart will generate draws of f(x) for each x which is a row of x.test.

  # save model
  ## note: "To do this, one must “touch” the sampler's state object before saving"
  invisible(bart_mod_cases$fit$state)
  write_rds(list(bart_mod_cases, modified_cases_data), here::here(paste0(output_directory, "/bart_mod_", "cases", ".rds")))

  return(NULL)
}


#' Fit nowcast Boost model object
#' @return list containing predicted count and whether disease is expected or not (T/F)
#' @import dplyr tidyr
#' @importFrom readr write_rds
#' @importFrom assertthat assert_that
#' @importFrom here here
#' @export
repel_fit.nowcast_boost <- function(model_object,
                                    augmented_data,
                                    output_directory,
                                    verbose = interactive()) {

  # disease lookup (move to augment)
  disease_lookup <- augmented_data %>%
    distinct(disease) %>%
    mutate(disease_clean = janitor::make_clean_names(disease))

  # modify data prior to running
  modified_disease_status_data <- augmented_data %>%
    drop_na(disease_status) %>%
    mutate(report_semester_1 = as.numeric(report_semester == 1)) %>%
    select(-cases, -report_year, -report_semester) %>%
    mutate_if(is.integer, as.double) %>%
    mutate(disease_status = factor(disease_status)) %>%
    filter(country_iso3c %in% c("USA", "ESP", "CHN", "AUS", "BEL", "THA")) %>%
    left_join(disease_lookup) %>%
    select(-disease) %>%
    rename(disease = disease_clean)

  # last modification steps in recipe
  modified_disease_status_recipe <- modified_disease_status_data %>%
    recipe(disease_status ~ .) %>%
    step_dummy(all_nominal(), -disease_status)

  # confirm no NAs or Inf
  test_modified_disease_status_rec <- juice(prep(modified_disease_status_recipe))

  assertthat::assert_that(all(map_lgl(test_modified_disease_status_rec, ~all(!is.na(.)))))
  assertthat::assert_that(all(map_lgl(test_modified_disease_status_rec, ~all(!is.infinite(.)))))
  assertthat::assert_that(all(map_lgl(test_modified_disease_status_rec, ~all(!is.nan(.)))))
  assertthat::assert_that(all(map_lgl(test_modified_disease_status_rec, ~all(!is.null(.)))))

  # prep the recipe
  modified_disease_status_prep <- prep(modified_disease_status_recipe, training = modified_disease_status_data)
  modified_disease_status_juice <- juice(modified_disease_status_prep)

  # cv folds
  folds <- vfold_cv(modified_disease_status_data, strata = disease_status)

  # set up model specifications
  xgb_spec <-
    boost_tree(
      trees = 1000,
      tree_depth = tune(), min_n = tune(), loss_reduction = tune(),   ## first three: model complexity
      sample_size = tune(), mtry = tune(),         ## randomness
      learn_rate = tune(),                         ## step size
    ) %>%
    set_engine("xgboost") %>%
    set_mode("classification")

  # make a workflow that combines the recipe and model
  xgb_wf <- workflow() %>%
    add_recipe(modified_disease_status_recipe) %>%
    add_model(xgb_spec)

  # update parameter set
  xgb_set <- parameters(xgb_wf) %>%
    update(mtry = finalize(mtry(), modified_disease_status_data))

  doParallel::registerDoParallel()
  xgb_res <-  tune_bayes(
    xgb_wf,
    resamples = folds,
    param_info = xgb_set,
    initial = 7,
    # How to measure performance?
    metrics = metric_set(roc_auc),
    control = control_bayes(no_improve = 30, verbose = TRUE)
  )

  # xgb_grid <- grid_latin_hypercube(
  #   tree_depth(),
  #   min_n(),
  #   loss_reduction(),
  #   sample_size = sample_prop(),
  #   finalize(mtry(), modified_disease_status_data),
  #   learn_rate(),
  #   size = 30 #TODO confirm size
  # )

  # xgb_res <- tune_grid(
  #   xgb_wf,
  #   resamples = folds,
  #   grid = xgb_grid,
  #   control = control_grid(save_pred = TRUE, verbose = verbose)
  # )

  #TODO check if xgbdump or xgbserialize is needed prior to saving (c object outside r memory)
  write_rds(xgb_res, here::here(paste0(output_directory, "/boost_mod_", "disease_status", ".rds")))

}
