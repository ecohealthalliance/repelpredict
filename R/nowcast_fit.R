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

  # modify data prior to running
  modified_disease_status_data <- augmented_data %>%
    drop_na(disease_status) %>%
    mutate(disease_status = factor(disease_status)) %>%
    select(-cases, -report_year)

  # confirm no NAs or Inf
  assertthat::assert_that(all(map_lgl(modified_disease_status_data, ~all(!is.na(.)))))
  assertthat::assert_that(all(map_lgl(modified_disease_status_data, ~all(!is.infinite(.)))))
  assertthat::assert_that(all(map_lgl(modified_disease_status_data, ~all(!is.nan(.)))))
  assertthat::assert_that(all(map_lgl(modified_disease_status_data, ~all(!is.null(.)))))

  xgb_spec <-
    boost_tree(
      trees = 1000,
      tree_depth = tune(), min_n = tune(), loss_reduction = tune(),   ## first three: model complexity
      sample_size = tune(), mtry = tune(),         ## randomness
      learn_rate = tune(),                         ## step size
    ) %>%
    set_engine("xgboost") %>%
    set_mode("classification")

  xgb_grid <- grid_latin_hypercube(
    tree_depth(),
    min_n(),
    loss_reduction(),
    sample_size = sample_prop(),
    finalize(mtry(), modified_disease_status_data),
    learn_rate(),
    size = 30 #TODO confirm size
  )

  xgb_wf <- workflow() %>%
    add_formula(disease_status ~ .) %>%
    add_model(xgb_spec)


  vb_folds <- vfold_cv(modified_disease_status_data, strata = disease_status)

  doParallel::registerDoParallel()

  tic()
  set.seed(234)
  xgb_res <- tune_grid(
    xgb_wf,
    resamples = vb_folds,
    grid = xgb_grid,
    control = control_grid(save_pred = TRUE, verbose = verbose)
  )
  toc()
  #TODO check if xgbdump or xgbserialize is needed prior to saving (c object outside r memory)
  write_rds(xgb_res, here::here(paste0(output_directory, "/boost_mod_", "disease_status", ".rds")))

}
