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
#' @import dplyr tidyr recipes
#' @importFrom readr write_rds
#' @importFrom assertthat assert_that
#' @importFrom here here
#' @importFrom xgboost xgboost xgb.save
#' @export
repel_fit.nowcast_boost <- function(model_object,
                                    augmented_data,
                                    output_directory,
                                    verbose = interactive()) {

  # Status model ------------------------------------------------------------
  # using recipes for some convenience functions and for "baking" new data later
  disease_status_recipe <- augmented_data %>%
    recipe(disease_status ~ .) %>%
    step_mutate(report_semester_1 = as.numeric(report_semester == 1)) %>%
    step_rm(report_year, report_semester, cases) %>%
    step_dummy(all_nominal(), -disease_status) %>%
    step_zv(all_predictors())

  # get tabular data
  modified_disease_status_data <- disease_status_recipe %>%
    prep() %>%
    juice() %>%
    drop_na(disease_status)

  # confirm no NAs etc
  assertthat::assert_that(all(map_lgl(modified_disease_status_data, ~all(!is.na(.)))))
  assertthat::assert_that(all(map_lgl(modified_disease_status_data, ~all(!is.infinite(.)))))
  assertthat::assert_that(all(map_lgl(modified_disease_status_data, ~all(!is.nan(.)))))
  assertthat::assert_that(all(map_lgl(modified_disease_status_data, ~all(!is.null(.)))))

  train_disease_status <- list(
    data = modified_disease_status_data %>%
      select(-disease_status) %>%
      as.matrix(),
    label = modified_disease_status_data$disease_status
  )

  boost_mod_disease_status <- xgboost(data = train_disease_status$data,
                                      label = train_disease_status$label,  verbose = 2,
                                      max.depth = 6, eta = 1, nthread = 2, nrounds = 30,
                                      booster="gbtree",
                                      objective = "binary:logistic")

  xgb.save(boost_mod_disease_status, here::here(paste0(output_directory, "/boost_mod_disease_status.model")))
  write_rds(disease_status_recipe, here::here(paste0(output_directory, "/boost_recipe_disease_status.rds")))
  write_rds(train_disease_status, here::here(paste0(output_directory, "/boost_train_disease_status.rds")))

  # Case model ------------------------------------------------------------
  # using recipes for some convenience functions and for "baking" new data later
  cases_recipe <- augmented_data %>%
    recipe(cases ~ .) %>%
    step_mutate(report_semester_1 = as.numeric(report_semester == 1)) %>%
    step_rm(report_year, report_semester, disease_status) %>%
    step_dummy(all_nominal()) %>%
    step_zv(all_predictors())

  # get tabular data
  modified_cases_data <- cases_recipe %>%
    prep() %>%
    juice() %>%
    drop_na(cases) %>%
    filter(cases > 0)

  # confirm no NAs or Inf
  assertthat::assert_that(all(map_lgl(modified_cases_data, ~all(!is.na(.)))))
  assertthat::assert_that(all(map_lgl(modified_cases_data, ~all(!is.infinite(.)))))
  assertthat::assert_that(all(map_lgl(modified_cases_data, ~all(!is.nan(.)))))
  assertthat::assert_that(all(map_lgl(modified_cases_data, ~all(!is.null(.)))))

  train_cases <- list(
    data = modified_cases_data %>%
      select(-cases) %>%
      as.matrix(),
    label = modified_cases_data$cases
  )

  boost_mod_cases <- xgboost(data = train_cases$data,
                             label = train_cases$label,  verbose = 2,
                             max.depth = 6, eta = 1, nthread = 2, nrounds = 20,
                             booster="gbtree",
                             objective="count:poisson")

  # importance <- xgb.importance(feature_names = colnames(train_cases$data), model = boost_mod_cases)
  # head(importance, n = 10)
  xgb.save(boost_mod_cases, here::here(paste0(output_directory, "/boost_mod_cases.model")))
  # write_rds(cases_recipe, here::here(paste0(output_directory, "/boost_recipe_cases.rds")))
  write_rds(train_cases, here::here(paste0(output_directory, "/boost_train_cases.rds")))

}
