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
#' @importFrom recipes recipe prep juice
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
#' @import dplyr tidyr parsnip dials tune workflows tictoc
#' @importFrom rsample vfold_cv
#' @importFrom parallel detectCores makePSOCKcluster
#' @importFrom doParallel registerDoParallel
#' @importFrom readr write_rds
#' @importFrom assertthat assert_that
#' @importFrom here here
#' @importFrom recipes recipe step_mutate step_rm step_novel step_dummy step_zv prep juice all_nominal all_predictors all_outcomes
#' @export
repel_fit.nowcast_boost <- function(model_object,
                                    model = c("disease_status", "cases"),
                                    augmented_data,
                                    output_directory,
                                    verbose = interactive()) {

  # Status model ------------------------------------------------------------
  if(model == "disease_status"){
    # Model recipe
    disease_status_recipe <-
      recipe(formula = disease_status ~ ., data = augmented_data) %>%
      step_mutate(report_semester_1 = as.numeric(report_semester == 1)) %>%
      step_rm(report_year, report_semester, cases) %>%
      step_novel(all_nominal(), -all_outcomes()) %>%
      step_dummy(all_nominal(), -all_outcomes(), one_hot = TRUE) %>%
      step_zv(all_predictors()) %>%
      step_mutate(disease_status = factor(disease_status), skip = TRUE)

    # Set up model to tune all parameters
    disease_status_spec <-
      boost_tree(trees = tune(), min_n = tune(), tree_depth = tune(), learn_rate = tune(),
                 loss_reduction = tune(), sample_size = tune(),
                 mtry = tune()) %>%
      set_mode("classification" ) %>%
      set_engine("xgboost")

    # Make a tidymodel workflow combining recipe and specs
    disease_status_workflow <-
      workflow() %>%
      add_recipe(disease_status_recipe) %>%
      add_model(disease_status_spec)

    # Modify parameters to be able to tune mtry()
    disease_status_param <-
      disease_status_workflow %>%
      parameters() %>%
      update(mtry = finalize(mtry(), augmented_data))

    # Set up 10 fold cross validation
    disease_status_folds <- vfold_cv(augmented_data, strata = disease_status)

    # Set up parallel
    all_cores <- parallel::detectCores(logical = FALSE)
    cl <- parallel::makePSOCKcluster(all_cores)
    doParallel::registerDoParallel(cl)

    # Tune disease status model
    tic("tuning disease status model")
    disease_status_tune_bayes <-
      tune_bayes(disease_status_workflow,
                 resamples = disease_status_folds,
                 param_info = disease_status_param,
                 control = control_bayes(verbose = TRUE, no_improve = 10, seed = 348),
                 initial =  8)
    toc()
    #^ this takes about 14-15 hrs
    parallel::stopCluster(cl = cl)
    write_rds(disease_status_tune_bayes, here::here(paste0(output_directory, "/boost_tune_disease_status.rds")))
    write_rds(disease_status_recipe, here::here(paste0(output_directory, "/boost_recipe_disease_status.rds")))

    # Read in tuned results and select best parameters
    disease_status_tune_bayes <- read_rds(here::here(paste0(output_directory, "/boost_tune_disease_status.rds")))
    disease_status_tuned_param <- select_by_one_std_err(disease_status_tune_bayes, mtry, trees, min_n, tree_depth, learn_rate, loss_reduction, sample_size)

    # Update workflow with selected parameters
    disease_status_workflow_tuned <- finalize_workflow(disease_status_workflow, disease_status_tuned_param)

    # Set up parallel again
    all_cores <- parallel::detectCores(logical = FALSE)
    cl <- parallel::makePSOCKcluster(all_cores)
    doParallel::registerDoParallel(cl)

    # Fit model with tuned parameters
    tic("Fit final disease status model")
    disease_status_fit <-  parsnip::fit(object = disease_status_workflow_tuned,
                                        data = augmented_data)
    toc()
    # ^ about 1 hr

   write_rds(disease_status_fit, here::here(paste0(output_directory, "/boost_mod_disease_status.rds")))
   parallel::stopCluster(cl = cl)

   }

  # Case model ------------------------------------------------------------
  if(model == "cases"){

    augmented_data_cases <- augmented_data  %>%
      drop_na(cases) %>%
      filter(cases > 0 | cases_lag1 > 0)

    # Model recipe
    cases_recipe <-
      recipe(formula = cases ~ ., data = augmented_data_cases) %>%
      step_mutate(report_semester_1 = as.numeric(report_semester == 1)) %>%
      step_rm(report_year, report_semester, disease_status) %>%
      step_novel(all_nominal(), -all_outcomes()) %>%
      step_dummy(all_nominal(), -all_outcomes(), one_hot = TRUE) %>%
      step_zv(all_predictors())

    # Set up model to tune all parameters
    cases_spec <-
      boost_tree(trees = tune(), min_n = tune(), tree_depth = tune(), learn_rate = tune(),
                 loss_reduction = tune(), sample_size = tune(),
                 mtry = tune()) %>%
      set_mode("regression" ) %>%
      set_engine("xgboost", objective = "count:poisson")


    # Make a tidymodel workflow combining recipe and specs
    cases_workflow <-
      workflow() %>%
      add_recipe(cases_recipe) %>%
      add_model(cases_spec)

    # Modify parameters to be able to tune mtry()
    cases_param <-
      cases_workflow %>%
      parameters() %>%
      update(mtry = finalize(mtry(), augmented_data_cases))

    # Set up 10 fold cross validation
    cases_folds <- vfold_cv(augmented_data_cases)

    # Set up parallel
    all_cores <- parallel::detectCores(logical = FALSE)
    cl <- parallel::makePSOCKcluster(all_cores)
    doParallel::registerDoParallel(cl)

    # Tune cases model
    tic("tuning cases model")
    cases_tune_bayes <-
      tune_bayes(cases_workflow,
                 resamples = cases_folds,
                 param_info = cases_param,
                 control = control_bayes(verbose = TRUE, no_improve = 10, seed = 988),
                 initial =  8)
    toc()
    #^ this takes about 1.5 hrs on prospero
    parallel::stopCluster(cl = cl)
    write_rds(cases_tune_bayes, here::here(paste0(output_directory, "/boost_tune_cases.rds")))
    write_rds(cases_recipe, here::here(paste0(output_directory, "/boost_recipe_cases.rds")))

    # Read in tuned results and select best parameters
    cases_tune_bayes <- read_rds(here::here(paste0(output_directory, "/boost_tune_cases.rds")))
    cases_tuned_param <- select_by_one_std_err(cases_tune_bayes, mtry, trees, min_n, tree_depth, learn_rate, loss_reduction, sample_size)

    # Update workflow with selected parameters
    cases_workflow_tuned <- finalize_workflow(cases_workflow, cases_tuned_param)

    # Set up parallel again
    all_cores <- parallel::detectCores(logical = FALSE)
    cl <- parallel::makePSOCKcluster(all_cores)
    doParallel::registerDoParallel(cl)

    # Fit model with tuned parameters
    tic("Fit final cases model")
    cases_fit <-  parsnip::fit(object = cases_workflow_tuned,
                                        data = augmented_data_cases)
    toc()
    # ^ about 5 min
    write_rds(cases_fit, here::here(paste0(output_directory, "/boost_mod_cases.rds")))
    parallel::stopCluster(cl = cl)
  }
}
