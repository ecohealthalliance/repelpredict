# define generic fit
#' @export
repel_fit <- function(x, ...){
  UseMethod("repel_fit")
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
#' @importFrom aws.s3 s3saveRDS
#' @importFrom recipes recipe step_mutate step_rm step_novel step_dummy step_zv prep juice all_nominal all_predictors all_outcomes step_mutate_at step_log
#' @export
repel_fit.nowcast_boost <- function(model_object,
                                    model = c("disease_status", "cases"),
                                    augmented_data,
                                    output_directory,
                                    verbose = interactive()) {

  augmented_data <- recode_disease_rare(augmented_data) # does not fit into recipe step

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

    write_rds(disease_status_recipe, here::here(paste0(output_directory, "/boost_recipe_disease_status.rds")))

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

    # disease_status_recipe_prepped <- prep(disease_status_recipe)
    # disease_status_recipe_juiced <- juice(disease_status_recipe_prepped)
    # any(map_lgl(disease_status_recipe_juiced, ~any(is.na(.))))

    # Set up parallel
    all_cores <- parallel::detectCores(logical = FALSE)
    cl <- parallel::makePSOCKcluster(all_cores)
    doParallel::registerDoParallel(cl)

    # Tune disease status model - first using a grid
    tic("pre-tuning disease status model (grid)")
    disease_status_tune_grid <- tune_grid(disease_status_workflow,
                                          resamples = disease_status_folds,
                                          control = control_grid(verbose = TRUE))
    toc()
    # ^ this takes about 24 hrs on aegypti
    write_rds(disease_status_tune_grid, here::here(paste0(output_directory, "/boost_tune_disease_status_grid.rds")))

    parallel::stopCluster(cl = cl)

    ### Not running bayes tune
    # Tune disease status model - now with bayes, using tune grid as prior
    # disease_status_tune_grid <- read_rds(here::here(paste0(output_directory, "/boost_tune_disease_status_grid.rds")))
    # tic("Tuning disease status model (bayes)")
    # disease_status_tune_bayes <-
    #   tune_bayes(disease_status_workflow,
    #              resamples = disease_status_folds,
    #              param_info = disease_status_param,
    #              iter = 14,
    #              control = control_bayes(verbose = TRUE, no_improve = 10, seed = 400),
    #              initial =  disease_status_tune_grid)
    # # ^ this takes about 6 hrs
    # toc()
    # write_rds(disease_status_tune_bayes, here::here(paste0(output_directory, "/boost_tune_disease_status_bayes.rds")))
    #
    # # Read in tuned results and select best parameters
    # disease_status_tune_bayes <- read_rds(here::here(paste0(output_directory, "/boost_tune_disease_status_bayes.rds")))
    # disease_status_tuned_param <- select_by_one_std_err(disease_status_tune_bayes, mtry, trees, min_n, tree_depth, learn_rate, loss_reduction, sample_size)

    # # Read in tuned results and select best parameters
    disease_status_tune_grid <- read_rds(here::here(paste0(output_directory, "/boost_tune_disease_status_grid.rds")))
    disease_status_tuned_param <- select_by_one_std_err(disease_status_tune_grid, mtry, trees, min_n, tree_depth, learn_rate, loss_reduction, sample_size)

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
    aws.s3::s3saveRDS(disease_status_fit, bucket = "repeldb/models", object = "boost_mod_disease_status.rds")
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
      step_zv(all_predictors()) %>%
      step_mutate_at(all_outcomes(), starts_with("cases_lag"), fn = ~ifelse(. == 0, 0.1, .), skip = TRUE) %>%
      step_log(all_outcomes(), starts_with("cases_lag"), base = 10, skip = TRUE)

    write_rds(cases_recipe, here::here(paste0(output_directory, "/boost_recipe_cases.rds")))

    # Set up model to tune all parameters
    cases_spec <-
      boost_tree(trees = tune(), min_n = tune(), tree_depth = tune(), learn_rate = tune(),
                 loss_reduction = tune(), sample_size = tune(),
                 mtry = tune()) %>%
      set_mode("regression" ) %>% #  [default=reg:squarederror]
      set_engine("xgboost")

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

    # Tune cases model - first using a grid
    tic("pre-tuning cases model (grid)")
    cases_tune_grid <- tune_grid(cases_workflow,
                                 resamples = cases_folds,
                                 control = control_grid(verbose = TRUE))
    toc()
    # ^ this takes about 1.5 hrs on aegypti
    write_rds(cases_tune_grid, here::here(paste0(output_directory, "/boost_tune_cases_grid.rds")))

    # Tune cases model - now with bayes, using tune grid as prior
    cases_tune_grid <- read_rds(here::here(paste0(output_directory, "/boost_tune_cases_grid.rds")))

    tic("Tuning cases model (bayes)")
    cases_tune_bayes <-
      tune_bayes(cases_workflow,
                 resamples = cases_folds,
                 param_info = cases_param,
                 iter = 14,
                 control = control_bayes(verbose = TRUE, no_improve = 10, seed = 988),
                 initial =  cases_tune_grid)
    # ^ this takes about 1 hr
    toc()
    write_rds(cases_tune_bayes, here::here(paste0(output_directory, "/boost_tune_cases_bayes.rds")))
    parallel::stopCluster(cl = cl)

    # Read in tuned results and select best parameters
    cases_tune_bayes <- read_rds(here::here(paste0(output_directory, "/boost_tune_cases_bayes.rds")))
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
    aws.s3::s3saveRDS(cases_fit, bucket = "repeldb/models", object = "boost_mod_cases.rds")
    parallel::stopCluster(cl = cl)
  }
}
