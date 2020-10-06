library(usemodels)
library(tidymodels)
library(tidyverse)
library(xgboost)
library(tictoc)
set.seed(2873)

augmented_data <- read_rds(here::here("tmp/augmented_data.rds"))
#use_xgboost(disease_status ~ ., data = augmented_data)

# model recipe ------------------------------------------------------------
xgboost_recipe <-
  recipe(formula = disease_status ~ ., data = augmented_data) %>%
  step_mutate(report_semester_1 = as.numeric(report_semester == 1)) %>%
  step_rm(report_year, report_semester, cases) %>%
  step_novel(all_nominal(), -all_outcomes()) %>%
  step_dummy(all_nominal(), -all_outcomes(), one_hot = TRUE) %>%
  step_zv(all_predictors())

# first run default model and compare to xgboost alone (default params) --------------------
### xgboost
modified_disease_status_data <- xgboost_recipe %>%
  prep() %>%
  juice() %>%
  drop_na(disease_status)

train_disease_status <- list(
  data = modified_disease_status_data %>%
    select(-disease_status) %>%
    as.matrix(),
  label = modified_disease_status_data$disease_status
)

tic()
boost_mod_disease_status <- xgboost(data = train_disease_status$data,
                                    label = train_disease_status$label,  verbose = 2,
                                    nthread = 20,
                                    booster="gbtree",
                                    objective = "binary:logistic",
                                    nrounds = 15, # trees
                                    max_depth = 6, # tree_depth
                                    eta = 0.3, # learn_rate
                                    min_child_weight = 1, # min_n
                                    gamma = 0, #loss_reduction
                                    subsample = 1 #sample_size
)
toc()
# 87.862 sec elapsed nthread not specified
# 10.711 sec elapsed with 20 threads cool

### tidymod
xgboost_recipe2 <- xgboost_recipe %>%
  step_mutate(disease_status = factor(disease_status))

xgboost_spec <-
  boost_tree(trees = 15, min_n = 1, tree_depth = 6, learn_rate = 0.3,
             loss_reduction = 0, sample_size = 1) %>%
  set_mode("classification" ) %>%
  set_engine("xgboost")

xgboost_workflow <-
  workflow() %>%
  add_recipe(xgboost_recipe2) %>%
  add_model(xgboost_spec)

tic()
boost_mod_disease_status_tidy <- parsnip::fit(object = xgboost_workflow, data = augmented_data)
toc()
# 95.148 sec elapsed (not parallel)

# test out parallel
# all_cores <- parallel::detectCores(logical = FALSE)
# cl <- makePSOCKcluster(all_cores)
# registerDoParallel(cl)

# tic()
# boost_mod_disease_status_tidy <- parsnip::fit(object = xgboost_workflow, data = augmented_data)
# toc()
# 94.097 sec elapsed (hmmm)

# try changing some of the defaults (pre-tuning) --------------------------

grid <- tibble(nrounds = 15, # trees
               max_depth = 6, # tree_depth
               eta = 0.3, # learn_rate
               min_child_weight = 1, # min_n
               gamma = 0, #loss_reduction
               subsample = 1, #sample_size
               iter = "base")

grid_out <- bind_rows(grid,
                      mutate(grid, max_depth = 12, iter = "dbl_max_depth"))   %>%
  bind_rows(mutate(grid, eta = 0.6, iter = "dbl_eta")) %>%
  bind_rows(mutate(grid, eta = 0.01, iter = "small_eta")) %>%
  bind_rows(mutate(grid, eta = 0.99, iter = "large_eta")) %>%

  bind_rows(mutate(grid, min_child_weight = 2, iter = "dbl_min_child_weight")) %>%
  bind_rows(mutate(grid, min_child_weight = 10, iter = "large_min_child_weight")) %>%

  bind_rows(mutate(grid, subsample = 0.5, iter = "hlf_subsample")) %>%
  bind_rows(mutate(grid, subsample = 0.75, iter = "0.75_subsample"))

grid_out$error <- NA_integer_

for(i in 1:nrow(grid_out)){
  mod <- xgboost(data = train_disease_status$data, label = train_disease_status$label,  verbose = 2,
                 booster="gbtree", objective = "binary:logistic",  nthread = 20,
                 nrounds = grid_out$nrounds[i], # trees
                 max_depth = grid_out$max_depth[i], # tree_depth
                 eta = grid_out$eta[i], # learn_rate
                 min_child_weight = grid_out$min_child_weight[i], # min_n
                 gamma = grid_out$gamma[i], #loss_reduction
                 subsample = grid_out$subsample[i] #sample_size
  )
  grid_out$error[i] <- mod$evaluation_log  %>% tail(1) %>% pull(train_error)
}

# max_depth - model is sensitive
# eta - model not super sensitive
# min_child_weight - model not super sensitive
# subsample - model not super sensitive

# so, try tuning and fixing eta, min_child_weight, and subsample

# tuning with tidymod (grid) ----------------------------------------

# semituned specs
xgboost_spec_semi_tuned <-
  boost_tree(trees = tune(), min_n = 1, tree_depth = tune(), learn_rate = 0.3,
             loss_reduction = tune(), sample_size = 1) %>%
  set_mode("classification" ) %>%
  set_engine("xgboost")

# cv folds
folds <- vfold_cv(augmented_data, strata = disease_status)

# semituned workflow
xgboost_workflow_semi_tuned <-
  workflow() %>%
  add_recipe(xgboost_recipe2) %>%
  add_model(xgboost_spec_semi_tuned)

all_cores <- parallel::detectCores(logical = FALSE)
print(all_cores)
cl <- parallel::makePSOCKcluster(all_cores)
cl
doParallel::registerDoParallel(cl)

tic()
xgboost_tune <-
  tune_grid(xgboost_workflow_semi_tuned,
            resamples = folds,
            control = control_grid(verbose = TRUE))
toc()

parallel::stopCluster(cl = cl)
write_rds(xgboost_tune, here::here("tmp/xgboost_tune.rds"))
# ^  ~17 hrs to tune

# Check out grid tuning results ------------------------------------------------

# these were the settings:
# trees = tune(), min_n = 1, tree_depth = tune(), learn_rate = 0.3,
# loss_reduction = tune(), sample_size = 1
xgboost_tune <- read_rds(here::here("tmp/xgboost_tune.rds"))

# ROC stands for Receiver Operating Curve and It’s a plot between FPR i.e False Positive Rate or 1-specificity on x-axis and TPR on True Positive Rate on the y-axis.
autoplot(xgboost_tune, metric = "roc_auc")

show_best(xgboost_tune, "roc_auc")
xgboost_tuned_param <- select_by_one_std_err(xgboost_tune, trees, tree_depth, loss_reduction)

xgboost_workflow_tuned <- finalize_workflow(xgboost_workflow_semi_tuned, xgboost_tuned_param)

# tic()
# xgboost_fit_tuned <-  parsnip::fit(object = xgboost_workflow_tuned,
#                                    data = augmented_data)
# toc()
tic()
boost_mod_disease_status_tuned <- xgboost(data = train_disease_status$data,
                                    label = train_disease_status$label,  verbose = 2,
                                    nthread = 40,
                                    booster="gbtree",
                                    objective = "binary:logistic",
                                    nrounds = xgboost_tuned_param$trees, # trees
                                    max_depth = xgboost_tuned_param$tree_depth, # tree_depth
                                    eta = 0.3, # learn_rate
                                    min_child_weight = 1, # min_n
                                    gamma = xgboost_tuned_param$loss_reduction, #loss_reduction
                                    subsample = 1 #sample_size
)
toc()
xgb.save(boost_mod_disease_status_tuned, here::here(paste0("models", "/boost_mod_cases_tuned.model")))


# Now try tune_bayes ------------------------------------------------------
# also let's just tune everything...
xgboost_spec<-
  boost_tree(trees = tune(), min_n = tune(), tree_depth = tune(), learn_rate = tune(),
             loss_reduction = tune(), sample_size = tune(),
              mtry = tune()) %>%
  set_mode("classification" ) %>%
  set_engine("xgboost")

# cv folds
folds <- vfold_cv(augmented_data, strata = disease_status)

# semituned workflow
xgboost_workflow <-
  workflow() %>%
  add_recipe(xgboost_recipe2) %>%
  add_model(xgboost_spec)

xgboost_param <-
  xgboost_workflow %>%
  parameters() %>%
  update(mtry = finalize(mtry(), augmented_data))

all_cores <- parallel::detectCores(logical = FALSE)
print(all_cores)
cl <- parallel::makePSOCKcluster(all_cores)
doParallel::registerDoParallel(cl)

tic()
xgboost_tune_bayes <-
  tune_bayes(xgboost_workflow,
            resamples = folds,
            param_info = xgboost_param,
            control = control_bayes(verbose = TRUE, no_improve = 10, seed = 348),
            initial =  8)
toc()
parallel::stopCluster(cl = cl)
write_rds(xgboost_tune, here::here("tmp/xgboost_tune_bayes.rds"))
