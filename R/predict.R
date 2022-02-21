# define generic predict
#' @export
repel_predict <- function(x, ...){
  UseMethod("repel_predict")
}


#' Predict from nowcast baseline model object
#' @return list containing predicted count and whether disease is expected or not (T/F)
#' @export
#'
repel_predict.nowcast_baseline <- function(model_object, newdata) {

  newdata %>%
    mutate(disease_status_predicted = replace_na(disease_status_lag1, 0)) %>%
    mutate(cases_predicted = ifelse(disease_status_predicted == 1, cases_lag1, 0))

}

#' Predict from nowcast xgboost model object
#' @return vector containing predicted count
#' @import tidyr dplyr workflows
#' @importFrom here here
#' @importFrom readr read_rds
#' @importFrom assertthat assert_that
#' @importFrom recipes bake
#' @export
#'
repel_predict.nowcast_boost <- function(model_object, newdata, use_cache = TRUE) {

  # Load models
  boost_mod_disease_status <- model_object$disease_status_model
  # boost_mod_disease_status_xg <- pull_workflow_fit(boost_mod_disease_status)

  boost_mod_cases <- model_object$cases_model
  # boost_mod_cases_xg <- pull_workflow_fit(boost_mod_cases)

  newdata %>%
    mutate(disease_status_predicted = as.numeric(as.character(predict(boost_mod_disease_status,  new_data = .)$.pred_class))) %>%
    mutate(cases_predicted  = predict(boost_mod_cases,  new_data = .)$.pred) %>%
    mutate(cases_predicted = round(10^cases_predicted)) %>%
    mutate(cases_predicted = ifelse(disease_status_predicted == 0, 0, cases_predicted))

}


#' Predict from network lme model object
#' @return vector containing predicted probability of an outbreak
#' @import tidyr dplyr lme4 stringr
#' @export
#'
repel_predict.network_lme <- function(model_object, newdata) {

  # load model
  lme_mod <- model_object$network_model

  # load scaling values
  network_scaling_values <- model_object$network_scaling_values

  # get predictor_vars
  predictor_vars <- network_scaling_values$key

  # transform with scaling values
  newdata2 <- network_recipe(augmented_data = newdata, predictor_vars = predictor_vars, scaling_values = network_scaling_values)
  newdata2 <- newdata2 %>%
    mutate(complete_case = complete.cases(newdata2)) %>%
    mutate(row_number = row_number())

  out <- newdata2 %>%
    filter(complete_case) %>%
    mutate(prediction = predict(lme_mod, ., type = "response")) %>%
    bind_rows(newdata2 %>% filter(!complete_case)) %>%
    arrange(row_number)

  if(any(!newdata2$complete_case)) warning("NAs returned for augmented data containing NAs")

  return(out$prediction)

}

#' Predict from network brms model object
#' @return vector containing predicted probability of an outbreak
#' @import tidyr dplyr brms
#' @importFrom purrr imap_dfr
#' @export
#'
repel_predict.network_brms <- function(model_object, newdata) {

  # load model
  brm_mod <- model_object$network_model

  # load scaling values
  network_scaling_values <- model_object$network_scaling_values

  # get predictor_vars
  predictor_vars <- network_scaling_values$key

  # transform with scaling values
  newdata2 <- network_recipe(augmented_data = newdata, predictor_vars = predictor_vars, scaling_values = network_scaling_values)
  newdata2 <- newdata2 %>%
    mutate(complete_case = complete.cases(newdata2))

  out <- split(newdata2, (as.numeric(rownames(newdata2))-1) %/% 10000) %>%
    purrr::imap_dfr(function(x, i){
      print(i)
      fitted(brm_mod, x, type = "response", allow_new_levels = TRUE) %>%
        as_tibble()
    })

  return(out)
}
