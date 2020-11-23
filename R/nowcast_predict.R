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

  predicted_disease_status <- newdata$disease_status_lag1
  which_predicted_status_positive <- which(predicted_disease_status == 1)

  # Predict case count
  if(length(which_predicted_status_positive)){
    predicted_cases <- newdata$cases_lag1[which_predicted_status_positive]

    # Return a tibble
    predicted_cases <- tibble(id = 1:nrow(newdata)) %>%
      left_join(tibble(id = which_predicted_status_positive, predicted_cases = predicted_cases),
                by = "id") %>%
      mutate(predicted_cases = replace_na(predicted_cases, 0)) %>%
      pull(predicted_cases)
  }else{
    predicted_cases <- predicted_disease_status
  }
  return(predicted_cases)
}

#' Predict from nowcast xgboost model object
#' @return list containing predicted count and whether disease is expected or not (T/F)
#' @import tidyr dplyr workflows
#' @importFrom here here
#' @importFrom readr read_rds
#' @importFrom assertthat assert_that
#' @importFrom recipes bake
#' @export
#'
repel_predict.nowcast_boost <- function(model_object, newdata) {

  # Load models
  boost_mod_disease_status <- model_object$disease_status_model
  # boost_mod_disease_status_xg <- pull_workflow_fit(boost_mod_disease_status)

  boost_mod_cases <- model_object$cases_model
  # boost_mod_cases_xg <- pull_workflow_fit(boost_mod_cases)

  # Predict disease status
  predicted_disease_status <- predict(boost_mod_disease_status,  new_data = newdata)
  which_predicted_status_positive <- which(predicted_disease_status$.pred_class == 1)

  # Predict case count
  if(length(which_predicted_status_positive)){
    predicted_cases <- predict(boost_mod_cases,  new_data = newdata[which_predicted_status_positive,]) %>% pull(.pred)
    predicted_cases <- 10^predicted_cases
    predicted_cases <- round(predicted_cases)
    assertthat::assert_that(min(predicted_cases) >= 0)

    # Return a tibble
    predicted_cases <- tibble(id = 1:nrow(newdata)) %>%
      left_join(tibble(id = which_predicted_status_positive, predicted_cases = predicted_cases),
                by = "id") %>%
      mutate(predicted_cases = as.numeric(replace_na(predicted_cases, 0))) %>%
      pull(predicted_cases)
  }else{
    predicted_cases <- as.numeric(rep(0, nrow(newdata)))
  }
  return(predicted_cases)
}
