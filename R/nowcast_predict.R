# define generic predict
repel_predict <- function(x, ...){
  UseMethod("repel_predict")
}


#' Predict from nowcast baseline model object
#' @return list containing predicted count and whether disease is expected or not (T/F)
#'
repel_predict.nowcast_baseline <- function(model_object, augmented_data) {
  # this assumes NAs are 0s
  list(
    predicted_cases = replace_na(augmented_data$cases_lag1, 0),
    predicted_disease_status = replace_na(augmented_data$disease_status_lag1, 0),
    na_cases = which(is.na(augmented_data$cases_lag1)),
    na_disease_status = which(is.na(augmented_data$disease_status_lag1))
  )
}
