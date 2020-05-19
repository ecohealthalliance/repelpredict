# define generic predict
repel_predict <- function(x){
  UseMethod("repel_predict")
}


#' Predict from nowcast baseline model object
#' @return list containing predicted count and whether disease is expected or not (T/F)
#'
repel_predict.nowcast_baseline <- function(model_object, augmented_data) {
  list(
    predicted_count = augmented_data$cases_lag1,
    predicted_presence = !is.na(augmented_data$cases_lag1) & augmented_data$cases_lag1>0,
    na_values = which(is.na(augmented_data$cases_lag1)),
    se = NULL
  )
}
