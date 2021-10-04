#' define generic augment
#' @export
repel_forecast <- function(x, ...){
  UseMethod("repel_forecast")
}

#' forecast nowcast baseline model object
#' @param model_object nowcast model object
#' @param conn connection to repel db
#' @param newdata data to augment/predict
#' @export
#'
repel_forecast.repel_model <- function(model_object, conn, newdata) {
  augmented_data <- repel_augment(model_object, conn, newdata)
  predictions <- repel_predict(model_object, newdata = augmented_data)
  return(list(augmented_data = augmented_data, predicted_cases = predictions))
}
