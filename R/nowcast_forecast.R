#' define generic augment
#' @export
repel_forecast <- function(x, ...){
  UseMethod("repel_forecast")
}


#' forecast nowcast baseline model object
#' @import repeldata dplyr tidyr
#' @importFrom assertthat has_name assert_that
#' @export
#'
repel_forecast.nowcast_model <- function(model_object, conn, newdata) {
  augmented_data <- repel_augment(model_object, conn, newdata)
  predictions <- repel_predict(model_object, augmented_data)
  return(list(augmented_data = augmented_data, predicted_data = predictions))
}
