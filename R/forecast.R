#' define generic augment
#' @export
repel_forecast <- function(x, ...){
  UseMethod("repel_forecast")
}

#' forecast nowcast network model object
#' @param model_object network model object
#' @param conn connection to repel db
#' @param newdata data to augment/predict
#' @export
#'
repel_forecast.network_model <- function(model_object, conn, newdata) {
  augmented_data <- repel_augment(model_object, conn, newdata)
  predictions <- repel_predict(model_object, newdata = augmented_data)
  return(list(augmented_data = augmented_data, predicted_cases = predictions))
}

#' forecast nowcast model object
#' @param model_object nowcast model object
#' @param conn connection to repel db
#' @param subset optional dataframe that contains country_iso3c, report_year, report_semester, disease, disease_population, taxa. Used to subset the output (e.g., for validation/training)
#' @export
#'
repel_forecast.nowcast_model <- function(model_object,
                                         conn,
                                         subset = NULL) {
  augmented_data <- repel_augment(model_object, conn, subset)
  predictions <- repel_predict(model_object, newdata = augmented_data)
  return(predictions)
}

