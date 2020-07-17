# define generic predict
#' @export
repel_predict <- function(x, ...){
  UseMethod("repel_predict")
}

bart_predict <- function(model, newdata) {
  apply(predict(model, newdata), 2, mean)
}


#' Predict from nowcast baseline model object
#' @return list containing predicted count and whether disease is expected or not (T/F)
#' @export
#'
repel_predict.nowcast_baseline <- function(model_object, augmented_data) {
  # this assumes NAs are 0s
  list(
    predicted_cases = replace_na(augmented_data$cases_lag1, 0),
    predicted_disease_status_probability = NA,
    predicted_disease_status = replace_na(augmented_data$disease_status_lag1, 0),
    na_predicted_cases = which(is.na(augmented_data$cases_lag1)),
    na_predicted_disease_status = which(is.na(augmented_data$disease_status_lag1))
  )
}

#' Predict from nowcast BART model object
#' @return list containing predicted count and whether disease is expected or not (T/F)
#' @importFrom here here
#' @export
#'
repel_predict.nowcast_bart <- function(model_object, augmented_data) {


  bart_mod_disease_status <- read_rds(here::here("models/bart_mod_disease_status.rds"))
  bart_mod_cases <- read_rds(here::here("models/bart_mod_cases.rds"))
  modified_data_disease_status <- modify_augmented_data(augmented_data, "disease_status")
  modified_data_cases <- modify_augmented_data(augmented_data, "cases")

  list(
    predicted_cases = bart_predict(model = bart_mod_cases, newdata = modified_data_cases),
    predicted_disease_status_probability = bart_predict(model = bart_mod_disease_status, newdata = modified_data_disease_status),
    predicted_disease_status = round(predicted_disease_status_probability),
    na_predicted_cases = which(is.na(predicted_cases)),
    na_predicted_disease_status = which(is.na(predicted_disease_status))
  )
  #predicted_means = apply(bart_mod_cases$yhat.train, 3, mean)
}
