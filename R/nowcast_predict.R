# define generic predict
#' @export
repel_predict <- function(x, ...){
  UseMethod("repel_predict")
}


#' Predict from nowcast baseline model object
#' @return list containing predicted count and whether disease is expected or not (T/F)
#' @export
#'
#TODO rename augmented_data -> newdata
repel_predict.nowcast_baseline <- function(model_object, augmented_data) {
  # this assumes NAs are 0s
  list(
    predicted_cases = augmented_data$cases_lag1,
    predicted_disease_status_probability = NA,
    predicted_disease_status = augmented_data$disease_status_lag1,
    na_predicted_cases = which(augmented_data$cases_lag1_missing),
    na_predicted_disease_status = which(augmented_data$disease_status_lag1_missing)
  )
}

#' Predict from nowcast BART model object
#' @return list containing predicted count and whether disease is expected or not (T/F)
#' @importFrom here here
#' @importFrom qs qread
#' @export
#'
repel_predict.nowcast_bart <- function(model_object, newdata) {

  # cases
  bart_mod_cases <- qread(here::here("models/bart_mod_cases.qs"))
  model <- bart_mod_cases[[1]]
  model_data <- bart_mod_cases[[2]]

  newdata <- modify_augmented_data(newdata, "cases")

  preds <- tibble(actual = model$y, predicted = model$yhat.test.mean)
  preds <- preds %>%
    mutate(predicted_adjust = ifelse(predicted < 0, 0, round(predicted)))  %>%
    mutate(diff = abs(actual - predicted_adjust))
  summary(preds$predicted)
  summary(preds$actual)
  summary(preds$predicted_adjust)
  summary(preds$diff)

  ## predicting on new data
  levels(newdata$disease) <- levels(model_data$disease)
  levels(newdata$taxa) <- levels(model_data$taxa)
  levels(newdata$country_iso3c) <- levels(model_data$country_iso3c)
  levels(newdata$disease_population) <- levels(model_data$disease_population)

  pred <- predict(model, newdata)

  dim(pred)
  dim(newdata)
  apply(pred, 2, mean) # all same number???

  # binary
  #apply(pnorm(model$yhat.train), 3, mean) # binary

}
