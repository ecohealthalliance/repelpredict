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

  ## cases
  # read in model
  bart_mod_cases <- qread(here::here("models/bart_mod_cases.qs"))
  model <- bart_mod_cases[[1]]
  model_data <- bart_mod_cases[[2]]
  plot(model)
  #
  # # predictions from model object
  # preds <- tibble(actual = 10^model$y, predicted = 10^model$yhat.test.mean)
  # preds <- preds %>%
  #   mutate(predicted = round(predicted))  %>%
  #   mutate(diff = abs(actual - predicted))
  # summary(preds$predicted)
  # summary(preds$actual)
  # summary(preds$diff)
  # ggplot(preds, aes(x = predicted, y = actual, color = diff)) +
  #   geom_abline() +
  #   geom_point() +
  #   scale_color_viridis_c() +
  #   scale_x_log10(limits = c(1, max(c(preds$predicted, preds$actual)))) +
  #   scale_y_log10(limits = c(1, max(c(preds$predicted, preds$actual)))) +
  #   theme_minimal()
  #
  # # predicting on new data (not working)
  #
  # newdata <- augmented_data_sub %>%
  #   slice(sample(nrow(.), 100))
  # newdata = augmented_data_sub %>% filter(country_iso3c != "CAN")
  #
  newdata <- modify_augmented_data(newdata, "cases")
  #
  # newdata <- model_data %>%
  #   slice(sample(nrow(.), 100))
  #
  levels(newdata$disease) <- levels(model_data$disease)
  levels(newdata$taxa) <- levels(model_data$taxa)
  levels(newdata$country_iso3c) <- levels(model_data$country_iso3c)
  levels(newdata$disease_population) <- levels(model_data$disease_population)
  #
  pred <- dbarts:::predict.bart(object = model,
                                newdata = newdata,
                                type = c("ev"), #The quantity to be returned by generic functions. Options are "ev" - samples from the posterior of the individual level expected value, "bart" - the sum of trees component; same as "ev" for linear models but on the probit scale for binary ones, and "ppd" - samples from the posterior predictive distribution. To synergize with predict.glm, "response" can be used as a synonym for "value" and "link" can be used as a synonym for "bart".
                                combineChains = T)

  #
  preds <- tibble(actual = 10^newdata$cases, predicted = 10^apply(pred, 2, mean) )
  preds <- preds %>%
    mutate(predicted = round(predicted))  %>%
    mutate(diff = abs(actual - predicted))
  summary(preds$predicted)
  summary(preds$actual)
  summary(preds$diff)
  ggplot(preds, aes(x = predicted, y = actual, color = diff)) +
    geom_abline() +
    geom_point() +
    scale_color_viridis_c() +
    scale_x_log10(limits = c(1, max(c(preds$predicted, preds$actual)))) +
    scale_y_log10(limits = c(1, max(c(preds$predicted, preds$actual)))) +
    theme_minimal()


  # binary
  #apply(pnorm(model$yhat.train), 3, mean) # binary

}
