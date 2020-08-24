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
  list(
    # case model
    predicted_cases = newdata$cases_lag1, # assumes 0 for NA lag
    na_predicted_cases = which(newdata$cases_lag1_missing), # which values were NA (and replaced as 0)
    # disease status model
    predicted_disease_status = newdata$disease_status_lag1, # assumes 0 for NA lag
    predicted_disease_status_probability = NA, # this is for BART
    na_predicted_disease_status = which(newdata$disease_status_lag1_missing) # which values were NA (and replaced as 0)
  )
}

#' Predict from nowcast BART model object
#' @return list containing predicted count and whether disease is expected or not (T/F)
#' @importFrom here here
#' @importFrom qs qread
#' @export
#'
repel_predict.nowcast_bart <- function(model_object, newdata) {

  # models
  bart_mod_cases <- qread(here::here("models/bart_mod_cases.qs"))
  mod_cases_obj <- bart_mod_cases[[1]]
  mod_cases_dat <- bart_mod_cases[[2]]

  bart_mod_disease_status <- qread(here::here("models/bart_mod_disease_status.qs"))
  mod_disease_status_obj <- bart_mod_disease_status[[1]]
  mod_disease_status_dat <- bart_mod_disease_status[[2]]

  levels(newdata$disease) <- levels(mod_cases_dat$disease)
  levels(newdata$taxa) <- levels(mod_cases_dat$taxa)
  levels(newdata$country_iso3c) <- levels(mod_cases_dat$country_iso3c)
  levels(newdata$disease_population) <- levels(mod_cases_dat$disease_population)

  cases_pred <- dbarts:::predict.bart(object = mod_cases_obj,
                                          newdata = newdata,
                                          type = c("bart"), #The quantity to be returned by generic functions. Options are "ev" - samples from the posterior of the individual level expected value, "bart" - the sum of trees component; same as "ev" for linear models but on the probit scale for binary ones, and "ppd" - samples from the posterior predictive distribution. To synergize with predict.glm, "response" can be used as a synonym for "value" and "link" can be used as a synonym for "bart".
                                          combineChains = T)

  disease_status_pred <- dbarts:::predict.bart(object = mod_disease_status_obj,
                                                   newdata = newdata,
                                                   type = c("bart"), #The quantity to be returned by generic functions. Options are "ev" - samples from the posterior of the individual level expected value, "bart" - the sum of trees component; same as "ev" for linear models but on the probit scale for binary ones, and "ppd" - samples from the posterior predictive distribution. To synergize with predict.glm, "response" can be used as a synonym for "value" and "link" can be used as a synonym for "bart".
                                                   combineChains = T)
  list(
    predicted_cases = round(10^apply(cases_pred, 2, mean)-1),
    predicted_disease_status_probability = apply(pnorm(disease_status_pred), 2, mean),
    predicted_disease_status = round(apply(pnorm(disease_status_pred), 2, mean)),
    na_predicted_cases = NA, # always returns a prediction
    na_predicted_disease_status = NA # always returns a prediction
  )
}
