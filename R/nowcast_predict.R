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

#' Predict from nowcast BART model object
#' @return list containing predicted count and whether disease is expected or not (T/F)
#' @importFrom here here
#' @importFrom readr read_rds
#' @importFrom assertthat assert_that
#' @export
#'
repel_predict.nowcast_bart <- function(model_object, newdata) {

  # Load models
  bart_mod_disease_status <- read_rds(here::here("models/bart_mod_disease_status.rds"))
  mod_disease_status_obj <- bart_mod_disease_status[[1]]
  mod_disease_status_dat <- bart_mod_disease_status[[2]]

  bart_mod_cases <- read_rds(here::here("models/bart_mod_cases.rds"))
  mod_cases_obj <- bart_mod_cases[[1]]
  mod_cases_dat <- bart_mod_cases[[2]]

  # Pre-process newdata
  required_names <- names(mod_disease_status_dat)[!names(mod_disease_status_dat) %in% "disease_status"]
  assert_that(all(required_names %in% names(newdata)))
  levels(newdata$disease) <- levels(mod_disease_status_dat$disease)
  levels(newdata$taxa) <- levels(mod_disease_status_dat$taxa)
  levels(newdata$country_iso3c) <- levels(mod_disease_status_dat$country_iso3c)
  levels(newdata$disease_population) <- levels(mod_disease_status_dat$disease_population)

  # Predict disease status
  disease_status_pred <- dbarts:::predict.bart(object = mod_disease_status_obj,
                                               newdata = newdata,
                                               type = c("bart"), #The quantity to be returned by generic functions. Options are "ev" - samples from the posterior of the individual level expected value, "bart" - the sum of trees component; same as "ev" for linear models but on the probit scale for binary ones, and "ppd" - samples from the posterior predictive distribution. To synergize with predict.glm, "response" can be used as a synonym for "value" and "link" can be used as a synonym for "bart".
                                               combineChains = T)

  predicted_disease_status_probability <- apply(pnorm(disease_status_pred), 2, mean)
  predicted_disease_status <- round(predicted_disease_status_probability)
  which_predicted_status_positive <- which(predicted_disease_status == 1)

  # Predict case count
  if(length(which_predicted_status_positive)){
    cases_pred <- dbarts:::predict.bart(object = mod_cases_obj,
                                        newdata = newdata[which_predicted_status_positive,], # only predict on positive outcomes
                                        type = c("bart"), #The quantity to be returned by generic functions. Options are "ev" - samples from the posterior of the individual level expected value, "bart" - the sum of trees component; same as "ev" for linear models but on the probit scale for binary ones, and "ppd" - samples from the posterior predictive distribution. To synergize with predict.glm, "response" can be used as a synonym for "value" and "link" can be used as a synonym for "bart".
                                        combineChains = T)

    predicted_cases <- round(apply(cases_pred, 2, mean))
    predicted_cases <- ifelse(predicted_cases < 0, 0, predicted_cases)

    # Return a tibble
    predicted_cases <- tibble(id = 1:nrow(newdata)) %>%
      left_join(tibble(id = which_predicted_status_positive, predicted_cases = predicted_cases),
                by = "id") %>%
      mutate(predicted_cases = replace_na(predicted_cases, 0)) %>%
      pull(predicted_cases)
  }else{
    predicted_cases <- predicted_disease_status
  }

  # out_augmented <- newdata %>%
  #   mutate(predicted_disease_status = predicted_disease_status,
  #          predicted_disease_status_probability = predicted_disease_status_probability) %>%
  #   bind_cols(out_cases)

  return(predicted_cases)
}


#' Predict from nowcast xgboost model object
#' @return list containing predicted count and whether disease is expected or not (T/F)
#' @import tidyr dplyr
#' @importFrom here here
#' @importFrom xgboost xgb.load
#' @importFrom readr read_rds
#' @importFrom assertthat assert_that
#' @importFrom recipes bake
#' @export
#'
repel_predict.nowcast_boost <- function(model_object, newdata) {

  # Load models
  boost_mod_disease_status <- xgb.load(here::here("models/boost_mod_disease_status.model"))
  boost_mod_cases <- xgb.load(here::here("models/boost_mod_cases.model"))

  # Load recipe
  disease_status_recipe <- read_rds(here::here("models/boost_recipe_disease_status.rds"))

  # Pre-process newdata for status model
  newdata_prepped <- bake(object = prep(disease_status_recipe), new_data = newdata) %>%
    select(-one_of("disease_status")) %>%
    as.matrix()

  # Predict disease status
  predicted_disease_status_probability <- predict(boost_mod_disease_status, newdata = newdata_prepped)
  predicted_disease_status <- round(predicted_disease_status_probability)
  which_predicted_status_positive <- which(predicted_disease_status == 1)

  # Predict case count
  if(length(which_predicted_status_positive)){
    predicted_cases <- predict(object = boost_mod_cases,
                               newdata = newdata_prepped[which_predicted_status_positive,]) # only predict on positive outcomes
    predicted_cases <- round(predicted_cases)

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
