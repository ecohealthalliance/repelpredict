#' define generic augment
#' @export
repel_forecast <- function(x, ...){
  UseMethod("repel_forecast")
}


#' forecast nowcast baseline model object
#' @param model_object nowcast model object
#' @param conn connection to repel db
#' @import repeldata dplyr tidyr
#' @importFrom assertthat has_name assert_that
#' @export
#'
repel_forecast.nowcast_baseline <- function(model_object, conn, newdata) {
  augmented_data <- repel_augment(model_object, conn, newdata)
  predictions <- repel_predict(model_object, newdata = augmented_data)
  return(list(augmented_data = augmented_data, predicted_cases = predictions))
}

#' forecast nowcast boost model object
#' @import repeldata dplyr tidyr
#' @importFrom assertthat has_name assert_that
#' @param model_object nowcast model object
#' @param conn connection to repel db
#' @param newdata - dataframe or tibble with fields: country_iso3c, report_year, report_semester, disease, disease_population, taxa
#' @param use_cache whether to use cached prediced data (TRUE/FALSE)
#' @export
#'
repel_forecast.nowcast_boost <- function(model_object, conn, newdata, use_cache = TRUE) {

  # check only inputting grouping vars
  newdata <- select(newdata, all_of(grouping_vars))

  # check diseases match training data (can be with or without spaces)
  diseases_recode <- read_csv(system.file("lookup", "nowcast_diseases_recode.csv",  package = "repelpredict"), col_types = cols(
    disease = col_character(),
    disease_recode = col_character()
  ))
  disease_recode_lookup <- diseases_recode$disease_recode
  names(disease_recode_lookup) <- diseases_recode$disease

  newdata <- newdata %>%
    mutate(disease = case_when(disease %in% diseases_recode$disease ~ disease_recode_lookup[disease],
                               TRUE ~ disease))

  if(any(!unique(newdata$disease) %in% diseases_recode$disease_recode)){
    stop(paste(
      unique(newdata$disease)[which(!unique(newdata$disease) %in% diseases_recode$disease_recode)],
      "is not a recognized disease"
    ))
  }

  # recode diseases as rare
  newdata <- recode_disease_rare(newdata) # does not fit into recipe step

  # use cache if available
  if(use_cache){

    cached_data <- DBI::dbReadTable(conn, "nowcast_boost_augment_predict")

    # check if any data is not covered in the cache, if it's all in cache, return cache, otherwise run predictions
    cache_check <- anti_join(newdata, cached_data,  by = c("report_year", "report_semester", "disease", "country_iso3c", "disease_population", "taxa"))

    if(nrow(cache_check)==0){
      augmented_data <- left_join(newdata, cached_data)
      predictions <- augmented_data$predicted_cases
      augmented_data <- augmented_data %>% select(-predicted_cases)
      assertthat::assert_that(!any(is.na(predictions)))
      return(list(augmented_data = augmented_data, predicted_cases = predictions))
    }
  }
  augmented_data <- repel_augment(model_object, conn, newdata)
  predictions <- repel_predict(model_object, newdata = augmented_data)
  return(list(augmented_data = augmented_data, predicted_cases = predictions))

}



#' forecast network lme model object
#' @import repeldata dplyr tidyr
#' @importFrom assertthat has_name assert_that
#' @param model_object nowcast model object
#' @param conn connection to repel db
#' @param newdata - dataframe or tibble with fields: country_iso3c, report_year, report_semester, disease, disease_population, taxa
#' @param use_cache whether to use cached prediced data (TRUE/FALSE)
#' @export
#'
repel_forecast.network_lme <- function(model_object, conn, newdata, use_cache = TRUE) {

  # check diseases match training data (can be with or without spaces)
  diseases_recode <- read_csv(system.file("lookup", "network_diseases_recode.csv",  package = "repelpredict"), col_types = cols(
    disease = col_character(),
    disease_recode = col_character()
  ))
  disease_recode_lookup <- diseases_recode$disease_recode
  names(disease_recode_lookup) <- diseases_recode$disease

  newdata <- newdata %>%
    mutate(disease = case_when(disease %in% diseases_recode$disease ~ disease_recode_lookup[disease],
                               TRUE ~ disease))

  if(any(!unique(newdata$disease) %in% diseases_recode$disease_recode)){
    stop(paste(
      unique(newdata$disease)[which(!unique(newdata$disease) %in% diseases_recode$disease_recode)],
      "is not a recognized disease"
    ))
  }

  # use cache if available
  if(use_cache){

    cached_data <- DBI::dbReadTable(conn, "network_lme_augment_predict") %>%
      mutate(month = as.Date(month))

    # check if any data is not covered in the cache, if it's all in cache, return cache, otherwise run predictions
    cache_check <- anti_join(newdata, cached_data,  by = c("disease", "country_iso3c", "month"))

    if(nrow(cache_check)==0){
      augmented_data <- left_join(newdata, cached_data)
      predictions <- as.numeric(augmented_data$predicted_outbreak_probability)
      augmented_data <- augmented_data %>% select(-predicted_outbreak_probability)
      assertthat::assert_that(!any(is.na(predictions)))
      return(list(augmented_data = augmented_data, predicted_probablity = predictions))
    }
  }

  augmented_data <- repel_augment(model_object, conn, newdata)
  predictions <- repel_predict(model_object, newdata = augmented_data)
  return(list(augmented_data = augmented_data, predicted_probablity = predictions))

}
