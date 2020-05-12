#' @import repeldata dplyr tidyr
#' @importFrom DBI dbDisconnect
#' @importFrom digest digest2int
#' @noRd
repel_cases_split <- function(conn){
  tbl(conn, "annual_reports_animal_hosts") %>%
    filter(!is.na(cases)) %>%
    filter(report_semester != "0") %>%
    mutate_at(.vars = c("report_year", "report_semester"), ~as.numeric(.)) %>%
    select(report, country, country_iso3c, report_year, report_semester, disease, disease_population, serotype, disease_status, taxa, cases) %>%
    collect() %>%
    mutate(validation_set = digest::digest2int(paste0(report, disease, disease_population, serotype, disease_status, taxa)) %% 5 == 1)

  #TODO all diseases and taxa need to be in training?
}


#' Get cases training set (~80%)
#' @import repeldata dplyr tidyr
#' @return a tibble
#' @export
repel_cases_train <- function(conn){
  repel_cases_split(conn) %>%
    filter(!validation_set) %>%
    select(-validation_set)
}


#' Get cases validation set (~20%)
#' @import repeldata dplyr tidyr
#' @return a tibble
#' @export
repel_cases_validate <- function(conn){
  repel_cases_split(conn) %>%
    filter(validation_set) %>%
    select(-validation_set)
}

#' Augment data with predictions
#' @import repeldata dplyr tidyr
#' @importFrom assertthat assert_that has_name
#' @return a tibble
#' @export
nowcast_baseline_augment <- function(conn, newdata, n_semesters = NULL){

  assertthat::has_name(newdata, c("country_iso3c", "report_year", "report_semester", "disease", "taxa"))

  model_lookup <- repel_cases_split(conn) %>%
    mutate(report_period = as.numeric(paste0(report_year, report_semester))) %>% # temp for filtering
    group_by(country_iso3c, disease, taxa) %>%
    mutate(lag_cases_1 = lag(cases, order_by = report_period, n = 1)) %>%
    mutate(lag_cases_2 = lag(cases, order_by = report_period, n = 2)) %>%
    mutate(lag_cases_3 = lag(cases, order_by = report_period, n = 3)) %>%
    ungroup() %>%
    mutate(lag_sum = apply(select(., starts_with("lag_cases")), 1, sum_na)) %>%
    select(country_iso3c, disease, taxa, report_period, cases, starts_with("lag"))

  out <- newdata %>%
    mutate(report_period = as.numeric(paste0(report_year, report_semester))) %>% # temp for filtering
    left_join(model_lookup, by = c("country_iso3c", "disease", "taxa", "report_period"))


  return(out)
}



#' Get baseline cases nowcast
#' @import repeldata dplyr tidyr
#' @importFrom assertthat assert_that has_name
#' @return a list with predictions and metadata
#' @export
nowcast_baseline_model <- function(conn, dat){

  # assert that newdata has columns needed for model
  assertthat::has_name(dat, c("country_iso3c", "report_year", "report_semester", "disease", "taxa"))

  # run augment function
  dat_augmented <- nowcast_baseline_augment(conn, newdata = dat)

  # return predictions
  structure(list(predictions = dat_augmented$lag_cases_1,
            predict_function = nowcast_baseline_augment,
            class = c("nowcast_baseline", "nowcast_model")))
}


#' Get baseline actual cases
#' @import repeldata dplyr tidyr
#' @importFrom assertthat assert_that has_name
#' @export
nowcast_score <- function(model, newdata, conn){

  # assert that newdata has columns needed for model
  assertthat::has_name(newdata, c("country_iso3c", "report_year", "report_semester", "disease", "taxa"))

  # run augment function
  newdata_augmented <- nowcast_baseline_augment(conn, newdata)

  # return predictions & real values & performance statistic & residuals
  out <- newdata_augmented %>%
    rename(actual_cases = cases, predicted_cases = lag_cases_1,) %>%
    mutate(residual_error = abs(actual_cases - predicted_cases)) %>%
    select(-starts_with("lag"))

  rmse <- sqrt(mean(out$residual_error, na.rm = TRUE)^2)

  return(list(out, rmse))
}

#TODO separate validate functions
# 1. nowcast_score() = generic to lookup real values
#    augment predict + real values + performance statistic + residuals
#    (give NAs for no real values avail)
# 2. nowcast_validate() = run nowcast_test() on the validation dataset
#    bootstap validate dataset - return performance statistic (w bootstrap noise)
#TODO function for handling missing results - new diseases, etc


