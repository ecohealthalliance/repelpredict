devtools::load_all()
library(tictoc)

# baseline
sample_size <- 150
model_object <- nowcast_baseline_model()
conn <- repel_local_conn()
newdata <- repel_cases_train(conn) %>%
  select("country_iso3c", "report_year", "report_semester", "disease", "taxa") %>%
  slice(sample(1:nrow(.), size = sample_size))

tic()
augmented_data <- repel_augment.nowcast_baseline(model_object = model_object, conn = conn, newdata = newdata)
toc()

assertthat::are_equal(sample_size, nrow(augmented_data))
####TODO NEED TO HANDLE DUPEs (related to serotypes and disease populations, I think. Should be included in primary grouping vars)

tic()
predicted_data <- repel_predict.nowcast_baseline(model_object = model_object, augmented_data = augmented_data)
toc()

repel_score.nowcast_baseline(model_object = model_object, augmented_data = augmented_data)


##
















#' Augment data with predictions
#' @import repeldata dplyr tidyr
#' @importFrom assertthat assert_that has_name
#' @return a tibble
#' @export
nowcast_baseline_augment <- function(conn, newdata, n_semesters = NULL){

  assertthat::has_name(newdata, c("country_iso3c", "report_year", "report_semester", "disease", "taxa"))

  model_lookup <- repel_cases_split(conn) %>%
    mutate(report_period = as.numeric(paste0(report_year, report_semester))) %>% # temp for filtering
    left_join(expand(., nesting(country_iso3c, disease, taxa), report_period), .) %>%
    group_by(country_iso3c, disease, taxa) %>%
    mutate(cases_lag1 = lag(cases, order_by = report_period, n = 1, default = 0)) %>%
    mutate(cases_lag2 = lag(cases, order_by = report_period, n = 2, default = 0)) %>%
    mutate(cases_lag3 = lag(cases, order_by = report_period, n = 3, default = 0)) %>%
    ungroup() %>%
    select(country_iso3c, disease, taxa, report_period, starts_with("cases"))

  out <- newdata %>%
    mutate(report_period = as.numeric(paste0(report_year, report_semester))) %>% # temp for filtering
    left_join(model_lookup, by = c("country_iso3c", "disease", "taxa", "report_period")) %>%
    mutate_at(.vars = c("cases", "cases_lag1", "cases_lag2", "cases_lag3"), ~replace_na(., 0))

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
  structure(list(predictions = dat_augmented$cases_lag1,
                 predict_function = nowcast_baseline_augment,
                 score_function = nowcast_baseline_augment,
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


