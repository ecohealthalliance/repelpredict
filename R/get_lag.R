#' @export
repel_lag <- function(x, ...){
  UseMethod("repel_lag")
}


#' Get case and status lags
#' @import repeldata dplyr tidyr
#' @importFrom assertthat has_name assert_that
#' @export
repel_lag.nowcast_model <- function(model_object, conn, six_month_reports_summary = NULL, newdata, lags = 1:3, control_measures = FALSE){

  if(is.null(six_month_reports_summary))   six_month_reports_summary <- repel_split(model_object, conn)

  # add 2 semesters into future (to be able to pull lag from reports two years into future)
  last_two <- six_month_reports_summary %>%
    arrange(report_year, report_semester) %>%
    distinct(report_year, report_semester) %>%
    tail(2)

  if(n_distinct(last_two$report_year) == 1){
    future_two <- tibble(report_year = max(last_two$report_year) + 1, report_semester = 1:2)
  }else{
    future_two <- tibble(report_year = c(max(last_two$report_year), max(last_two$report_year) + 1), report_semester = 2:1)
  }

  future_reports <- six_month_reports_summary %>%
    distinct(country_iso3c, disease, disease_population, taxa) %>%
    expand_grid(future_two)

  model_lookup <- six_month_reports_summary %>%
    bind_rows(future_reports) %>%
    mutate(report_period = as.numeric(paste0(report_year, report_semester))) %>%
    left_join(expand(., nesting(country_iso3c, disease, disease_population, taxa), report_period), .,  by = c("country_iso3c", "disease", "disease_population", "taxa", "report_period")) %>%
    group_by(country_iso3c, disease, disease_population, taxa)

  for(i in lags){
    model_lookup <- model_lookup %>%
      mutate(!!paste0("cases_lag", i) := lag(cases, order_by = report_period, n = i, default = NA)) %>%
      mutate(!!paste0("disease_status_lag", i) := lag(disease_status, order_by = report_period, n = i, default = NA))
  }

  if(control_measures){
    for(i in lags){
      model_lookup <- model_lookup %>%
        mutate(!!paste0("control_measures_lag", i) := lag(control_measures, order_by = report_period, n = i, default = NA))
    }
  }

  model_lookup <- model_lookup %>%
    ungroup() %>%
    select(country_iso3c, disease, disease_population, taxa, report_period, disease_status,
           starts_with("cases"), starts_with("disease_status"),
           all_of(starts_with("control_measures"))
           )

  # get cases and last three semesters
  lagged_newdata <- newdata %>%
    select(-suppressWarnings(one_of("cases")), -suppressWarnings(one_of("disease_status")), -suppressWarnings(one_of("control_measures"))) %>%
    mutate(report_period = as.integer(paste0(report_year, report_semester))) %>%
    left_join(model_lookup, by = c("country_iso3c", "disease", "disease_population", "taxa", "report_period")) %>%
    select(-report_period, -control_measures)

  return(lagged_newdata)
}
