#' Get  lags in cases, disease status, and control measures for nowcast model
#' @import repeldata dplyr tidyr
#' @importFrom assertthat has_name assert_that
#' @export
get_lag <- function(six_month_processed, lags = 3){

  six_month_processed_lagged <- six_month_processed %>%
    mutate(report_period = as.numeric(paste0(report_year, report_semester))) %>%
    group_by(country_iso3c, disease, disease_population, taxa)

  for(i in 1:lags){
    six_month_processed_lagged <- six_month_processed_lagged %>%
      mutate(!!paste0("cases_lag", i) := lag(cases, order_by = report_period, n = i, default = NA)) %>%
      mutate(!!paste0("disease_status_lag", i) := lag(disease_status, order_by = report_period, n = i, default = NA)) %>%
      mutate(!!paste0("control_measures_lag", i) := lag(control_measures, order_by = report_period, n = i, default = NA))
  }

  six_month_processed_lagged <- six_month_processed_lagged %>%
    ungroup() %>%
    select(country_iso3c, report_year, report_semester, disease, disease_population, taxa, disease_status,
           starts_with("cases"), starts_with("disease_status"),
           all_of(starts_with("control_measures"))
    )

  return(six_month_processed_lagged)
}
