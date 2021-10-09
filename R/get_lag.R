#' Get  lags in cases, disease status, and control measures for nowcast model
#' @param six_month_processed full six_months dataset processed by repel_init
#' @import repeldata dplyr tidyr
#' @importFrom assertthat has_name assert_that
#' @importFrom lubridate floor_date floor_date year
#' @export
get_lag <- function(six_month_processed, lags = 3){

  # first: expand
  this_year <- year(floor_date(Sys.Date(), unit = "year")) #TODO implement logic to get one semester into future
  years_to_expand <- crossing(report_year = 2005:this_year, report_semester = 1:2) #2005 is beginning of reporting

  six_month_expand <- six_month_processed %>%
    distinct(disease, disease_name_uncleaned, disease_population, taxa) %>%
    expand_grid(country_iso3c = unique(six_month_processed$country_iso3c),
                years_to_expand)

  six_month_processed_expanded <- left_join(six_month_expand, six_month_processed, by = c("country_iso3c", "report_year", "report_semester", "disease_name_uncleaned", "disease_population", "taxa", "disease")) %>%
    arrange(country_iso3c, taxa, disease, disease_population, report_year, report_semester)


  # look up lags
  six_month_processed_lagged <- six_month_processed_expanded %>%
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
