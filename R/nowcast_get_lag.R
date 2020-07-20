#' Get lags for case data
#' @import repeldata dplyr tidyr
#' @importFrom assertthat has_name assert_that
#' @export
get_nowcast_lag <- function(conn, casedat, lags = 1:3){

  # check casedat has correct input vars
  assertthat::has_name(casedat, grouping_vars)

  # check that taxa in casedat are relevant
  assertthat::assert_that(all(unique(casedat$taxa) %in% taxa_list))

  # lookup table for augmenting
  model_lookup <-
    repel_cases(conn) %>%
    mutate(report_period = as.numeric(paste0(report_year, report_semester))) %>% # temp for filtering
    left_join(expand(., nesting(country_iso3c, disease, disease_population, taxa), report_period), .,  by = c("country_iso3c", "disease", "disease_population", "taxa", "report_period")) %>%
    group_by(country_iso3c, disease, disease_population, taxa)

  for(i in lags){
    model_lookup <- model_lookup %>%
      mutate(!!paste0("cases_lag", i) := lag(cases, order_by = report_period, n = i, default = NA)) %>%
      mutate(!!paste0("disease_status_lag", i) := lag(disease_status, order_by = report_period, n = i, default = NA))
  }

  model_lookup <- model_lookup %>%
    ungroup() %>%
    select(country_iso3c, disease, disease_population, taxa, report_period, disease_status, starts_with("cases"), starts_with("disease_status"))

  # get cases and last three semesters
  lagged_data <- casedat %>%
    select(-suppressWarnings(one_of("cases")), -suppressWarnings(one_of("disease_status"))) %>%
    mutate(report_period = as.integer(paste0(report_year, report_semester))) %>%
    left_join(model_lookup, by = c("country_iso3c", "disease", "disease_population", "taxa", "report_period")) %>%
    select(-report_period)

  return(lagged_data)
}
