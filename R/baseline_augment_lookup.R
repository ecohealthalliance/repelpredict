#' Support function to generate the augment lookup for the baseline model
#' @param conn either repel_local_conn or repel_remote_conn objects from 'repeldata'
#' @import repeldata dplyr tidyr

#TODO this should be triggered with db updated
baseline_augment_lookup <- function(conn){
  repel_cases_split(conn) %>%
    mutate(report_period = as.numeric(paste0(report_year, report_semester))) %>% # temp for filtering
    left_join(expand(., nesting(country_iso3c, disease, taxa), report_period), .) %>%
    group_by(country_iso3c, disease, taxa) %>%
    mutate(cases_lag1 = lag(cases, order_by = report_period, n = 1, default = NA)) %>%
    mutate(cases_lag2 = lag(cases, order_by = report_period, n = 2, default = NA)) %>%
    mutate(cases_lag3 = lag(cases, order_by = report_period, n = 3, default = NA)) %>%
    ungroup() %>%
    select(country_iso3c, disease, taxa, report_period, starts_with("cases"))
}


#' column specs for reading in baseline_augment_lookup csv
#' @import readr
baseline_augment_lookup_specs <- function(){
  cols(
    country_iso3c = col_character(),
    disease = col_character(),
    taxa = col_character(),
    report_period = col_integer(),
    cases = col_integer(),
    cases_lag1 = col_integer(),
    cases_lag2 = col_integer(),
    cases_lag3 = col_integer()
  )
}
