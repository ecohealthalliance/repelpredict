#' Get lags for case data
#' @import repeldata dplyr tidyr
#' @importFrom assertthat has_name assert_that
#'

get_nowcast_lag <- function(case_dat){
  # lookup table for augmenting
  model_lookup <- get_case_lag_lookup()

  # check case_dat has correct input vars
  assertthat::has_name(case_dat, c("country_iso3c", "report_year", "report_semester", "disease", "taxa"))

  # check that taxa in case_dat are relevant
  assertthat::assert_that(all(unique(case_dat$taxa) %in% taxa_list))

  # get cases and last three semesters
  case_dat %>%
    select(-suppressWarnings(one_of("cases"))) %>%
    mutate(report_period = as.integer(paste0(report_year, report_semester))) %>%
    left_join(model_lookup, by = c("country_iso3c", "disease", "taxa", "report_period"))

}
