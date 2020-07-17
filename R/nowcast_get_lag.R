#' Get lags for case data
#' @import repeldata dplyr tidyr
#' @importFrom assertthat has_name assert_that
#' @export
#'

get_nowcast_lag <- function(conn, casedat){
  # lookup table for augmenting
  #model_lookup <- get_case_lag_lookup()
  model_lookup <- case_lag_lookup(conn)

  # check casedat has correct input vars
  assertthat::has_name(casedat, grouping_vars)

  # check that taxa in casedat are relevant
  assertthat::assert_that(all(unique(casedat$taxa) %in% taxa_list))

  # get cases and last three semesters
  lagdat <- casedat %>%
    select(-suppressWarnings(one_of("cases")), -suppressWarnings(one_of("disease_status"))) %>%
    mutate(report_period = as.integer(paste0(report_year, report_semester))) %>%
    left_join(model_lookup, by = c("country_iso3c", "disease", "disease_population", "taxa", "report_period")) %>%
    select(-report_period)

  return(lagdat)
}
