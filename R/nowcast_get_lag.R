#' Get lags for case data
#' @import repeldata dplyr tidyr
#' @importFrom assertthat has_name assert_that
#'

get_nowcast_lag <- function(conn, casedat){
  # lookup table for augmenting
  #model_lookup <- get_case_lag_lookup()
  model_lookup <- case_lag_lookup(conn)

  # check casedat has correct input vars
  assertthat::has_name(casedat, group_vars)

  # check that taxa in casedat are relevant
  assertthat::assert_that(all(unique(casedat$taxa) %in% taxa_list))

  # get cases and last three semesters
  lagdat <- casedat %>%
    select(-suppressWarnings(one_of("cases"))) %>%
    mutate(report_period = as.integer(paste0(report_year, report_semester))) %>%
    left_join(model_lookup, by = c("country_iso3c", "disease", "disease_population", "taxa", "report_period")) %>%
    select(-report_period)

  # if disease is present and absent, select only present
  #TODO - move this to WAHIS (it's done for the animal diseases but not animal host tables)
  lagdat <- lagdat %>%
    mutate(disease_status_rank = recode(disease_status, "present" = 1, "suspected" = 1, "absent" = 2, "unreported" = 3)) %>%
    group_by_at(.vars = group_vars) %>%
    filter(disease_status_rank == min(disease_status_rank)) %>%
    ungroup() %>%
    select(-disease_status_rank)

  return(lagdat)
}
