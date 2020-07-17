#' Get lags for case data
#' @import repeldata dplyr tidyr
#' @importFrom assertthat has_name assert_that
#'

get_nowcast_lag <- function(conn, casedat, lags = 1:3){

  assertthat::has_name(casedat, grouping_vars)

  all_lags <-
    left_join(
      casedat %>% select(all_of(grouping_vars)) %>% select(-report_year, -report_semester),
      repel_cases(conn) %>% select(all_of(grouping_vars), cases, disease_status),
      c("country_iso3c", "disease", "disease_population", "taxa")
    ) %>%
    mutate(disease_status_rank = recode(disease_status, "present" = 1, "suspected" = 1, "absent" = 2, "unreported" = 3)) %>%
    group_by_at(.vars = grouping_vars) %>%
    filter(disease_status_rank == min(disease_status_rank)) %>%
    ungroup() %>%
    select(-disease_status_rank) %>%
    mutate(report_period = as.numeric(paste0(report_year, report_semester))) %>%
    left_join(expand(., nesting(country_iso3c, disease, disease_population, taxa), report_period), .,  by = c("country_iso3c", "disease", "disease_population", "taxa", "report_period")) %>%
    group_by(country_iso3c, disease, disease_population, taxa) %>%
    group_split(.keep = TRUE) %>%
    map_dfr(function(x) {
      x <- arrange(x, report_period)
      for (i in lags) {
        x[[paste0("cases_lag", i)]] <- dplyr::lag(x$cases, order_by = x$report_period, n = i, default = NA)
        x[[paste0("disease_status_lag", i)]] <- dplyr::lag(x$disease_status, order_by = x$report_period, n = i, default = NA)
      }
      return(x)
    }) %>%
    select(country_iso3c, disease, disease_population, taxa, report_year, report_semester, starts_with("cases_lag"), starts_with("disease_status_lag"))

  lagged_data <-
    left_join(
      casedat,
      all_lags,
      by = grouping_vars)
  # get cases and last three semesters

  assertthat::assert_that(all(unique(lagged_data$taxa) %in% taxa_list))

  return(lagged_data)
}
