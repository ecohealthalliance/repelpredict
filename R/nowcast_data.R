#' @import repeldata dplyr tidyr
#' @importFrom DBI dbDisconnect
#' @importFrom digest digest2int
#' @noRd
repel_cases_split <- function(conn){
  tbl(conn, "annual_reports_animal_hosts") %>%
    filter(!is.na(cases)) %>%
    filter(report_semester != "0") %>%
    mutate_at(.vars = c("report_year", "report_semester"), ~as.numeric(.)) %>%
    select(report, country, country_iso3c, report_year, report_semester, disease, disease_population, serotype, disease_status, taxa, cases) %>%
    collect() %>%
    mutate(validation_set = digest::digest2int(paste0(report, disease, disease_population, serotype, disease_status, taxa)) %% 5 == 1)
  #TODO all diseases and taxa need to be in training?
}

#' Get cases training set (~80%)
#' @import repeldata dplyr tidyr
#' @return a tibble
#' @export
repel_cases_train <- function(conn){
  repel_cases_split(conn) %>%
    filter(!validation_set) %>%
    select(-validation_set)
}


#' Get cases validation set (~20%)
#' @import repeldata dplyr tidyr
#' @return a tibble
#' @export
repel_cases_validate <- function(conn){
  repel_cases_split(conn) %>%
    filter(validation_set) %>%
    select(-validation_set)
}
