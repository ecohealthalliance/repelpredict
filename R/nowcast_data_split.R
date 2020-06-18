taxa_list <- c("goats", "sheep",  "sheep/goats", # handling for multiple species, sheep/goats (combine all sheep goats?)
               "cattle", "birds", "camelidae", "dogs", "equidae",
               "cats", "cervidae" , "swine" , "buffaloes", "hares/rabbits")

#' @import repeldata dplyr tidyr
#' @importFrom DBI dbDisconnect
#' @importFrom digest digest2int
#' @noRd
repel_cases <- function(conn){
  tbl(conn, "annual_reports_animal_hosts") %>%
    filter(taxa %in% taxa_list) %>%
    filter(report_semester != "0") %>%
    select(report, country, country_iso3c, report_year, report_semester, disease, disease_population, serotype, disease_status, taxa, cases) %>%
    collect() %>%
    mutate_at(.vars = c("report_year", "report_semester", "cases"), ~suppressWarnings(as.integer(.))) %>%
    mutate(validation_set = digest::digest2int(paste0(report, disease, disease_population, serotype, disease_status, taxa)) %% 5 == 1)
  #TODO all diseases and taxa need to be in training?
}

#' Get cases training set (~80%)
#' @import repeldata dplyr tidyr
#' @return a tibble
#' @export
repel_cases_train <- function(conn){
  repel_cases(conn) %>%
    filter(!validation_set) %>%
    select(-validation_set)
}


#' Get cases validation set (~20%)
#' @import repeldata dplyr tidyr
#' @return a tibble
#' @export
repel_cases_validate <- function(conn){
  repel_cases(conn) %>%
    filter(validation_set) %>%
    select(-validation_set)
}
