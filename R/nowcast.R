
#' @import repeldata dplyr tidyr
#' @importFrom DBI dbDisconnect
#' @importFrom digest digest2int
#' @noRd
repel_cases_split <- function(conn){

  cases <- tbl(conn, "annual_reports_animal_hosts") %>%
    filter(!is.na(cases)) %>%
    filter(report_semester != "0") %>%
    mutate_at(.vars = c("report_year", "report_semester"), ~as.numeric(.)) %>%
    select(report, country, country_iso3c, report_year, report_semester, disease, disease_population, serotype, disease_status, taxa, cases) %>%
    collect() %>%
    mutate(validation_set = digest::digest2int(paste0(report, disease, disease_population, serotype, disease_status, taxa)) %% 5 == 1)

  #TODO all diseases and taxa need to be in training?
  return(cases)
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


#' Get baseline cases nowcast
#' @import repeldata dplyr tidyr
#' @importFrom assertthat assert_that
#' @export
nowcast_baseline_predict <- function(conn, country_iso3c, year, disease, taxa){

  # disease <- "foot-and-mouth disease"
  # taxa <- "swine"
  # country_iso3c <- "IND"
  # year <- 2018
  # conn <- repel_remote_conn()

  repel <- repel_cases_split(conn)

  repel %>%
    filter(is.na(taxa)) # 38 cases of taxa NA - these represent "..." ie all species??

  cases <- repel %>%
    filter(country_iso3c == !!country_iso3c, disease == !!disease, taxa == !!taxa) %>%
    filter(report_year < !!year) %>%
    filter(report_year == max(report_year)) %>%
    filter(report_semester == max(report_semester)) %>%
    mutate(prediction_year = !!year) %>%
    select(country, country_iso3c, disease, taxa, report_year, report_semester, prediction_year, cases)

  assertthat::assert_that(nrow(cases) <= 1)

  if(nrow(cases)==0){

    if(!country_iso3c %in% unique(repel$country_iso3c)) warning(paste("country code", country_iso3c, "not in the REPEL database"))
    if(!disease %in% unique(repel$disease)) warning(paste("disease", disease, "not in the REPEL database"))
    if(!taxa %in% unique(repel$taxa)) warning(paste("taxa", taxa, "not in the REPEL database"))

    country <- unique(repel$country[repel$country_iso3c == country_iso3c])

    cases <- tibble(country, country_iso3c, disease, taxa, report_year = NA, report_semester = NA, prediction_year = !!year, cases = 0)
  }

  return(cases)
}

