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
nowcast_baseline_predict <- function(conn, country_iso3c, year, semester, disease, taxa, validate = FALSE){

  # Get data
  repel <- repel_cases_split(conn)
  repel <- read_csv("repel.csv") # temp

  # Generate prediction period number for filter
  prediction_period <- as.numeric(paste0(year, semester))

  #TODO handling these NAs
  # repel %>%
  #   filter(is.na(taxa)) # 38 cases of taxa NA - these represent "..." ie all species??

  # Filter data for given country, disease, taxa
  cases_cdt <- repel %>%
    filter(country_iso3c == !!country_iso3c, disease == !!disease, taxa == !!taxa) %>%
    mutate(prediction_period = as.numeric(paste0(report_year, report_semester))) # temp for filtering

  # Get prediction from the previous semester
  cases_cdt_predict <- cases_cdt %>%
    filter(prediction_period < !!prediction_period) %>%
    filter(prediction_period == max(prediction_period)) %>%
    mutate(prediction_year = !!year) %>%
    mutate(prediction_semester = !!semester) %>%
    select(country, country_iso3c, disease, taxa, report_year, report_semester, prediction_year, prediction_semester, predicted_cases = cases)

  assertthat::assert_that(nrow(cases_cdt_predict) <= 1)

  # If prediction is unavailable...
  if(nrow(cases_cdt_predict)==0){

    # Check whether country code, disease, and taxa are available in data (may be misspelling of input vars)
    if(!country_iso3c %in% unique(repel$country_iso3c)) warning(paste("country code", country_iso3c, "not in the REPEL database"))
    if(!disease %in% unique(repel$disease)) warning(paste("disease", disease, "not in the REPEL database"))
    if(!taxa %in% unique(repel$taxa)) warning(paste("taxa", taxa, "not in the REPEL database"))

    # Lookup country name for output data
    country <- na_empty(unique(repel$country[repel$country_iso3c == country_iso3c]))

    # Create output tibble with 0 predicted cases
    cases_cdt_predict <- tibble(country, country_iso3c, disease, taxa,
                                report_year = NA_integer_, report_semester = NA_integer_,
                                prediction_year = !!year, prediction_semester = !!semester,
                                predicted_cases = 0)
  }

  # If validating, get actual value if it exists
  if(validate){

    # Actual cases
    cases_cdt_actual <- cases_cdt %>%
      filter(prediction_period == !!prediction_period) %>%
      pull(cases)
    assertthat::assert_that(length(cases_cdt_actual) <= 1)

    # Add actual cases and calculate prediction error
    cases_cdt_predict <- cases_cdt_predict %>%
      mutate(report_cases = na_empty(cases_cdt_actual)) %>%
      mutate(prediction_error = abs(report_cases - predicted_cases))
  }

  return(cases_cdt_predict)
}

#' @noRd
na_empty <- function(x){
  na_var <- switch(class(x), "numeric" = NA_integer_, "character" = NA_character_)
  if(!length(x))  x <- na_var
  return(x)
}
