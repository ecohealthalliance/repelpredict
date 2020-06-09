#' define generic augment
repel_augment <- function(x, ...){
  UseMethod("repel_augment")
}


#' Augment nowcast baseline model object
#' @import repeldata dplyr tidyr
#' @importFrom assertthat has_name assert_that
#'
repel_augment.nowcast_baseline <- function(model_object, conn, newdata) {

  get_nowcast_lag(conn, casedat = newdata) %>%
    select(-cases_lag2, -cases_lag3)

}

#' Augment nowcast bart model object
#' @import repeldata dplyr tidyr
#' @importFrom assertthat has_name assert_that
#'
repel_augment.nowcast_bart <- function(model_object, conn, traindat, binary_outcome = TRUE) {

  # get lag cases
  traindat_augment <- get_nowcast_lag(conn, casedat = traindat)

  # get summed lag values of adjacent countries
  borders <- tbl(conn, "connect_static_vars") %>%
    filter(shared_border == "t") %>%
    select(country_origin, country_destination) %>%
    collect()

  traindat_augment_borders <- traindat_augment %>%
    distinct(country_origin = country_iso3c, disease, taxa, report_year, report_semester) %>%
    left_join(borders,  by = "country_origin") %>%
    rename(country_iso3c = country_destination)

  borders_augment <- get_nowcast_lag(conn, casedat = traindat_augment_borders)

  borders_sum <- borders_augment %>%
    select(-cases) %>%
    pivot_longer(cols = c(cases_lag1, cases_lag2, cases_lag3)) %>%
    group_by(country_origin, disease, taxa, report_year, report_semester) %>%
    summarize(cases_border_countries = sum_na(as.integer(value))) %>%
    ungroup() %>%
    rename(country_iso3c = country_origin)

  traindat_augment <- left_join(traindat_augment, borders_sum, by = c("country_iso3c", "report_year", "report_semester", "disease", "taxa"))

  # measures of vet capacity - counts ()
  vets <- tbl(conn, "annual_reports_veterinarians") %>%
    collect() %>%
    group_by(country_iso3c, report_year) %>%
    summarize(veterinarian_count = sum_na(suppressWarnings(as.integer(total_count)))) %>%
    ungroup() %>%
    mutate(report_year = as.integer(report_year))

  traindat_augment <- left_join(traindat_augment, vets, by = c("country_iso3c", "report_year"))

  # taxa population
  taxa <- tbl(conn, "country_taxa_population") %>%
    collect() %>%
    rename(report_year = year, taxa_population = population)

  traindat_augment <- left_join(traindat_augment, taxa, by = c("country_iso3c", "report_year", "taxa"))

  # GDP
  gdp <-  tbl(conn, "country_gdp") %>%
    collect() %>%
    rename(report_year = year)

  traindat_augment <- left_join(traindat_augment, gdp,  by = c("country_iso3c", "report_year"))

  # finalize
  if(binary_outcome){
    # disease_status column needed for binary bart model
    assertthat::has_name(traindat_augment, c("disease_status"))

    traindat_augment <- traindat_augment %>%
      select(-cases) %>%
      mutate(disease_status = recode(disease_status, "present" = 1, "suspected" = 1, "absent" = 0))
  }
  return(traindat_augment)

}

