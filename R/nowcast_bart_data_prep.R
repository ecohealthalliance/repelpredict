#' Prep case data for BART model
#' @import repeldata dplyr tidyr
#'

nowcast_prep_bart <- function(traindat, conn){

  # get lag cases
  #TODO - should this be an the augment function or a separate function
  traindat_augment <- get_nowcast_lag(case_dat = traindat)

  # get summed lag values of adjacent countries
  borders <- tbl(conn, "connect_static_vars") %>%
    filter(shared_border == "t") %>%
    select(country_origin, country_destination) %>%
    collect()

  traindat_augment_borders <- traindat_augment %>%
    distinct(country_origin = country_iso3c, disease, taxa, report_year, report_semester) %>%
    left_join(borders)

  borders_augment <- get_nowcast_lag(case_dat = traindat_augment_borders %>%
                                                rename(country_iso3c = country_destination))

  borders_sum <- borders_augment %>%
    select(-cases) %>%
    pivot_longer(cols = c(cases_lag1, cases_lag2, cases_lag3)) %>%
    group_by(country_origin, disease, taxa, report_year, report_semester) %>%
    summarize(cases_border_countries = sum(as.numeric(value))) %>%
    ungroup() %>%
    rename(country_iso3c = country_origin)

  traindat_augment <- left_join(traindat_augment, borders_sum)

  # measures of vet capacity - counts
  vets <- tbl(conn, "annual_reports_veterinarians") %>%
    collect() %>%
    group_by(country_iso3c, report_year) %>%
    summarize(veterinarian_count = sum(as.integer(total_count), na.rm = TRUE)) %>%
    ungroup() %>%
    mutate(report_year = as.integer(report_year))

  traindat_augment <- left_join(traindat_augment, vets)

  # taxa population
  taxa <- tbl(conn, "annual_reports_animal_population") %>%
    filter(population_units == "animals") %>%
    collect() %>%
    group_by(country_iso3c, report_year, taxa) %>%
    summarize(taxa_pop_count = sum(as.integer(population_count), na.rm = TRUE)) %>%
    ungroup() %>%
    mutate(report_year = as.integer(report_year))

  traindat_augment <- left_join(traindat_augment, taxa)

  #fao <- read_csv("FAOSTAT_data_5-18-2020.csv") # alt to OIE counts - needs wrangling

  # GDP - to come from db
  gdp <- jsonlite::fromJSON(paste0("http://api.worldbank.org/v2/country/all/indicator/NY.GDP.MKTP.CD?per_page=20000&format=json"))
  gdp <- gdp[[2]] %>%
    dplyr::select(report_year = date, gdp = value, country_iso3c = countryiso3code) %>%
    as_tibble() %>%
    filter(country_iso3c != "") %>%
    drop_na() %>%
    mutate(report_year = as.integer(report_year))

  traindat_augment <- left_join(traindat_augment, gdp)

  return(traindat_augment)

}

