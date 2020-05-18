library(tidyverse)
library(repeldata)
library(httr)
library(jsonlite)

devtools::load_all()
conn <- repel_remote_conn()
db_list_tables(conn)

library(dbarts)

fit_nowcast_bart <- function(conn){

  traindat <- repel_cases_train(conn) %>%
    select("country_iso3c", "report_year", "report_semester", "disease", "taxa") %>%
    # filter out aquatic taxa?
    filter(taxa %in% c("goats", "sheep",  "sheep/goats",
                       "cattle", "birds", "camelidae", "dogs", "equidae",
                       "cats", "cervidae" , "swine" , "buffaloes", "hares/rabbits"))
  #TODO filter min year (3 semesters) by country?

  # generate lag values (using augument) for last 3 semesters -> mean imputation if missing
  traindat_augment <- nowcast_baseline_augment(conn = conn, newdata = traindat)

  # get summed lag values of adjacent countries
  borders <- tbl(conn, "connect_static_vars") %>%
    filter(shared_border) %>%
    select(country_origin, country_destination) %>%
    collect()

  traindat_augment_borders <- traindat_augment %>%
    distinct(country_origin = country_iso3c, disease, taxa, report_year, report_semester) %>%
    left_join(borders)

  borders_augment <- nowcast_baseline_augment(conn = conn,
                                              newdata = traindat_augment_borders %>%
                                                rename(country_iso3c = country_destination))
  borders_sum <- borders_augment %>%
    select(-cases) %>%
    pivot_longer(cols = c(cases_lag1, cases_lag2, cases_lag3)) %>%
    group_by(country_origin, disease, taxa, report_year, report_semester) %>%
    summarize(cases_border_countries = sum(value)) %>%
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

  # GDP
  gdp <- fromJSON(paste0("http://api.worldbank.org/v2/country/all/indicator/NY.GDP.MKTP.CD?per_page=20000&format=json"))
  gdp <- gdp[[2]] %>%
    dplyr::select(report_year = date, gdp = value, country_iso3c = countryiso3code) %>%
    as_tibble() %>%
    filter(country_iso3c != "") %>%
    drop_na() %>%
    mutate(report_year = as.integer(report_year))

  traindat_augment <- left_join(traindat_augment, gdp)


  # call dbart package - predict - return list structure with class  (class(obj), nowcast_bart, nowcast_model) (in this order)
  # list contains model object, other slots for anything else (eg metadata)

  # score(model_object)

  # validate(model_object, newdata)

}

# model object saved as .rds
# drake? (later)
# objects loaded in package (load with sys.file)? or plumber api (separate from this package)
# tidymodels - strip down model objects with butcher?

