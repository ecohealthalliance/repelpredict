#' Get variable importance for given disease and month, by country
#' @param network_augment_predict output of `get_cached_network_predictions()`
#' @param randef output of `get_cached_network_random_effects()`
#' @param country_iso3c iso3c(s) of countries
#' @param disease disease(s) name(s)
#' @param month in format of "yyyy-mm-01". if NULL, uses latest month in dataset
#' @return dataframe containing actual values ('x'), coefficients ('coef') and variable importance
#' @import dplyr tidyr
#' @export
get_network_variable_importance <- function(conn,
                                            country_iso3c,
                                            diseases = get_oie_high_importance_diseases(),
                                            month = NULL){

  if(is.null(month)) month <- max(network_augment_predict$month)

  # do all filtering etc in db
  network_augment_predict <- dbReadTable(conn, name = "network_lme_augment_predict") %>%
    select(-db_network_etag) %>%
    rename(outbreak_ongoing = outbreak_subsequent_month) %>%
    filter(disease %in% !!diseases)  %>%
    filter(country_iso3c %in% !!country_iso3c) %>%
    filter(month == !!month) %>%
    pivot_wider(names_from = continent, values_from = continent, names_prefix = "continent") %>%
    mutate_at(vars(starts_with("continent")), ~ifelse(!is.na(.), 1, NA)) %>%
    pivot_longer(cols = -c("country_iso3c", "country", "disease", "disease_clean", "month", "outbreak_start","endemic", "outbreak_ongoing", "predicted_outbreak_probability"), names_to = "variable", values_to = "x") %>%
    drop_na(x) %>%
    left_join(randef) %>%
    mutate(variable_importance = x * coef) %>%
    mutate(pos = variable_importance > 0)

  # get randef from db and merge in db

  # read in and do lookups before returning
  country_lookup <- readr::read_csv(system.file("lookup", "countrycode_lookup.csv", package = "repelpredict"),  col_types = cols(
    country.name.en = col_character(),
    iso3c = col_character()
  ))

  disease_lookup <- tibble(disease = unique(network_augment_predict$disease)) %>%
    mutate(disease_clean = str_to_title(str_replace_all(disease, "_", " ")))

  network_lme_augment_predict <- left_join(network_lme_augment_predict, country_lookup) %>%
    left_join(disease_lookup) %>%
    as_tibble()

}

#' Get variable importance for given disease and month, by country
#' @param disagg_network_augment_predict output of `get_cached_disagg_network_predictions()`
#' @param randef output of `get_cached_network_random_effects()`
#' @param country_iso3c iso3c(s) of countries
#' @param disease disease(s) name(s)
#' @param month in format of "yyyy-mm-01". if NULL, uses latest month in dataset
#' @return dataframe containing actual values ('x'), coefficients ('coef') and variable importance
#' @import dplyr tidyr
#' @export
get_network_variable_importance_with_origins <- function(disagg_network_augment_predict, randef,
                                            country_iso3c, diseases = get_oie_high_importance_diseases(),
                                            month = NULL){

  if(is.null(month)) month <- max(network_lme_augment_predict$month)

  disagg_network_augment_predict <- disagg_network_augment_predict %>%
    rename(outbreak_ongoing = outbreak_subsequent_month) %>%
    rename(country_origin_iso3c = country_origin)

  country_lookup <- tibble(country_iso3c = unique(disagg_network_augment_predict$country_iso3c)) %>%
    mutate(country = countrycode::countrycode(country_iso3c, origin = "iso3c", destination = "country.name"))

  country_lookup2 <- tibble(country_origin_iso3c = unique(disagg_network_augment_predict$country_origin_iso3c)) %>%
    mutate(country_origin = countrycode::countrycode(country_origin_iso3c, origin = "iso3c", destination = "country.name"))

  disease_lookup <- tibble(disease = unique(disagg_network_augment_predict$disease)) %>%
    mutate(disease_clean = str_to_title(str_replace_all(disease, "_", " ")))

  disagg_network_augment_predict <- left_join(disagg_network_augment_predict, country_lookup) %>%
    left_join(country_lookup2) %>%
    left_join(disease_lookup) %>%
    as_tibble()

  disagg_network_augment_predict %>%
    filter(country_iso3c == "USA",
           disease %in% get_oie_high_importance_diseases(),
           month == "2018-12-01") %>%
    pivot_wider(names_from = continent, values_from = continent, names_prefix = "continent") %>%
    mutate_at(vars(starts_with("continent")), ~ifelse(!is.na(.), 1, NA)) %>%
    pivot_longer(cols = -c("country_iso3c", "country", "country_origin_iso3c", "country_origin", "disease", "disease_clean", "month", "outbreak_start","endemic", "outbreak_ongoing"), names_to = "variable", values_to = "x") %>%
    drop_na(x) %>%
    left_join(randef) %>%
    mutate(variable_importance = x * coef) %>%
    mutate(pos = variable_importance > 0)

}
