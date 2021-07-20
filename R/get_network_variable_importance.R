#' Get variable importance for given disease and month, by country
#' @param conn repeldata connection
#' @param country_iso3c iso3c(s) of countries
#' @param disease disease(s) name(s)
#' @param month in format of "yyyy-mm-01". if NULL, uses latest month
#' @return dataframe containing actual values ('x'), coefficients ('coef') and variable importance
#' @import dplyr tidyr
#' @importFrom readr read_csv
#' @importFrom lubridate floor_date
#' @export
get_network_variable_importance <- function(conn,
                                            country_iso3c,
                                            diseases = get_oie_high_importance_diseases(),
                                            month = NULL){

  if(is.null(month)) month <- floor_date(Sys.Date(), unit = "month")

  # get augmented data
  network_augment_predict <- tbl(conn,  "network_lme_augment_predict") %>%
    filter(disease %in% !!diseases)  %>%
    filter(country_iso3c %in% !!country_iso3c) %>%
    filter(month == !!month) %>%
    select(-db_network_etag, -outbreak_subsequent_month) %>%
    collect() %>%
    pivot_wider(names_from = continent, values_from = continent, names_prefix = "continent") %>%
    mutate_at(vars(starts_with("continent")), ~ifelse(!is.na(.), 1, NA)) %>%
    pivot_longer(cols = -c("country_iso3c", "disease", "month", "outbreak_start","endemic", "outbreak_ongoing", "predicted_outbreak_probability"), names_to = "variable", values_to = "x")

  # get model coeffs
  randef_disease <- tbl(conn, "network_lme_coefficients") %>% collect()
  network_scaling_values <-  tbl(conn, "network_lme_scaling_values") %>% collect() %>% rename(variable = key)

  # join together augment and coeffs, calc variable importance
  network_augment_predict <- network_augment_predict %>%
    left_join(randef_disease, by = c("disease", "variable")) %>%
    left_join(network_scaling_values, by = "variable") %>%
    mutate(x = (x - `mean`) / `sd`) %>%
    mutate(variable_importance = x * coef) %>%
    mutate(pos = variable_importance > 0)

  # country name lookup
  country_lookup <- readr::read_csv(system.file("lookup", "countrycode_lookup.csv", package = "repelpredict"),  col_types = cols(
    country = col_character(),
    country_iso3c = col_character()
  ))

  network_augment_predict <- left_join(network_augment_predict, country_lookup,  by = "country_iso3c")

  return(network_augment_predict)

}

#' Get variable importance for given disease and month, by country and country origin
#' @param conn repeldata connection
#' @param country_iso3c iso3c(s) of countries
#' @param disease disease(s) name(s)
#' @param month in format of "yyyy-mm-01". if NULL, uses latest month
#' @return dataframe containing actual values ('x'), coefficients ('coef') and variable importance by country origin
#' @importFrom readr read_csv
#' @importFrom lubridate floor_date
#' @import dplyr tidyr
#' @export
get_network_variable_importance_with_origins <- function(conn,
                                                         country_iso3c,
                                                         diseases = get_oie_high_importance_diseases(),
                                                         month = NULL){

  if(is.null(month)) month <- floor_date(Sys.Date(), unit = "month")

  disagg_network_augment_predict <- tbl(conn,  "network_lme_augment_predict_by_origin") %>%
    filter(disease %in% !!diseases)  %>%
    filter(country_iso3c %in% !!country_iso3c) %>%
    filter(month == !!month) %>%
    select(-outbreak_subsequent_month) %>%
    rename(country_origin_iso3c = country_origin) %>%
    collect() %>%
    pivot_wider(names_from = continent, values_from = continent, names_prefix = "continent") %>%
    mutate_at(vars(starts_with("continent")), ~ifelse(!is.na(.), 1, NA)) %>%
    pivot_longer(cols = -c("country_iso3c", "country_origin_iso3c", "disease", "month", "outbreak_start","endemic", "outbreak_ongoing", "predicted_outbreak_probability"),
                 names_to = "variable", values_to = "x")


  # get model coeffs
  randef_disease <- tbl(conn, "network_lme_coefficients") %>% collect()
  network_scaling_values <-  tbl(conn, "network_lme_scaling_values") %>% collect() %>% rename(variable = key)

  # join together augment and coeffs, calc variable importance
  disagg_network_augment_predict <- disagg_network_augment_predict %>%
    left_join(randef_disease, by = c("disease", "variable")) %>%
    left_join(network_scaling_values, by = "variable") %>%
    #TODO how to handle standardization for individual contributions????
    mutate(x = (x - `mean`) / `sd`) %>%
    mutate(variable_importance = x * coef) %>%
    mutate(pos = variable_importance > 0)

  # country name lookup
  country_lookup <- readr::read_csv(system.file("lookup", "countrycode_lookup.csv", package = "repelpredict"),  col_types = cols(
    country = col_character(),
    country_iso3c = col_character()
  ))

  disagg_network_augment_predict <- left_join(disagg_network_augment_predict, country_lookup, by = "country_iso3c") %>%
    left_join(country_lookup %>% rename(country_origin = country, country_origin_iso3c = country_iso3c), by = "country_origin_iso3c")

  return(disagg_network_augment_predict)

}



#' Get contribution of origin countries to import risk
#' @param conn repeldata connection
#' @param country_iso3c iso3c(s) of countries
#' @param disease disease(s) name(s)
#' @param month in format of "yyyy-mm-01". if NULL, uses latest month
#' @return dataframe containing sum of variable importance by country origin
#' @import dplyr tidyr
#' @export
get_network_origin_contribution_import_risk <- function(conn,
                                                        country_iso3c,
                                                        diseases = get_oie_high_importance_diseases(),
                                                        month = NULL){

  if(is.null(month)) month <- floor_date(Sys.Date(), unit = "month")

  vi_co <- get_network_variable_importance_with_origins(conn,
                                                        country_iso3c = country_iso3c,
                                                        diseases =  diseases,
                                                        month = month)

  vi_co %>%
    filter(str_detect(variable, "from_outbreaks")) %>%
    group_by(month, disease,
             country, country_iso3c, country_origin, country_origin_iso3c,
             predicted_outbreak_probability) %>%
    summarize(contribution_to_import_risk = sum(variable_importance)) %>%
    ungroup() %>%
    arrange(country, month, disease, -contribution_to_import_risk)

}

