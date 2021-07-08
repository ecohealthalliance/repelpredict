#' Get variable importance for given disease and month, by country
#' @param conn repeldata connection
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

  #  if(is.null(month)) month <- max(network_augment_predict$month)

  # get augmented data
  network_augment_predict <- tbl(conn,  "network_lme_augment_predict") %>%
    filter(disease %in% !!diseases)  %>%
    filter(country_iso3c %in% !!country_iso3c) %>%
    filter(month == !!month) %>%
    select(-db_network_etag) %>%
    rename(outbreak_ongoing = outbreak_subsequent_month) %>%
    collect() %>%
    pivot_wider(names_from = continent, values_from = continent, names_prefix = "continent") %>%
    mutate_at(vars(starts_with("continent")), ~ifelse(!is.na(.), 1, NA)) %>%
    pivot_longer(cols = -c("country_iso3c", "disease", "month", "outbreak_start","endemic", "outbreak_ongoing", "predicted_outbreak_probability"), names_to = "variable", values_to = "x")

  # get model coeffs (to be transferred to db)
  model_object <-  network_lme_model(
    network_model = aws.s3::s3readRDS(bucket = "repeldb/models", object = "lme_mod_network.rds"),
    network_scaling_values = aws.s3::s3readRDS(bucket = "repeldb/models", object = "network_scaling_values.rds")
  )

  lme_mod <- model_object$network_model

  randef <- ranef(lme_mod)
  randef_disease <- randef$disease %>%
    tibble::rownames_to_column(var = "disease") %>%
    as_tibble()%>%
    filter(disease %in% !!diseases)  %>%
    pivot_longer(-disease, names_to = "variable", values_to = "coef") %>%
    mutate(disease_clean = str_to_title(str_replace_all(disease, "_", " "))) %>%
    mutate(variable_clean = str_replace(variable, "_from_outbreaks", " from countries with existing outbreak"),
           variable_clean = str_replace(variable_clean, "fao_trade_", ""),
           variable_clean = str_replace(variable_clean, "_other", " (other)"),
           variable_clean = str_replace_all(variable_clean, "_", " "),
           variable_clean = str_remove(variable_clean, "continent"),
           variable_clean = str_replace(variable_clean, "shared borders from countries with existing outbreak", "shared borders with country with existing outbreak"))

  # join together augment and coeffs, calc variable importance
  network_augment_predict <- network_augment_predict %>%
    left_join(randef_disease, by = c("disease", "variable")) %>%
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

#' Get variable importance for given disease and month, by country
#' @param conn repeldata connection
#' @param country_iso3c iso3c(s) of countries
#' @param disease disease(s) name(s)
#' @param month in format of "yyyy-mm-01". if NULL, uses latest month in dataset
#' @return dataframe containing actual values ('x'), coefficients ('coef') and variable importance
#' @import dplyr tidyr
#' @export
get_network_variable_importance_with_origins <- function(conn,
                                                         country_iso3c,
                                                         diseases = get_oie_high_importance_diseases(),
                                                         month = NULL){

  #if(is.null(month)) month <- max(network_lme_augment_predict$month)

  disagg_network_augment_predict <- tbl(conn,  "network_lme_augment_disaggregated") %>%
    filter(disease %in% !!diseases)  %>%
    filter(country_iso3c %in% !!country_iso3c) %>%
    filter(month == !!month) %>%
    rename(outbreak_ongoing = outbreak_subsequent_month) %>%
    rename(country_origin_iso3c = country_origin) %>%
    collect() %>%
    pivot_wider(names_from = continent, values_from = continent, names_prefix = "continent") %>%
    mutate_at(vars(starts_with("continent")), ~ifelse(!is.na(.), 1, NA)) %>%
    pivot_longer(cols = -c("country_iso3c", "country_origin_iso3c", "disease", "month", "outbreak_start","endemic", "outbreak_ongoing"),
                 names_to = "variable", values_to = "x")


  # get model coeffs (to be transferred to db)
  model_object <-  network_lme_model(
    network_model = aws.s3::s3readRDS(bucket = "repeldb/models", object = "lme_mod_network.rds"),
    network_scaling_values = aws.s3::s3readRDS(bucket = "repeldb/models", object = "network_scaling_values.rds")
  )

  lme_mod <- model_object$network_model

  randef <- ranef(lme_mod)
  randef_disease <- randef$disease %>%
    tibble::rownames_to_column(var = "disease") %>%
    as_tibble()%>%
    filter(disease %in% !!diseases)  %>%
    pivot_longer(-disease, names_to = "variable", values_to = "coef") %>%
    mutate(disease_clean = str_to_title(str_replace_all(disease, "_", " "))) %>%
    mutate(variable_clean = str_replace(variable, "_from_outbreaks", " from countries with existing outbreak"),
           variable_clean = str_replace(variable_clean, "fao_trade_", ""),
           variable_clean = str_replace(variable_clean, "_other", " (other)"),
           variable_clean = str_replace_all(variable_clean, "_", " "),
           variable_clean = str_remove(variable_clean, "continent"),
           variable_clean = str_replace(variable_clean, "shared borders from countries with existing outbreak", "shared borders with country with existing outbreak"))

  # join together augment and coeffs, calc variable importance
  disagg_network_augment_predict <- disagg_network_augment_predict %>%
    left_join(randef_disease, by = c("disease", "variable")) %>%
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
