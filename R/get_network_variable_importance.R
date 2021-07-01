#' Get variable importance for given disease and month, by country
#' @param network_lme_augment_predict output of `get_cached_network_predictions()`
#' @param randef output of `get_cached_network_random_effects()`
#' @param country_iso3c iso3c(s) of countries
#' @param disease disease(s) name(s)
#' @param month in format of "yyyy-mm-01". if NULL, uses latest month in dataset
#' @return dataframe containing actual values ('x'), coefficients ('coef') and variable importance
#' @import dplyr tidyr
#' @export
get_network_variable_importance <- function(network_lme_augment_predict, randef,
                                            country_iso3c, diseases = get_oie_high_importance_diseases(),
                                            month = NULL){

  if(is.null(month)) month <- max(network_lme_augment_predict$month)

  network_lme_augment_predict %>%
    filter(disease %in% !!diseases)  %>%
    filter(country_iso3c %in% !!country_iso3c) %>%
    filter(month == !!month) %>%
    pivot_wider(names_from = continent, values_from = continent, names_prefix = "continent") %>%
    mutate_at(vars(starts_with("continent")), ~ifelse(!is.na(.), 1, NA)) %>%
    pivot_longer(cols = -c("country_iso3c", "country", "disease", "month", "outbreak_start","endemic", "outbreak_ongoing", "predicted_outbreak_probability"), names_to = "variable", values_to = "x") %>%
    drop_na(x) %>%
    left_join(randef) %>%
    mutate(variable_importance = x * coef) %>%
    mutate(pos = variable_importance > 0)

}

