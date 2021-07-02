#' Get disease status and predicted outbreak prob by country
#' @param network_boost_augment_predict output of `get_cached_nowcast_predictions()`
#' @param network_lme_augment_predict output of `get_cached_network_predictions()`
#' @param country_iso3c iso3c(s) of countries
#' @param disease disease(s) name(s)
#' @param month in format of "yyyy-mm-01". if NULL, uses latest month in dataset
#' @return dataframe containing actual values ('x'), coefficients ('coef') and variable importance
#' @import dplyr tidyr
#' @export
get_disease_status_predict <- function(network_boost_augment_predict,
                                       network_lme_augment_predict,
                                       country_iso3c, diseases = get_oie_high_importance_diseases()){

  outbreak_predict <- network_lme_augment_predict %>%
    filter(disease %in% !!diseases)  %>%
    filter(country_iso3c %in% !!country_iso3c) %>%
    mutate(report_year = year(month)) %>%
    mutate(report_semester = ifelse(month(month) < 7, 1, 2)) %>%
    mutate(yr = report_year + (report_semester - 1)/2)  %>%
    group_by(disease, disease_clean, country, country_iso3c, yr) %>%
    summarize(predicted_semester_outbreak_probability = max(predicted_outbreak_probability),
              endemic = any(endemic),
              outbreak_start = any(outbreak_start),
              outbreak_ongoing =any(outbreak_ongoing)) %>%
    ungroup()

  overall_status_predict <- network_boost_augment_predict  %>%
    filter(disease %in% !!diseases)  %>%
    filter(country_iso3c %in% !!country_iso3c) %>%
    left_join(outbreak_predict)
    # pivot_longer(cols = c("Predicted", "Reported"), names_to = "type") %>%
    # mutate(missing = factor(if_else(value, "Present", "Absent", "Missing"), levels = c("Missing", "Present", "Absent"))) %>%
    # select(-value)

  return(overall_status_predict)

}

