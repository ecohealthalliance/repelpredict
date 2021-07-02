#' Get cached nowcast model augmented data and model predictions
#' @param conn repeldata connection
#' @return dataframe containing full dataset and predictions
#' @import dplyr tidyr repeldata DBI stringr
#' @importFrom countrycode countrycode
#' @export
get_cached_nowcast_predictions <- function(conn){

  nowcast_boost_augment_predict <- dbReadTable(conn, name = "nowcast_boost_predict_oie_diseases")

  country_lookup <- tibble(country_iso3c = unique(nowcast_boost_augment_predict$country_iso3c)) %>%
    mutate(country = countrycode::countrycode(country_iso3c, origin = "iso3c", destination = "country.name"))

  disease_lookup <- tibble(disease = unique(nowcast_boost_augment_predict$disease)) %>%
    mutate(disease_clean = str_to_title(str_replace_all(disease, "_", " ")))

  nowcast_boost_augment_predict <- left_join(nowcast_boost_augment_predict, country_lookup) %>%
    left_join(disease_lookup) %>%
    # mutate(status_coalesced = factor(status_coalesced,
    #                                  levels = c("reported present",
    #                                             "unreported, predicted present",
    #                                             "reported absent", "unreported, predicted absent"))) %>%
    mutate(yr = report_year + (report_semester - 1)/2) %>%
    mutate(cases_coalesced = coalesce(actual_cases, predicted_cases)) %>%
    mutate(label = paste0("Reported cases = ", replace_na(actual_cases, "missing"), "<br/>Predicted cases = ", predicted_cases)) %>%
    arrange(disease, country_iso3c) %>%
    group_by(country_iso3c, disease) %>%
    mutate(status_coalesced = if_else(rep(all(predicted_cases == 0) & all(cases_coalesced == 0), n()),
                                      rep("never reported or predicted", n()),
                                      status_coalesced)) %>%
    ungroup() %>%
    as_tibble()

  return(nowcast_boost_augment_predict)

}
