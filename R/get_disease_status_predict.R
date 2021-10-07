#' Get disease status and predicted outbreak prob by country
#' @param conn repeldata connection
#' @param country_iso3c iso3c(s) of countries
#' @param disease disease(s) name(s)
#' @return dataframe containing disease status (reported/predicted), reported endemic/outbreak status, predicted imported outbreak prob
#' @import dplyr tidyr
#' @importFrom readr read_csv
#' @export
get_disease_status_predict <- function(conn,
                                       country_iso3c,
                                       diseases = get_oie_high_importance_diseases()){

  outbreak_predict <- tbl(conn,  "network_lme_augment_predict") %>%
    filter(disease %in% !!diseases)  %>%
    filter(country_iso3c %in% !!country_iso3c) %>%
    select(-db_network_etag, -outbreak_subsequent_month, -id) %>%
    collect() %>%
    mutate(report_semester = ifelse(month(month) < 7, 1, 2)) %>%
    mutate(yr = year(month) + (report_semester - 1)/2)  %>%
    group_by(disease, country_iso3c, yr) %>%
    summarize(predicted_semester_outbreak_probability = max(predicted_outbreak_probability),
              endemic = any(endemic),
              outbreak_start = any(outbreak_start),
              outbreak_ongoing = any(outbreak_ongoing)) %>%
    ungroup()


  overall_status_predict <- tbl(conn, "nowcast_boost_augment_predict")  %>%
    filter(disease %in% !!diseases)  %>%
    filter(country_iso3c %in% !!country_iso3c) %>%
    select(country_iso3c, disease, report_year, report_semester,
           taxa, disease_population,
           disease_status_unreported, actual_disease_status = disease_status,
           predicted_cases) %>%
    collect() %>%
    mutate(yr = report_year + (report_semester - 1)/2)   %>%
    group_by(country_iso3c, disease, yr, report_year, report_semester) %>%
    summarize(disease_status_unreported = sum(disease_status_unreported) == 0,
              actual_disease_status = sum(actual_disease_status, na.rm = TRUE)>0,
              predicted_disease_status = sum(predicted_cases, na.rm = TRUE)>0
    ) %>%
    ungroup() %>%
    mutate(actual_disease_status = if_else(actual_disease_status, "present", "absent")) %>%
    mutate(actual_disease_status = if_else(disease_status_unreported, "unreported", actual_disease_status)) %>%
    mutate(predicted_disease_status = if_else(predicted_disease_status, "present", "absent")) %>%
    select(-disease_status_unreported) %>%
    left_join(outbreak_predict,  by = c("country_iso3c", "disease", "yr"))

  country_lookup <- readr::read_csv(system.file("lookup", "countrycode_lookup.csv", package = "repelpredict"),  col_types = cols(
    country = col_character(),
    country_iso3c = col_character()
  ))

  overall_status_predict <- overall_status_predict %>%
    left_join(country_lookup, by = "country_iso3c") %>%
    mutate(disease_clean = str_to_title(str_replace_all(disease, "_", " ")))

  return(overall_status_predict)

}
