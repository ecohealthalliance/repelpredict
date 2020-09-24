#' define generic score
#' @export
repel_score <- function(x, ...){
  UseMethod("repel_score")
}


#' Score accuracy of nowcast baseline model object
#' @return list containing predicted count and whether disease is expected or not (T/F)
#' @export
#'
repel_score.nowcast_model <- function(model_object, augmented_data, predicted_cases) {

  # only score for non-na actual cases
  scored_tibble <- augmented_data %>%
    select(all_of(grouping_vars), disease_status_actual = disease_status, cases_actual = cases, cases_lag1, disease_status_lag1) %>%
    # cases model
    mutate(cases_predicted = predicted_cases) %>%
    mutate(cases_error = abs(cases_actual - cases_predicted)) %>%
    mutate(cases_dirchange_actual = cases_actual - cases_lag1) %>%
    mutate(cases_dirchange_predicted = cases_predicted - cases_lag1) %>%
    mutate_at(.vars = c("cases_dirchange_actual", "cases_dirchange_predicted"),
              ~case_when(. == 0 ~ "no change",
                         . > 0 ~ "increase",
                         . < 0 ~ "decrease",
                         is.na(.) ~ NA_character_)) %>%
    mutate(cases_dirchange_match = cases_dirchange_actual == cases_dirchange_predicted) %>%
    # disease status model
    mutate(disease_status_predicted = as.integer(cases_predicted > 0)) %>%
    mutate(disease_status_match = disease_status_actual == disease_status_predicted) %>%
    mutate(disease_status_dirchange_actual = disease_status_actual - disease_status_lag1) %>%
    mutate(disease_status_dirchange_predicted = disease_status_predicted - disease_status_lag1) %>%
    mutate_at(.vars = c("disease_status_dirchange_actual", "disease_status_dirchange_predicted"),
              ~case_when(. == 0 ~ "no change",
                         . > 0 ~ "outbreak starts",
                         . < 0 ~ "outbreak ends",
                         is.na(.) ~ NA_character_))

  list(
    scored_tibble = scored_tibble#,
    # # just disease status
    # disease_status_confusion_matrix = table(scored_tibble %>% select(disease_status_actual, disease_status_predicted)),
    # disease_status_prediction_accuracy = sum(scored_tibble$disease_status_match, na.rm = TRUE)/nrow(scored_tibble %>% drop_na(disease_status_match)),
    # # case counts
    # cases_direction_confusion_matrix = table(scored_tibble %>% select(cases_dirchange_actual, cases_dirchange_predicted)), #this automatically removes NA
    # cases_direction_prediction_accuracy = sum(scored_tibble$cases_dirchange_match, na.rm = TRUE)/nrow(scored_tibble %>% drop_na(cases_dirchange_match)),
    # mean_abs_error = mean(scored_tibble$cases_error, na.rm = TRUE),
    # p95_abs_error = quantile(scored_tibble$cases_error, 0.95, na.rm = TRUE)
  )

}
