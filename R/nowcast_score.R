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
  cases_comp <- augmented_data %>%
    select(all_of(grouping_vars), disease_status_actual = disease_status, cases_actual = cases, cases_lag1) %>%
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
    mutate(disease_status_predicted = as.integer(cases_predicted > 0)) %>%
    mutate(disease_status_match = disease_status_actual == disease_status_predicted)

  list(
    cases_tibble = cases_comp,
    # just disease status
    disease_status_confusion_matrix = table(cases_comp %>% select(disease_status_actual, disease_status_predicted)),
    disease_status_prediction_accuracy = sum(cases_comp$disease_status_match, na.rm = TRUE)/nrow(cases_comp %>% drop_na(disease_status_match)),
    # case counts
    cases_direction_confusion_matrix = table(cases_comp %>% select(cases_dirchange_actual, cases_dirchange_predicted)), #this automatically removes NA
    cases_direction_prediction_accuracy = sum(cases_comp$cases_dirchange_match, na.rm = TRUE)/nrow(cases_comp %>% drop_na(cases_dirchange_match)),
    mean_abs_error = mean(cases_comp$cases_error, na.rm = TRUE),
    p95_abs_error = quantile(cases_comp$cases_error, 0.95, na.rm = TRUE)
  )

}
