#' define generic score
#' @export
repel_score <- function(x, ...){
  UseMethod("repel_score")
}


#' Score accuracy of nowcast baseline or bart model object
#' @return list containing predicted count and whether disease is expected or not (T/F)
#' @export
#'
repel_score.nowcast_model <- function(model_object, augmented_data, predicted_data) {

  cases_predicted <- predicted_data$predicted_cases
  disease_status_predicted <-  predicted_data$predicted_disease_status

  # only score for non-na actual cases
  cases_comp <- augmented_data %>%
    select(all_of(grouping_vars), cases_actual = cases, cases_lag1) %>%
    mutate(cases_predicted = cases_predicted) %>%
    mutate(cases_error = abs(cases_actual - cases_predicted)) %>%
    mutate(cases_dirchange_actual = cases_actual - cases_lag1) %>%
    mutate(cases_dirchange_predicted = cases_predicted - cases_lag1) %>%
    mutate_at(.vars = c("cases_dirchange_actual", "cases_dirchange_predicted"),
              ~case_when(. == 0 ~ "no change",
                         . > 0 ~ "increase",
                         . < 0 ~ "decrease")) %>%
    mutate(cases_dirchange_match = cases_dirchange_actual == cases_dirchange_predicted) %>%
    drop_na(cases_actual)

  cases_error <-  cases_comp %>%
    pull(cases_error)

  # disease status for all cases
  disease_status_comp <- augmented_data %>%
    select(all_of(grouping_vars), disease_status_actual = disease_status, disease_status_lag1) %>%
    mutate(disease_status_predicted = disease_status_predicted) %>%
    mutate(disease_status_match = disease_status_actual == disease_status_predicted) %>%
    mutate(disease_status_switch_actual = disease_status_actual - disease_status_lag1) %>%
    mutate(disease_status_switch_predicted = disease_status_predicted - disease_status_lag1) %>%
    mutate_at(.vars = c("disease_status_switch_actual", "disease_status_switch_predicted"), ~recode(.,
                                                                                                    `1` = "new presence",
                                                                                                    `0` = "no switch",
                                                                                                    `-1` = "eliminated")) %>%
    mutate(disease_status_switch_match = disease_status_switch_actual == disease_status_switch_predicted) %>%
    drop_na(disease_status_actual)


  list(
    # case count model
    cases_tibble = cases_comp %>%
      select(all_of(grouping_vars), cases_actual, cases_predicted, cases_error),
    cases_direction_confusion_matrix = table(cases_comp %>% select(cases_dirchange_actual, cases_dirchange_predicted)), #this automatically removes NA
    cases_direction_prediction_accuracy = sum(cases_comp$cases_dirchange_match)/nrow(cases_comp %>% drop_na(cases_dirchange_match)),
    mean_abs_error = mean(cases_error),
    p95_abs_error = quantile(cases_error, 0.95),
    # binary disease status model
    disease_status_tibble = disease_status_comp %>%
      select(all_of(grouping_vars), disease_status_actual, disease_status_predicted, disease_status_match),
    disease_status_confusion_matrix = table(disease_status_comp %>% select(disease_status_actual, disease_status_predicted)),
    disease_status_prediction_accuracy = sum(disease_status_comp$disease_status_match)/nrow(disease_status_comp),
    disease_status_switch_confusion_matrix = table(disease_status_comp %>% select(disease_status_switch_actual, disease_status_switch_predicted)),
    disease_status_switch_accuracy = sum(disease_status_comp$disease_status_switch_match)/nrow(disease_status_comp)
  )

}
