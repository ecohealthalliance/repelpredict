# define generic score
repel_score <- function(x, ...){
  UseMethod("repel_score")
}


#' Score accuracy of nowcast baseline model object
#' @return list containing predicted count and whether disease is expected or not (T/F)
#'
repel_score.nowcast_baseline <- function(model_object, augmented_data, predicted_data) {

  cases_actual <- augmented_data$cases
  disease_status_actual <-  augmented_data$disease_status

  cases_predicted <- predicted_data$predicted_cases
  disease_status_predicted <-  predicted_data$predicted_disease_status

  cases_comp <- augmented_data %>%
    select(all_of(grouping_vars)) %>%
    mutate(cases_actual = cases_actual) %>%
    mutate(cases_predicted = cases_predicted) %>%
    mutate(diff = abs(cases_actual - cases_predicted))

  disease_status_comp <- augmented_data %>%
    select(all_of(grouping_vars), disease_status_lag1) %>%
    mutate(disease_status_lag1 = replace_na(disease_status_lag1, 0)) %>%
    mutate(disease_status_actual = disease_status_actual) %>%
    mutate(disease_status_change = disease_status_actual - disease_status_lag1) %>%
    #mutate(disease_status_change = recode(disease_status_change, `1` = "new presence"))
    mutate(disease_status_predicted = disease_status_predicted) %>%
    mutate(disease_status_match = disease_status_actual == disease_status_predicted)

  list(cases_comp,
       cases_score = median(cases_comp$diff, na.rm = TRUE),
       disease_status_comp,
       cases_presence_confusion_matrix = table(disease_status_comp %>% select(disease_status_actual, disease_status_predicted)),
       cases_presence_score = sum(disease_status_comp$match)/nrow(disease_status_comp)
       )

}
