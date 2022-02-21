#' define generic score
#' @export
repel_score <- function(x, ...){
  UseMethod("repel_score")
}


#' Score accuracy of nowcast model object
#' @return list containing predicted count and whether disease is expected or not (T/F)
#' @export
#'
repel_score.nowcast_model <- function(model_object, predicted_data) {

  # only score for non-na actual cases
  predicted_data %>%
    select(all_of(grouping_vars), disease_status_actual = disease_status, cases_actual = cases, cases_lag1, disease_status_lag1, disease_status_predicted, cases_predicted) %>%
     # cases model
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
    mutate(disease_status_match = disease_status_actual == disease_status_predicted) %>%
    mutate(disease_status_dirchange_actual = disease_status_actual - disease_status_lag1) %>%
    mutate(disease_status_dirchange_predicted = disease_status_predicted - disease_status_lag1) %>%
    mutate_at(.vars = c("disease_status_dirchange_actual", "disease_status_dirchange_predicted"),
              ~case_when(. == 0 ~ "no change",
                         . > 0 ~ "outbreak starts",
                         . < 0 ~ "outbreak ends",
                         is.na(.) ~ NA_character_))

}
