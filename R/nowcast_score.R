# define generic score
repel_score <- function(x, ...){
  UseMethod("repel_score")
}


#' Score accuracy of nowcast baseline model object
#' @return list containing predicted count and whether disease is expected or not (T/F)
#'
repel_score.nowcast_baseline <- function(model_object, augmented_data) {

  cases_actual <- augmented_data$cases
  cases_presence_actual <-  !is.na(cases_actual) & cases_actual>0

  cases_predicted <- augmented_data$cases_lag1
  cases_presence_predicted <-  !is.na(cases_predicted) & cases_predicted>0

  cases_comp <- tibble(cases_actual, cases_predicted) %>%
    mutate(diff = abs(cases_actual - cases_predicted))

  cases_presence_comp <- tibble(cases_presence_actual, cases_presence_predicted) %>%
    mutate(match = cases_presence_actual == cases_presence_predicted)

  list(cases_comp,
       cases_score = median(cases_comp$diff, na.rm = TRUE),
       cases_presence_comp,
       cases_presence_score = sum(cases_presence_comp$match)/nrow(cases_presence_comp)
       )

}
