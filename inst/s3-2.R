nowcast_baseline_model <- function(){
  structure(list(description = "Nowcast baseline model",
                 class = c("nowcast_baseline", "nowcast_model")))
}

forecast.nowcast_model <- function(model_object, conn, newdata) {
  augmented_data <- augment(model_object, conn, newdata)
  predictions <- predict(model_object, augmented_data)
  returns(predictions)
}

augment.nowcast_baseline <- function(model_object, conn, newdata) {
  augmented_data <- left_join(newdata,
                              yadayada)
  return(augmented_data)
}

predict.nowcast_baseline(model_object, augmented_data) {
  list(
    predictions = augmented_data$lag1,
    na_values = NULL,
    se = NULL
  )
}

nowcast_bart_model <- function(bart_file) {
  bart_model <- readRDS(bart_file)
  structure(list(description = "Nowcast BART model",
                 model_object = bart_model,
                 class = c("nowcast_bart", "nowcast_model")))
}

augment.nowcast_bart <- function(bart_model, conn, newdata) {

}

predict.nowcast_bart <- function(bart_model, augmented_data) {
  assert_that(inherits(bart_model$model_object, "bart"))
  predictions <- dbarts:::predict.bart(bart_model$model_object, augmented_data)
}
