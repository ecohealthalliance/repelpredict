# define generic forecast
repel_forecast <- function(x){
  UseMethod("repel_forecast")
}

forecast.nowcast_model <- function(model_object, conn, newdata) {
  augmented_data <- augment(model_object, conn, newdata)
  predictions <- predict(model_object, augmented_data)
  returns(predictions)
}
