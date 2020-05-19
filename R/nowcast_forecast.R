# define generic forecast
repel_forecast <- function(x){
  UseMethod("repel_forecast")
}

forecast.nowcast_model <- function(model_object, conn, newdata) {
  augmented_data <- repel_augment(model_object, conn, newdata)
  predictions <- repel_predict(model_object, augmented_data)
  return(predictions)
}
