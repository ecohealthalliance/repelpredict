library(tidyverse)
library(repeldata)

# example data
conn <- repel_remote_conn()

dat <- repel_cases_train(conn) %>%
  select("country_iso3c", "report_year", "report_semester", "disease", "taxa") %>%
  slice(sample(1:nrow(.), size = 150))

model_object <- nowcast_baseline_model(conn, dat)

newdata <- repel_cases_train(conn) %>%
  select("country_iso3c", "report_year", "report_semester", "disease", "taxa") %>%
  slice(sample(1:nrow(.), size = 150))

# define generic
repel_predict <- function(x){
  UseMethod("repel_predict")
}

# define method for nowcast_baseline predict
#TODO ??? is this right
repel_predict.nowcast_baseline <- function(model_object, conn, newdata){

    model_object$predict_function(conn = conn, newdata = newdata)

}

repel_predict.nowcast_baseline(model_object, conn, newdata)

predict()
score()








# s3 methods


repel_predict.nowcast_baseline <- function(model_object, newdata, conn){

  #nowcast_baseline_augment()

  model$predict_function(newdata)
}


nowcast_baseline_obj <- structure(list(predict_function = function(df) {return(list(predictions = NA))}),
                                  class = c("nowcast_baseline", "nowcast_model"))

augment

repel_predict.nowcast_baseline <- function(model, newdata, conn){

  #nowcast_baseline_augment()

  model$predict_function(newdata)
}

repel_predict(nowcast_baseline_obj)
