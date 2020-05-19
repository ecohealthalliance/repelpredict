devtools::load_all()

# example data
conn <- repel_local_conn()
#repel_local_download()

# fit model
dat <- repel_cases_train(conn) %>%
  select("country_iso3c", "report_year", "report_semester", "disease", "taxa") %>%
  slice(sample(1:nrow(.), size = 150))

model_object <- nowcast_baseline_model(conn, dat)

# new data
newdata <- repel_cases_train(conn) %>%
  select("country_iso3c", "report_year", "report_semester", "disease", "taxa") %>%
  slice(sample(1:nrow(.), size = 150))


# define generic predict
repel_predict <- function(x){
  UseMethod("repel_predict")
}

# define method for nowcast_baseline predict
repel_predict.nowcast_baseline <- function(model_object, conn, newdata){
  model_object$predict_function(conn = conn, newdata = newdata) %>%
    mutate(predicted = cases_lag1) %>%
    select(-starts_with("cases"))
}

# predict on new data
newdata_predict <- repel_predict.nowcast_baseline(model_object, conn, newdata)

# define generic score
repel_score <- function(x){
  UseMethod("repel_score")
}

# define method for nowcast_baseline score
repel_score.nowcast_baseline <- function(model_object, conn, newdata){
  model_object$score_function(conn = conn, newdata = newdata) %>%
    mutate(predicted = as.numeric(cases_lag1)) %>%
    mutate(actual = as.numeric(cases)) %>%
    mutate(error = abs(actual - predicted)) %>%
    dplyr::select(-starts_with("cases"))

  # rmse?
}

# score new data
newdata_score <- repel_score.nowcast_baseline(model_object, conn, newdata)

