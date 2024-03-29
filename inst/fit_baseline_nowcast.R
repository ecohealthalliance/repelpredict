devtools::load_all()
library(tidyverse)

#repeldata::repel_local_download()
conn <- repeldata::repel_remote_conn()

model_object <- nowcast_baseline_model()

traindat <- repel_training(model_object, conn) %>%
  select(all_of(grouping_vars)) %>%
  distinct()

augmented_data <- repel_augment(model_object = model_object, conn = conn, subset = traindat)
#assertthat::are_equal(nrow(traindat), nrow(augmented_data))
# ^ there are some dupes in cases where both present and suspected have counts
map(augmented_data, ~any(is.na(.))) # okay in disease_status_lag1_unreported (because these are missing reports)
# also okay in cases - often status is reported but not cases. we make assumptions around this for lags, but it's not necessary for current cases in baseline

predicted_data <- repel_predict(model_object = model_object, newdata = augmented_data)

scored_data <- repel_score(model_object = model_object, predicted_data = predicted_data)

# try new cases
newdata <- tibble(country_iso3c = "AFG",
                  report_year = rep(2016:2020, each = 2),
                  report_semester = rep(1:2, 5),
                  disease = "foot_and_mouth_disease",
                  disease_population = "domestic",
                  taxa = "cattle")

predicted_data <- repel_forecast(model_object = model_object,
                                 conn = conn,
                                 subset = newdata)

scored_new_data <- repel_score(model_object = model_object,
                               predicted_data = predicted_data)

newdata <- tibble(country_iso3c = "AFG",
                 report_year = rep(2016:2020, each = 2),
                 report_semester = rep(1:2, 5),
                 disease = "african horse sickness",
                 disease_population = "domestic",
                 taxa = "equidae")

forecasted_data <- repel_forecast(model_object = model_object, conn = conn, newdata = newdata)
scored_new_data <- repel_score(model_object = model_object,  augmented_data = forecasted_data$augmented_data, predicted_data = forecasted_data$predicted_data)

repel_local_disconnect()

