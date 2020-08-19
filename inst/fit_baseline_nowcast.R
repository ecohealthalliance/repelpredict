devtools::load_all()
library(tidyverse)

#repeldata::repel_local_download()
conn <- repeldata::repel_local_conn()

# Baseline ----------------------------------------------------------------
#TODO fill lag as far back as need to, otherwise 0

model_object <- nowcast_baseline_model()

traindat <- repel_cases_train(conn) %>%
  select(all_of(grouping_vars)) %>%
  distinct()

augmented_data <- repel_augment(model_object = model_object, conn = conn, dat = traindat)
#assertthat::are_equal(nrow(traindat), nrow(augmented_data))
# ^ there are some dupes in cases where both present and suspected have counts
map(augmented_data, ~any(is.na(.))) # can be in cases and disease status only

predicted_data <- repel_predict(model_object = model_object, newdata = augmented_data)

scored_data <- repel_score(model_object = model_object, augmented_data = augmented_data, predicted_data = predicted_data)

# try new cases
newdata <- tibble(country_iso3c = "AFG",
                  report_year = rep(2016:2020, each = 2),
                  report_semester = rep(1:2, 5),
                  disease = "foot-and-mouth disease",
                  disease_population = "domestic",
                  taxa = "cattle")

forcasted_data <- repel_forecast(model_object = model_object,
                                 conn = conn,
                                 newdata = newdata)
scored_new_data <- repel_score(model_object = model_object,
                               augmented_data = forcasted_data$augmented_data,
                               predicted_data = forcasted_data$predicted_data)

newdat <- tibble(country_iso3c = "AFG",
                 report_year = rep(2016:2020, each = 2),
                 report_semester = rep(1:2, 5),
                 disease = "african horse sickness",
                 disease_population = "domestic",
                 taxa = "equidae")

forcasted_data <- repel_forecast(model_object = model_object, conn = conn, newdata = newdat)
scored_new_data <- repel_score(model_object = model_object,  augmented_data = forcasted_data$augmented_data, predicted_data = forcasted_data$predicted_data)
