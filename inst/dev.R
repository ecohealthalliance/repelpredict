devtools::load_all()
library(tidyverse)
library(tictoc)
library(dbarts)
library(ceterisParibus)

#repeldata::repel_local_download()
conn <- repeldata::repel_local_conn()

# Baseline ----------------------------------------------------------------
model_object <- nowcast_baseline_model()

traindat <- repel_cases_train(conn) %>%
  select(all_of(grouping_vars)) %>%
  distinct()

augmented_data <- repel_augment(model_object = model_object, conn = conn, traindat = traindat)
#assertthat::are_equal(nrow(traindat), nrow(augmented_data))
# ^ there are some dupes in cases where both present and suspected have counts
map(augmented_data, ~any(is.na(.)))

predicted_data <- repel_predict(model_object = model_object, augmented_data = augmented_data)

scored_data <- repel_score(model_object = model_object, augmented_data = augmented_data, predicted_data = predicted_data)

# try new cases
newdat <- tibble(country_iso3c = "AFG",
                 report_year = 2020,
                 report_semester = 1,
                 disease = "foot-and-mouth disease",
                 disease_population = "domestic",
                 taxa = "cattle")

forcasted_data <- repel_forecast(model_object = model_object, conn = conn, newdata = newdat)
scored_new_data <- repel_score(model_object = model_object,  augmented_data = forcasted_data$augmented_data, predicted_data = forcasted_data$predicted_data)

# BART models --------------------------------------------------------------------
model_object <- nowcast_bart_model()

traindat <- repel_cases_train(conn) %>%
  select(all_of(grouping_vars)) %>%
  distinct()

augmented_data <- repel_augment(model_object = model_object, conn = conn, traindat = traindat) %>%
  arrange(country_iso3c, disease, taxa, report_year, report_semester)
map(augmented_data, ~any(is.na(.)))

repel_fit(model_object = model_object,
          augmented_data = augmented_data,
          outcome_var = "disease_status",
          output_directory = here::here("models"))

repel_fit(model_object = model_object,
          augmented_data = augmented_data,
          outcome_var = "cases",
          output_directory = here::here("models"))

predicted_data <- repel_predict(model_object = model_object, augmented_data = augmented_data)

scored_data <- repel_score(model_object = model_object, augmented_data = augmented_data, predicted_data = predicted_data)

repel_local_disconnect()


