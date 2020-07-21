devtools::load_all()
library(tidyverse)
library(tictoc)
library(dbarts)
library(ceterisParibus)

#repeldata::repel_local_download()
conn <- repeldata::repel_local_conn()

# Notes on NA handling
#TODO for lags, fill from latest available year, otherwise 0

model_object <- nowcast_bart_model()

traindat <- repel_cases_train(conn) %>%
  select(all_of(grouping_vars)) %>%
  distinct()

# augmented_data <- repel_augment(model_object = model_object, conn = conn, traindat = traindat) %>%
#   arrange(country_iso3c, disease, taxa, report_year, report_semester)
# map(augmented_data, ~any(is.na(.)))
#
# augmented_data_sub <- augmented_data %>%
#   filter(country_iso3c %in% c("USA", "BEL"))
# write_rds(augmented_data_sub, "inst/test_augmented_data.rds")

augmented_data <- read_rds("inst/test_augmented_data.rds") %>%
  filter(country_iso3c %in% c("USA", "BEL"))

tic("status model")
repel_fit(model_object = model_object,
          augmented_data = augmented_data,
          outcome_var = "disease_status",
          output_directory = here::here("models"))
toc()
tic("cases model")
repel_fit(model_object = model_object,
          augmented_data = augmented_data,
          outcome_var = "cases",
          output_directory = here::here("models"))
toc()

predicted_data <- repel_predict(model_object = model_object, augmented_data = augmented_data_sub)
#
# scored_data <- repel_score(model_object = model_object, augmented_data = augmented_data, predicted_data = predicted_data)

repel_local_disconnect()


