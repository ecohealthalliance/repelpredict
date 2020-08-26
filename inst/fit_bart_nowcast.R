devtools::load_all()
library(tidyverse)
library(tictoc)
library(dbarts)
library(ceterisParibus)

#repeldata::repel_local_download()
conn <- repeldata::repel_local_conn()

#TODO for lags, fill from latest available year, otherwise 0

model_object <- nowcast_bart_model()

traindat <- repel_cases_train(conn) %>%
  select(all_of(grouping_vars)) %>%
  distinct()

augmented_data <- repel_augment(model_object = model_object, conn = conn, newdata = traindat) %>%
  arrange(country_iso3c, disease, taxa, report_year, report_semester)
map(augmented_data, ~any(is.na(.)))
write_rds(augmented_data, "inst/augmented_data.rds")

augmented_data <- read_rds("inst/augmented_data.rds")

tic("cases model")
fit_object <- repel_fit(model_object = model_object,
                        augmented_data = augmented_data,
                        outcome_var = "cases",
                        output_directory = here::here("models"))
toc()

tic("status model")
repel_fit(model_object = model_object,
          augmented_data = augmented_data,
          outcome_var = "disease_status",
          output_directory = here::here("models"))
toc()

# predicted_data <- repel_predict(model_object = model_object,
#                                 newdata = augmented_data[1:10000,])
#
# scored_data <- repel_score(model_object = model_object,
#                            augmented_data = augmented_data[1:10000,],
#                            predicted_data = predicted_data)
#
# newdata <- tibble(country_iso3c = "AFG",
#                   report_year = rep(2016:2020, each = 2),
#                   report_semester = rep(1:2, 5),
#                   disease = "foot-and-mouth disease",
#                   disease_population = "domestic",
#                   taxa = "cattle")
#
# forcasted_data <- repel_forecast(model_object = model_object,
#                                  conn = conn,
#                                  newdata = newdata)
# scored_new_data <- repel_score(model_object = model_object,
#                                augmented_data = forcasted_data$augmented_data,
#                                predicted_data = forcasted_data$predicted_data)
#
#
# newdata <- tibble(country_iso3c = "AFG",
#                  report_year = rep(2016:2020, each = 2),
#                  report_semester = rep(1:2, 5),
#                  disease = "african horse sickness",
#                  disease_population = "domestic",
#                  taxa = "equidae")
#
# forcasted_data <- repel_forecast(model_object = model_object,
#                                  conn = conn,
#                                  newdata = newdata)
# scored_new_data <- repel_score(model_object = model_object,
#                                augmented_data = forcasted_data$augmented_data,
#                                predicted_data = forcasted_data$predicted_data)


repel_local_disconnect()


