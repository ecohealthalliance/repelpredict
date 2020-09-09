devtools::load_all()
library(tidyverse)
library(tictoc)
library(dbarts)
library(ceterisParibus)
set.seed(101)
#repeldata::repel_local_download()
conn <- repeldata::repel_local_conn()

model_object <- nowcast_bart_model()

traindat <- repel_cases_train(conn) %>%
  select(all_of(grouping_vars)) %>%
  distinct()

augmented_data <- repel_augment(model_object = model_object, conn = conn, newdata = traindat) %>%
  arrange(country_iso3c, disease, taxa, report_year, report_semester)
map(augmented_data, ~any(is.na(.)))
write_rds(augmented_data, "inst/augmented_data.rds")

augmented_data <- read_rds("inst/augmented_data.rds")[sample(425675, 100000, replace = F),]


tic("fit bart models")
repel_fit(model_object = model_object,
          augmented_data = augmented_data,
          output_directory = here::here("models"))
toc()

tic("predictions")
predicted_cases <- repel_predict(model_object = model_object,
                                newdata = augmented_data[1:10000,])
toc()
write_rds(predicted_cases, "inst/predicted_cases.rds")

predicted_cases <- read_rds("inst/predicted_cases.rds")


scored_data <- repel_score(model_object = model_object,
                           augmented_data = augmented_data[1:10000,],
                           predicted_cases = predicted_cases)


newdata <- tibble(country_iso3c = "AFG",
                  report_year = rep(2016:2020, each = 2),
                  report_semester = rep(1:2, 5),
                  disease = "foot-and-mouth disease",
                  disease_population = "domestic",
                  taxa = "cattle")

forecasted_data <- repel_forecast(model_object = model_object,
                                 conn = conn,
                                 newdata = newdata)



newdata <- tibble(country_iso3c = "AFG",
                 report_year = rep(2016:2020, each = 2),
                 report_semester = rep(1:2, 5),
                 disease = "african horse sickness",
                 disease_population = "domestic",
                 taxa = "equidae")

forecasted_data <- repel_forecast(model_object = model_object,
                                 conn = conn,
                                 newdata = newdata)

scored_new_data <- repel_score(model_object = model_object,
                               augmented_data = forecasted_data$augmented_data,
                               predicted_cases = forecasted_data$predicted_cases)

repel_local_disconnect()

