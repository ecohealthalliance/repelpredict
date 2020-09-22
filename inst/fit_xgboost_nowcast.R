devtools::load_all()
library(tidyverse)
library(tictoc)
library(dbarts)
library(ceterisParibus)
set.seed(101)
#repeldata::repel_local_download()
conn <- repeldata::repel_local_conn()

model_object <- nowcast_boost_model()

traindat <- repel_cases_train(conn) %>%
  select(all_of(grouping_vars)) %>%
  distinct()

# augmented_data <- repel_augment(model_object = model_object, conn = conn, newdata = traindat) %>%
#   arrange(country_iso3c, disease, taxa, report_year, report_semester)
# write_rds(augmented_data, "inst/augmented_data.rds")
augmented_data <- read_rds(here::here("inst/augmented_data.rds"))

tic("fit boost models")
repel_fit(model_object = model_object,
          augmented_data = augmented_data,
          output_directory = "models")
toc()











pred <- predict(boost_mod_disease_status, train_disease_status$data)
prediction <- as.numeric(pred > 0.5)
print(head(prediction))

err <- mean(as.numeric(pred > 0.5) != train_disease_status$label)
print(paste("test-error=", err))

importance <- xgb.importance(feature_names = colnames(train_disease_status$data), model = boost_mod_disease_status)
head(importance)

# Gain is the improvement in accuracy brought by a feature to the branches it is on. The idea is that before adding a new split on a feature X to the branch there was some wrongly classified elements, after adding the split on this feature, there are two new branches, and each of these branch is more accurate (one branch saying if your observation is on this branch then it should be classified as 1, and the other branch saying the exact opposite).
# Cover measures the relative quantity of observations concerned by a feature.
# Frequency is a simpler way to measure the Gain. It just counts the number of times a feature is used in all generated trees. You should not use it (unless you know why you want to use it).


# tic("predictions")
# predicted_cases <- repel_predict(model_object = model_object,
#                                  newdata = augmented_data)
# toc()
# write_rds(predicted_cases, "inst/predicted_cases.rds")

# predicted_cases <- read_rds("inst/predicted_cases.rds")
#
#
# scored_data <- repel_score(model_object = model_object,
#                            augmented_data = augmented_data[1:10000,],
#                            predicted_cases = predicted_cases)
#
#
# newdata <- tibble(country_iso3c = "AFG",
#                   report_year = rep(2016:2020, each = 2),
#                   report_semester = rep(1:2, 5),
#                   disease = "foot-and-mouth disease",
#                   disease_population = "domestic",
#                   taxa = "cattle")
#
# forecasted_data <- repel_forecast(model_object = model_object,
#                                  conn = conn,
#                                  newdata = newdata)
#
#
#
# newdata <- tibble(country_iso3c = "AFG",
#                  report_year = rep(2016:2020, each = 2),
#                  report_semester = rep(1:2, 5),
#                  disease = "african horse sickness",
#                  disease_population = "domestic",
#                  taxa = "equidae")
#
# forecasted_data <- repel_forecast(model_object = model_object,
#                                  conn = conn,
#                                  newdata = newdata)
#
# scored_new_data <- repel_score(model_object = model_object,
#                                augmented_data = forecasted_data$augmented_data,
#                                predicted_cases = forecasted_data$predicted_cases)
#
# repel_local_disconnect()

