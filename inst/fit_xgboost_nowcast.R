devtools::load_all()
# conn <- repeldata::repel_remote_conn()
model_object <-  nowcast_boost_model(disease_status_model = NULL, cases_model = NULL)

# Fitting  ----------------------------------------------------------------
# traindat <- repel_training(model_object, conn) %>%
#   select(all_of(grouping_vars)) %>%
#   distinct()
# map(traindat, ~any(is.na(.)))
#
# augmented_data <- repel_augment(model_object = model_object,
#                                 conn = conn, newdata = traindat) %>%
#    arrange(country_iso3c, disease, taxa, report_year, report_semester)
# write_rds(augmented_data, "tmp/augmented_data.rds")
augmented_data <- read_rds(here::here("tmp/augmented_data.rds"))

assertthat::are_equal(nrow(janitor::get_dupes(augmented_data, all_of(grouping_vars))), 0)

# fitting takes about a day for these two models on prospero
repel_fit(model_object =  model_object,
          augmented_data = augmented_data,
          model = "disease_status",
          output_directory = "models")

repel_fit(model_object = model_object,
          augmented_data = augmented_data,
          model = "cases",
          output_directory = "models")


# Forecast on training ----------------------------------------------------
# model_object <-  nowcast_boost_model(
#   disease_status_model = aws.s3::s3readRDS(bucket = "repeldb/models", object = "boost_mod_disease_status.rds"),
#   cases_model = aws.s3::s3readRDS(bucket = "repeldb/models", object = "boost_mod_cases.rds"))
#
# tic()
# forecasted_data <- repel_forecast(model_object = model_object,
#                                   conn = conn,
#                                   newdata = traindat,
#                                   use_cache = FALSE)
# toc()
#
# scored_new_data <- repel_score(model_object = model_object,
#                                augmented_data = forecasted_data$augmented_data,
#                                predicted_cases = forecasted_data$predicted_cases)

# Forecast on validate ----------------------------------------------------
# valdat <- repel_validation(model_object, conn) %>%
#   select(all_of(grouping_vars)) %>%
#   distinct()
#
# tic()
# forecasted_data <- repel_forecast(model_object = model_object,
#                                   conn = conn,
#                                   newdata = valdat,
#                                   use_cache = FALSE)
# toc()
#
# scored_new_data <- repel_score(model_object = model_object,
#                                augmented_data = forecasted_data$augmented_data,
#                                predicted_cases = forecasted_data$predicted_cases)
#
#
# # Forecast on new data ----------------------------------------------------
#
# # example 1
# newdata <- tibble(country_iso3c = "AFG",
#                   report_year = rep(2016:2019, each = 2),
#                   report_semester = rep(1:2, 4),
#                   disease = "foot_and_mouth_disease",
#                   disease_population = "domestic",
#                   taxa = "cattle")
#
# tic()
# forecasted_data <- repel_forecast(model_object = model_object,
#                                   conn = conn,
#                                   newdata = newdata)
# toc()
#
# scored_new_data <- repel_score(model_object = model_object,
#                                augmented_data = forecasted_data$augmented_data,
#                                predicted_cases = forecasted_data$predicted_cases)
#
# # example 2
# newdata <- tibble(country_iso3c = "AFG",
#                   report_year = rep(2016:2020, each = 2),
#                   report_semester = rep(1:2, 5),
#                   disease = "african horse sickness",
#                   disease_population = "domestic",
#                   taxa = "equidae")
#
# tic()
# forecasted_data <- repel_forecast(model_object = model_object,
#                                   conn = conn,
#                                   newdata = newdata)
# toc()
#
# scored_new_data <- repel_score(model_object = model_object,
#                                augmented_data = forecasted_data$augmented_data,
#                                predicted_cases = forecasted_data$predicted_cases)

# repel_local_disconnect()

