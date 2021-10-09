devtools::load_all()

# Connection and create model object  -------------------------------------------------------------------
conn <- repeldata::repel_remote_conn()
model_object <-  nowcast_boost_model(disease_status_model = NULL, cases_model = NULL)

# Preprocess and augment -----------------------------------------------------------------

# for model fitting
# valdat <- repel_validation(model_object, conn)
# traindat <- repel_training(model_object, conn)
# map(traindat, ~any(is.na(.)))
# write_rds(traindat, here::here("tmp/nowcast_traindat.rds"))
traindat <- read_rds(here::here("tmp/nowcast_traindat.rds"))

augmented_data <- repel_augment(model_object = model_object,
                                conn = conn,
                                subset = traindat) %>%
  arrange(country_iso3c, disease, taxa, report_year, report_semester)
assertthat::are_equal(nrow(janitor::get_dupes(augmented_data, all_of(grouping_vars))), 0)

# test = map_lgl(augmented_data, ~any(is.na(.)))
# test[test]

write_rds(augmented_data, "tmp/augmented_data.rds")
augmented_data <- read_rds("tmp/augmented_data.rds")

# Fitting -----------------------------------------------------------------
# fitting takes about a day for these two models on prospero
repel_fit(model_object =  model_object,
          augmented_data = augmented_data,
          model = "disease_status",
          output_directory = "models")

repel_fit(model_object = model_object,
          augmented_data = augmented_data,
          model = "cases",
          output_directory = "models")

# Predict/Forecast example --------------------------------------------------------
  model_object <-  nowcast_boost_model(
    disease_status_model = aws.s3::s3readRDS(bucket = "repeldb/models", object = "boost_mod_disease_status.rds"),
    cases_model = aws.s3::s3readRDS(bucket = "repeldb/models", object = "boost_mod_cases.rds")
  )

# full expanded dataset
augmented_data <- repel_augment(model_object = model_object,
                                conn = conn,
                                subset = NULL) %>%
  arrange(country_iso3c, disease, taxa, report_year, report_semester)

#map(disease_status_model$pre$actions$recipe$recipe$template, ~any(is.na(.))) # NA only in removed fields

predict_xgboost <-repel_predict(model_object = model_object,
                                    newdata = augmented_data)
forecasted_xgboost <-repel_forecast(model_object = model_object,
                                    conn = conn,
                                    subset = traindat)

# Baseline ----------------------------------------------------------------
model_object <-  nowcast_baseline_model()
augmented_baseline <- repel_augment(model_object = model_object,
                                    conn = conn,
                                    subset = traindat)

predicted_baseline <- repel_predict(model_object, newdata = augmented_baseline)

forecasted_baseline <-repel_forecast(model_object = model_object,
                                     conn = conn,
                                     subset = traindat)

# Disconnect DB -----------------------------------------------------------
repel_remote_disconnect()

