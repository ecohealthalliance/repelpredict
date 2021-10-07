devtools::load_all()

# Connection and create model object  -------------------------------------------------------------------
conn <- repeldata::repel_remote_conn()
model_object <-  nowcast_boost_model(disease_status_model = NULL, cases_model = NULL)

# Preprocess and augment -----------------------------------------------------------------
# this mirrors what would happen in repel infra using expanded six_month_processed.rds from repel_init()
# six_month_processed <- read_rds("six_month_processed.rds")
# augmented_data <- repel_augment(model_object = model_object,
#                                 conn = conn,
#                                 subset = NULL,
#                                 six_month_processed = six_month_processed) %>%
#    arrange(country_iso3c, disease, taxa, report_year, report_semester)

# for model fitting
valdat <- repel_validation(model_object, conn)
traindat <- repel_training(model_object, conn)
map(traindat, ~any(is.na(.)))

augmented_data <- repel_augment(model_object = model_object,
                                conn = conn,
                                subset = traindat,
                                six_month_processed = NULL) %>%
  arrange(country_iso3c, disease, taxa, report_year, report_semester)
assertthat::are_equal(nrow(janitor::get_dupes(augmented_data, all_of(grouping_vars))), 0)

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
forecasted_xgboost <-repel_forecast(model_object = model_object,
                                    conn = conn,
                                    subset = valdat,
                                    six_month_processed = NULL)

# Baseline ----------------------------------------------------------------
model_object <-  nowcast_baseline_model()
augmented_baseline <- repel_augment(model_object = model_object,
                                    conn = conn,
                                    subset = traindat,
                                    six_month_processed = NULL)

predicted_baseline <- repel_predict(model_object, newdata = augmented_baseline)

forecasted_baseline <-repel_forecast(model_object = model_object,
                                     conn = conn,
                                     subset = traindat,
                                     six_month_processed = NULL)

# Disconnect DB -----------------------------------------------------------
repel_remote_disconnect()

