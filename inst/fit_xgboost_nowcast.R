devtools::load_all()
# repeldata::repel_local_download()
conn <- repeldata::repel_local_conn()

traindat <- annual_reports_animal_hosts_training(conn) %>%
  select(all_of(grouping_vars)) %>%
  distinct()

augmented_data <- repel_augment(model_object = nowcast_boost_model(disease_status_model = NULL,
                                                                   cases_model = NULL),
                                conn = conn, newdata = traindat) %>%
   arrange(country_iso3c, disease, taxa, report_year, report_semester)
write_rds(augmented_data, "tmp/augmented_data.rds")
augmented_data <- read_rds(here::here("tmp/augmented_data.rds"))

# augmented_data <- DBI::dbReadTable(conn, "nowcast_boost_augment_predict") %>%
#   filter(!validation_set) %>%
#   select(-db_disease_status_etag, -db_cases_etag, -predicted_cases, -validation_set)

# fitting takes about a day for these two models on prospero
repel_fit(model_object =  nowcast_boost_model(disease_status_model = NULL,
                                              cases_model = NULL),
          augmented_data = augmented_data,
          model = "disease_status",
          output_directory = "models")

repel_fit(model_object = nowcast_boost_model(disease_status_model = NULL,
                                             cases_model = NULL),
          augmented_data = augmented_data,
          model = "cases",
          output_directory = "models")

augmented_data$cases <- as.numeric(augmented_data$cases)

predicted_cases <- repel_predict(model_object =  nowcast_boost_model(
  disease_status_model = aws.s3::s3readRDS(bucket = "repeldb/models", object = "boost_mod_disease_status.rds"),
  cases_model = aws.s3::s3readRDS(bucket = "repeldb/models", object = "boost_mod_cases.rds")),
  newdata = augmented_data)

scored_data <- repel_score(model_object =  nowcast_boost_model(disease_status_model = NULL,
                                                               cases_model = NULL),
                           augmented_data = augmented_data,
                           predicted_cases = predicted_cases)

model_object <-  nowcast_boost_model(
  disease_status_model = aws.s3::s3readRDS(bucket = "repeldb/models", object = "boost_mod_disease_status.rds"),
  cases_model = aws.s3::s3readRDS(bucket = "repeldb/models", object = "boost_mod_cases.rds"))

# # example 1
newdata <- tibble(country_iso3c = "AFG",
                  report_year = rep(2016:2020, each = 2),
                  report_semester = rep(1:2, 5),
                  disease = "foot-and-mouth disease",
                  disease_population = "domestic",
                  taxa = "cattle")


forecasted_data <- repel_forecast(model_object = model_object,
                                  conn = conn,
                                  newdata = newdata)

scored_new_data <- repel_score(model_object = model_object,
                               augmented_data = forecasted_data$augmented_data,
                               predicted_cases = forecasted_data$predicted_cases)

# example 2
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

