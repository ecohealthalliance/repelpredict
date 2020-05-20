devtools::load_all()
library(tictoc)

conn <- repel_local_conn()

# Baseline ----------------------------------------------------------------
sample_size <- 150
model_object <- nowcast_baseline_model()
newdata <- repel_cases_train(conn) %>%
  select("country_iso3c", "report_year", "report_semester", "disease", "taxa") %>%
  slice(sample(1:nrow(.), size = sample_size))

augmented_data <- repel_augment.nowcast_baseline(model_object = model_object, conn = conn, newdata = newdata)
assertthat::are_equal(sample_size, nrow(augmented_data)) # TODO ^ NEED TO HANDLE DUPEs (related to serotypes and disease populations, I think. Should be included in primary grouping vars)

predicted_data <- repel_predict.nowcast_baseline(model_object = model_object, augmented_data = augmented_data)

scored_data <- repel_score.nowcast_baseline(model_object = model_object, augmented_data = augmented_data)

repel_forecast(model_object = model_object, conn = conn, newdata = newdata)
