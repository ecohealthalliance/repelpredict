devtools::load_all()

# Connection and create model object  -------------------------------------------------------------------
conn <- repeldata::repel_remote_conn()
model_object <-  nowcast_boost_model(disease_status_model = NULL, cases_model = NULL)

# Preprocess and augment -----------------------------------------------------------------
# for model fitting
valdat <- repel_validation(model_object, conn)
map(valdat, ~any(is.na(.))) # NA in cases expected
write_rds(valdat, here::here("tmp/nowcast_valdat.rds"))
valdat <- read_rds(here::here("tmp/nowcast_valdat.rds"))

traindat <- repel_training(model_object, conn)
map(traindat, ~any(is.na(.))) # NA in cases expected
write_rds(traindat, here::here("tmp/nowcast_traindat.rds"))
traindat <- read_rds(here::here("tmp/nowcast_traindat.rds"))

augmented_data <- repel_augment(model_object = model_object,
                                conn = conn,
                                subset = traindat) %>%
  arrange(country_iso3c, disease, taxa, report_year, report_semester)
assertthat::are_equal(nrow(janitor::get_dupes(augmented_data, all_of(grouping_vars))), 0)

augmented_data_na_check = map_lgl(augmented_data, ~any(is.na(.)))
augmented_data_na_check[augmented_data_na_check] # NAs expected for cases, disease_status_lagx_unreported

write_rds(augmented_data, "tmp/augmented_data.rds")
augmented_data <- read_rds("tmp/augmented_data.rds")

# Augment expanded dataset for predictions ---------------------------------------------------------------------
# full expanded dataset (not traindat)
augmented_data_expanded <- repel_augment(model_object = model_object,
                                         conn = conn,
                                         subset = NULL) %>%
  arrange(country_iso3c, disease, taxa, report_year, report_semester)
write_rds(augmented_data_expanded, "tmp/augmented_data_expanded.rds")
augmented_data_expanded <- read_rds("tmp/augmented_data_expanded.rds")

augmented_data_expanded_na_check = map_lgl(augmented_data_expanded, ~any(is.na(.)))
augmented_data_expanded_na_check[augmented_data_expanded_na_check] # NAs expected for cases, disease_status_lagx_unreported

# Fitting -----------------------------------------------------------------
# takes ~ 5 hrs
repel_fit(model_object =  model_object,
          augmented_data = augmented_data,
          model = "disease_status",
          output_directory = "models")

# takes ~1 hr
repel_fit(model_object = model_object,
          augmented_data = augmented_data,
          model = "cases",
          output_directory = "models")


# Load fitted models ------------------------------------------------------
model_object <-  nowcast_boost_model(
  disease_status_model = aws.s3::s3readRDS(bucket = "repeldb/models", object = "boost_mod_disease_status.rds"),
  cases_model = aws.s3::s3readRDS(bucket = "repeldb/models", object = "boost_mod_cases.rds")
)

# Look at status predictions --------------------------------------------
get_ds_conf_max <- function(model_object, augmented_data){
  predicted <- predict(object = model_object$disease_status_model, new_data = augmented_data)
  predicted <- predicted %>% pull(.pred_class)
  actual <- augmented_data %>% pull(disease_status)
  tibble(actual, predicted) %>%
    yardstick::conf_mat(truth = actual, estimate = predicted)
}
# predict on augmented traindat from the model object
augmented_data_from_model <- model_object$disease_status_model$pre$actions$recipe$recipe$template
# actual <- model_object$disease_status_model$pre$mold$outcomes$disease_status # actual can be retrieved from model object
dim(augmented_data_from_model)
get_ds_conf_max(model_object, augmented_data = augmented_data_from_model)

dim(augmented_data_expanded)
get_ds_conf_max(model_object, augmented_data = augmented_data_expanded)

# Look at cases predictions --------------------------------------------
plot_cases <- function(model_object, augmented_data){
  predicted <- predict(object = model_object$cases_model, new_data = augmented_data)
  predicted <- predicted %>% pull(.pred)
  actual <- augmented_data %>% pull(cases)
  plot(log10(actual), predicted)
}

# predict on augmented traindat from the model object (filtered for positive cases in current or previous semester)
augmented_data_from_model <- model_object$cases_model$pre$actions$recipe$recipe$template
# actual <- model_object$cases_model$pre$mold$outcomes$cases # actual can be retrieved from the model object
dim(augmented_data_from_model)
plot_cases(model_object, augmented_data = augmented_data_from_model)

# predict on augmented traindat (not filtered for positives)
dim(augmented_data)
plot_cases(model_object, augmented_data = augmented_data)

# full expanded dataset (not traindat)
dim(augmented_data_expanded)
plot_cases(model_object, augmented_data = augmented_data_expanded)

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

