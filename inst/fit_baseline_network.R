devtools::load_all()

# Connection and create model object  -------------------------------------------------------------------
conn <- repeldata::repel_remote_conn()
model_object <- network_baseline_model()

# Preprocess and augment -----------------------------------------------------------------
#valdat <- repel_validation(model_object, conn)
traindat <- repel_training(model_object, conn)

augmented_data <- repel_augment(model_object = model_object,
                                conn = conn, newdata = traindat)

# Fitting -----------------------------------------------------------------
# exclude the connect variables
tic()
repel_fit(model_object =  model_object,
          augmented_data = augmented_data,
          predictor_vars = c("continent", "disease_present_anywhere",
                             "log_gdp_dollars", "log_human_population", "log_target_taxa_population", "log_veterinarians"),
          baseline = TRUE)
toc()


# Disconnect DB -----------------------------------------------------------
repel_remote_disconnect()


# Check model -------------------------------------------------------------

# Load model
model_object_baseline <-  network_lme_model(
  network_model = aws.s3::s3readRDS(bucket = "repeldb/models", object = "lme_mod_network_baseline.rds"),
  network_scaling_values = aws.s3::s3readRDS(bucket = "repeldb/models", object = "network_scaling_values_baseline.rds")
)
lme_mod_baseline <- model_object_baseline$network_model
network_scaling_values_baseline <- model_object_baseline$network_scaling_values

