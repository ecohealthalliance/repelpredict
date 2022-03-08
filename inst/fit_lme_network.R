devtools::load_all()

# Connection and create model object  -------------------------------------------------------------------
conn <- repeldata::repel_remote_conn()
model_object <- network_lme_model()

# Preprocess and augment -----------------------------------------------------------------
valdat <- repel_validation(model_object, conn)
traindat <- repel_training(model_object, conn)

augmented_data <- repel_augment(model_object = model_object,
                                conn = conn, newdata = traindat)

# Fitting -----------------------------------------------------------------
tic()
repel_fit(model_object =  model_object,
          augmented_data = augmented_data,
          predictor_vars = c("continent", "shared_borders_from_outbreaks",
                             "ots_trade_dollars_from_outbreaks","fao_livestock_heads_from_outbreaks", "n_migratory_wildlife_from_outbreaks",
                             "log_gdp_dollars", "log_human_population", "log_target_taxa_population", "log_veterinarians"))
toc() # 7656.041 sec elapsed (~2)

# Predict/Forecast example --------------------------------------------------------
model_object <-  repelpredict::network_lme_model(
  network_model = aws.s3::s3readRDS(bucket = "repeldb/models", object = "lme_mod_network.rds"),
  network_scaling_values = aws.s3::s3readRDS(bucket = "repeldb/models", object = "network_scaling_values.rds")
)
forecasted_lme <-repel_forecast(model_object = model_object,
                                    conn = conn,
                                    newdata = traindat)
# Disconnect DB -----------------------------------------------------------
repel_remote_disconnect()
