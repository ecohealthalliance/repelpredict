devtools::load_all()

# Connection and create model object  -------------------------------------------------------------------
# conn <- repeldata::repel_remote_conn()
model_object <- network_lme_model()

# Augment  ----------------------------------------------------------------
# valdat <- repel_validation(model_object, conn)
#
# traindat <- repel_training(model_object, conn) %>%
#   select(country_iso3c, disease, month)
# vroom::vroom_write(traindat, here::here("tmp/network_traindat.csv.gz"))
# traindat <- vroom::vroom(here::here("tmp/network_traindat.csv.gz"))
#
# augmented_data <- repel_augment(model_object = model_object,
#                                 conn = conn, newdata = traindat)
#
# vroom::vroom_write(augmented_data, gzfile("tmp/network_augmented_data.csv.gz"))
augmented_data <- vroom::vroom(here::here("tmp/network_augmented_data.csv.gz"))

# repel_remote_disconnect()

# Fitting -----------------------------------------------------------------
tic()
repel_fit(model_object =  model_object,
          augmented_data = augmented_data,
          predictor_vars = c("continent", "shared_borders_from_outbreaks",
                             "ots_trade_dollars_from_outbreaks","fao_livestock_heads_from_outbreaks", "n_migratory_wildlife_from_outbreaks",
                             "log_gdp_dollars", "log_human_population", "log_target_taxa_population", "log_veterinarians"),
          output_directory = "models")
toc() # 7656.041 sec elapsed (~2)

# Forecast on training and validation  ----------------------------------------------------
# model_object <-  network_lme_model(
#   network_model = aws.s3::s3readRDS(bucket = "repeldb/models", object = "lme_mod_network.rds"),
#   network_scaling_values = aws.s3::s3readRDS(bucket = "repeldb/models", object = "network_scaling_values.rds")
# )
#
# repel_predict(model_object, newdata = augmented_data)
#
# forecasted_data <- repel_forecast(model_object = model_object,
#                                   conn = conn,
#                                   newdata = valdat,
#                                   use_cache = FALSE)
#
