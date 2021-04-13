devtools::load_all()
library(lme4)
library(tictoc)
#cmdstanr::install_cmdstan()
#cmdstanr::set_cmdstan_path(path = NULL)

# Connection and create model object  -------------------------------------------------------------------
conn <- repeldata::repel_local_conn()
model_object <- network_lme_model()

# Augment  ----------------------------------------------------------------
# repel_init(model_object, conn)
# repel_split(model_object, conn)
# valdat <- repel_validation(model_object, conn)

traindat <- repel_training(model_object, conn) %>%
  filter(!disease_country_combo_unreported) %>%
  select(country_iso3c, disease, month)

tic()
augmented_data <- repel_augment(model_object = model_object,
                                conn = conn, newdata = traindat)
toc() # 854.062 sec elapsed
vroom::vroom_write(augmented_data, gzfile("tmp/network_augmented_data.csv.gz"))

augmented_data <- vroom::vroom(here::here("tmp/network_augmented_data.csv.gz"))

# Fitting -----------------------------------------------------------------
repel_fit(model_object =  model_object,
          augmented_data = augmented_data,
          predictor_vars = c("shared_borders_from_outbreaks", "ots_trade_dollars_from_outbreaks", "fao_livestock_heads_from_outbreaks"),
          output_directory = "models")

# Forecast on training ----------------------------------------------------
model_object <-  network_lme_model(
  network_model = aws.s3::s3readRDS(bucket = "repeldb/models", object = "lme_mod_network.rds"),
  network_scaling_values = aws.s3::s3readRDS(bucket = "repeldb/models", object = "network_scaling_values.rds")
  )

tic()

forecasted_data <- repel_forecast(model_object = model_object,
                                  conn = conn,
                                  newdata = traindat,
                                  use_cache = TRUE)
toc()

repel_local_disconnect()

