devtools::load_all()
# repeldata::repel_local_download()
conn <- repeldata::repel_local_conn()
model_object <- network_lme_model()

# Fitting  ----------------------------------------------------------------
# repel_init(model_object, conn)
# repel_split(model_object, conn)
# valdat <- repel_validation(model_object, conn)

traindat <- repel_training(model_object, conn) %>%
  select(country_iso3c, disease, month)

augmented_data <- repel_augment(model_object = model_object,
                                conn = conn, newdata = traindat)
vroom::vroom_write(augmented_data, gzfile("tmp/network_augmented_data.csv.gz"))

# scale before fitting!

# disconnect --------------------------------------------------------------

DBI::dbDisconnect(conn)
