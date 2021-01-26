devtools::load_all()
# repeldata::repel_local_download()
conn <- repeldata::repel_local_conn()
model_object <- network_lme_model()

# Fitting  ----------------------------------------------------------------
# repel_init(model_object, conn)
# repel_split(model_object, conn)

traindat <- repel_training(model_object, conn)
valdat <- repel_validation(model_object, conn)

# disconnect --------------------------------------------------------------

DBI::dbDisconnect(conn)
