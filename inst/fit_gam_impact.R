devtools::load_all()

# Connection and create model object  -------------------------------------------------------------------
conn <- repeldata::repel_remote_conn(host="0.0.0.0", port = 22053)
model_object <- impact_gam_model()

# Augment  ----------------------------------------------------------------
# debug(repel_init.impact_model)
dat <- repel_init(model_object, conn)
augmented_data <- repel_augment(model_object = model_object,
                                conn = conn, newdata = dat)
