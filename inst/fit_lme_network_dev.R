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

augmented_data <- vroom::vroom("tmp/network_augmented_data.csv.gz")

# model -------------------------------------------------------------------
scale2 <- function(x, na.rm = FALSE) (x - mean(x, na.rm = na.rm)) / sd(x, na.rm)
trade_names <- colnames(augmented_data)[str_detect(colnames(augmented_data), "trade_")]

augmented_data <- augmented_data %>%
  mutate(country_iso3c = as.factor(country_iso3c)) %>%
  mutate(disease = as.factor(disease)) %>%
  mutate_at(trade_names, scale2)

# V1
# frm <- as.formula(paste0("outbreak_start ~ ", paste(trade_names, collapse = " + "), " + (1 | country_iso3c) + (1 | disease)"))
# mod <- lme4::glmer(data = augmented_data, family = binomial, formula = frm)
# write_rds(mod, here::here("tmp/lme_mod_v1.rds"))

# V2
trade_names <- c("trade_asses", "trade_buffaloes", "trade_camels", "trade_cattle", "trade_chickens", "trade_ducks", "trade_goats",
                 "trade_horses", "trade_mules", "trade_pigs", "trade_rabbits_and_hares", "trade_rodents_other", "trade_sheep", "trade_turkeys",
                 "trade_meat_and_edible_meat_offal", "trade_dairy_produce", "trade_furskins_and_artificial_fur", "trade_raw_hides_and_skins_other_than_furskins_and_leather")
augmented_data <- augmented_data %>%
  select(country_iso3c, disease, month, outbreak_start, starts_with("n_outbreaks"), all_of(trade_names))

frm2 <- as.formula(paste0("outbreak_start ~ ", paste0(trade_names, " + (", trade_names, " | disease) + (", trade_names, "| country_iso3c)", collapse = " + ")))

all_cores <- parallel::detectCores(logical = FALSE)
cl <- parallel::makePSOCKcluster(all_cores)
doParallel::registerDoParallel(cl)

mod2 <- lme4::glmer(data = augmented_data, family = binomial, formula = frm2)
write_rds(mod2, here::here("tmp/lme_mod_v2.rds"))

parallel::stopCluster(cl = cl)

# disconnect --------------------------------------------------------------
DBI::dbDisconnect(conn)
