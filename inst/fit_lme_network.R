devtools::load_all()
library(lme4)
library(tictoc)

# Support functions -------------------------------------------------------
scale2 <- function(x, na.rm = FALSE) (x - mean(x, na.rm = na.rm)) / sd(x, na.rm)

# Connection and create model object  -------------------------------------------------------------------
# conn <- repeldata::repel_local_conn()
# model_object <- network_lme_model()

# Augment  ----------------------------------------------------------------
# repel_init(model_object, conn)
# repel_split(model_object, conn)
# valdat <- repel_validation(model_object, conn)

# traindat <- repel_training(model_object, conn) %>%
#   select(country_iso3c, disease, month)
#
# tic()
# augmented_data <- repel_augment(model_object = model_object,
#                                 conn = conn, newdata = traindat)
# toc() # 551.726s
# vroom::vroom_write(augmented_data, gzfile("tmp/network_augmented_data.csv.gz"))

augmented_data <- vroom::vroom(here::here("tmp/network_augmented_data.csv.gz"))

augmented_data <- augmented_data %>%
  select(country_iso3c, disease, month, outbreak_start,
         shared_borders_from_outbreaks,
         ots_trade_dollars_from_outbreaks,
         fao_livestock_heads_from_outbreaks) %>%
  drop_na() %>%
  mutate(country_iso3c = as.factor(country_iso3c)) %>%
  mutate(disease = as.factor(disease)) %>%
  mutate(shared_borders_from_outbreaks = as.factor(as.numeric(shared_borders_from_outbreaks))) %>%
  mutate(outbreak_start = as.factor(as.numeric(outbreak_start))) %>%
  mutate_if(is.numeric, scale2)


augmented_data_compressed <- augmented_data %>%
 # filter(disease %in% get_oie_high_importance_diseases()) %>%
  select(-month) %>%
  group_by_all() %>%
  count() %>%
  ungroup() %>%
  select(country_iso3c, disease, count = n, outbreak_start, everything()) %>%
  arrange(disease, desc(count), country_iso3c)

# what percent of events are oie diseases?
sum(augmented_data_compressed$count)/nrow(augmented_data)

# lme setup ---------------------------------------------------------------
wgts <- augmented_data_compressed$count
vars <- c("shared_borders_from_outbreaks", "ots_trade_dollars_from_outbreaks", "fao_livestock_heads_from_outbreaks")

RhpcBLASctl::blas_set_num_threads(16)

frm <- as.formula(paste0("outbreak_start ~ 0 + (1 | country_iso3c:disease) + ",
                         paste0("(0 + ", vars, "|disease)", collapse = " + ")))

# Fit ---------------------------------------------------------------------
tic("16 blas threads")
mod <- lme4::glmer(data = augmented_data_compressed,
                   weights = wgts,
                   family = binomial,
                   formula = frm,
                   nAGQ = 0, # adaptive Gaussian quadrature instead the Laplace approximation. The former is known to be better for binomial data.
                   verbose = 2, control = glmerControl(calc.derivs = TRUE))
toc()

write_rds(mod, here::here("tmp/lme_mod_all_diseases.rds"))

# Review mods -------------------------------------------------------------
mod <- read_rds(here::here("tmp/lme_mod_all_diseases.rds"))
randef <- ranef(mod)
randef$disease
randef$country_iso3c %>% head()
fixdef <- fixef(mod)

# disconnect --------------------------------------------------------------
# DBI::dbDisconnect(conn)

