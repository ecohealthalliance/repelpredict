devtools::load_all()
library(lme4)
library(tictoc)
library(insight)
library(DHARMa)

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
  mutate_if(is.numeric, scale2)

augmented_data_compressed <- augmented_data %>%
  # filter(disease %in% get_oie_high_importance_diseases()) %>%
  select(-month) %>%
  group_by_all() %>%
  count() %>%
  ungroup() %>%
  select(country_iso3c, disease, count = n, outbreak_start, everything()) %>%
  arrange(disease, desc(count), country_iso3c)

augmented_data_compressed %>%
  group_by(disease, outbreak_start) %>%
  count() %>%
  ungroup()# outbreak counts of 1 are diseases where the rest of the outbreaks are in the validation set

# how many outbreak starts
table(augmented_data$outbreak_start) #(0.1%)

# lme setup ---------------------------------------------------------------
wgts <- augmented_data_compressed$count
vars <- c("shared_borders_from_outbreaks", "ots_trade_dollars_from_outbreaks", "fao_livestock_heads_from_outbreaks")

frm <- as.formula(paste0("outbreak_start ~
                         0 + (1 | country_iso3c:disease) + ", # baseline intercept for disease in country
                         paste0("(0 + ", vars, "|disease)", collapse = " + "))) #  “variance of trade by disease”

# Fit ---------------------------------------------------------------------
RhpcBLASctl::blas_set_num_threads(16)
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
randef_disease <- randef$disease %>%
  tibble::rownames_to_column(var = "disease") %>%
  as_tibble()
randef_country <- randef$country_iso3c %>%
  tibble::rownames_to_column(var = "country_disease") %>%
  as_tibble() %>%
  separate(country_disease, into = c("country", "disease"), sep = ":") %>%
  rename(intercept = `(Intercept)` )
fixdef <- fixef(mod)

# Overall
summary(mod)

# insight package
get_variance(mod)
# Warning message:
#   Random slopes not present as fixed effects. This artificially inflates the conditional random effect variances.
# Solution: Respecify fixed structure!

# dharma
simulate_residuals <- simulateResiduals(fittedModel = mod, plot = T)


# dataset by condition - compressed dataset - condition without outcome calculate fraction of time with outbreak  - compare against prediction
# metric to compare probabilities (can create qq plot)

# View important network vars?
randef_disease %>% select(disease, shared_borders_from_outbreaksFALSE) %>%  arrange(-shared_borders_from_outbreaksFALSE)
randef_disease %>% select(disease, ots_trade_dollars_from_outbreaks) %>%  arrange(-ots_trade_dollars_from_outbreaks)
randef_disease %>% select(disease, fao_livestock_heads_from_outbreaks) %>%  arrange(-fao_livestock_heads_from_outbreaks)

# View baseline disease by country?
randef_country %>%
  group_by(country) %>%
  arrange(-intercept) %>%
  slice(1:10) %>%
  ungroup() %>%
  View

# visualizations from BB paper did not work - needs fixed effects?

# Predictions -------------------------------------------------------------
# training
augment_predicted <- predict(mod, augmented_data_compressed, type = "response")
summary(augment_predicted)

# validated
# valdat <- repel_validation(model_object, conn) %>%
#   select(country_iso3c, disease, month)

# augmented_data_val <- repel_augment(model_object, conn, newdata = valdat)
# vroom::vroom_write(augmented_data_val, gzfile("tmp/network_augmented_data_val.csv.gz"))
augmented_data_val <- vroom::vroom(here::here("tmp/network_augmented_data_val.csv.gz"))
augmented_data_val2 <- augmented_data_val %>%
  dplyr::select(country_iso3c, disease, month,outbreak_start,
                shared_borders_from_outbreaks,
                ots_trade_dollars_from_outbreaks,
                fao_livestock_heads_from_outbreaks) %>%
  drop_na() %>%
  mutate(country_iso3c = as.factor(country_iso3c)) %>%
  mutate(disease = as.factor(disease)) %>%
  mutate(shared_borders_from_outbreaks = as.factor(as.numeric(shared_borders_from_outbreaks))) %>%
  mutate_if(is.numeric, scale2) %>%
  rsample::bootstraps(times = 1) %>%
  pull(splits) %>%
  magrittr::extract2(1) %>%
  as_tibble()

augment_predicted_val <- predict(mod, augmented_data_val2, type = "response")
max(augment_predicted_val)

# scored
scored_tibble <- augmented_data_val2 %>%
  mutate(outbreak_start_actual = outbreak_start) %>%
  mutate(outbreak_start_predicted = augment_predicted_val >= 0.5) %>%
  mutate(outbreak_start_match = outbreak_start_actual == outbreak_start_predicted)

scored_tibble %>% filter(outbreak_start_actual==1) %>% pull(outbreak_start_predicted) %>% table()

# scored_tibble %>%
#   mutate(outbreak_start_actuaal = factor(outbreak_start_actual),
#          outbreak_start_predicted = factor(outbreak_start_predicted)) %>%
#   yardstick::conf_mat(outbreak_start_actual,  outbreak_start_predicted)

# outbreak status partial dependency by variable

oie_high_importance_diseases <- get_oie_high_importance_diseases()
lag_vars <- c("shared_borders_from_outbreaks", "ots_trade_dollars_from_outbreaks", "fao_livestock_heads_from_outbreaks")

library(DALEX)

explainer <- explain(
  model = mod,
  data = augmented_data_compressed,
  y = "outbreak_start",
  predict_function = predict
)

model_performance(explainer = explainer)
vip_glm <- variable_importance(explainer)


# Baseline ----------------------------------------------------------------
# For spread forecasting, the practical naïve baseline will be assumption of movement to the most strongly connected locale of the current outbreak.
# given disease, given month, which countries are high/low risk
# baseline disease outbreak probability
# how often does it happen in a given country

# disconnect --------------------------------------------------------------
# DBI::dbDisconnect(conn)

