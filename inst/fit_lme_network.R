devtools::load_all()
library(lme4)
library(tictoc)
library(insight)
library(DHARMa)
library(ggplot2)

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

# Review mod -------------------------------------------------------------
mod <- read_rds(here::here("tmp/lme_mod_all_diseases.rds"))

# Overall
summary(mod)

# Random effects
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

randef_disease_long <- randef_disease %>%
  filter(disease %in% get_oie_high_importance_diseases()) %>%
  pivot_longer(-disease) %>%
  mutate(disease = str_replace_all(disease, "_", " "))

vars_clean <- c("No shared border with one or more country with existing outbreaks",
                       "Shared border with one or more country with existing outbreaks",
                       "Trade value of animal products from countries with existing outbreaks",
                       "Livestock (heads) from countries with existing outbreaks"
                       )
names(vars_clean) <- unique(randef_disease_long$name)


randef_coeffs <- purrr::map(unique(randef_disease_long$name), function(var){
  dat <- randef_disease_long %>%
    filter(name == var) %>%
    mutate(disease = forcats::fct_reorder(disease, value)) %>%
    mutate(pos = value > 0)
  title <- vars_clean[var]
  ggplot(dat) +
    geom_hline(aes(yintercept = 0), color = "gray50") +
    geom_point(aes(x = disease, y = value, color = pos), size = 2) +
    geom_segment(aes(x = disease, xend = disease, y = value, yend = 0, color = pos)) +
    scale_color_manual(values = c("TRUE" = "#0072B2", "FALSE" = "#D55E00")) +
    labs(x = "", y = "", title = title) +
    coord_flip() +
    theme_minimal() +
    theme(legend.position = "none", axis.text = element_text(size = 12)) +
    NULL

})


# insight package
# get_variance(mod)
# Warning message:
#   Random slopes not present as fixed effects. This artificially inflates the conditional random effect variances.
# Solution: Respecify fixed structure!

# dharma
# simulate_residuals <- simulateResiduals(fittedModel = mod, plot = F)
# resid <- residuals(simulate_residuals, quantileFunction = qnorm, outlierValues = c(-7,7))
# # plot(simulate_residuals)
# testResiduals(simulate_residuals)

# Predictions -------------------------------------------------------------
# training
augment_predicted <- predict(mod, augmented_data_compressed, type = "response")
summary(augment_predicted)

# validated
# valdat <- repel_validation(model_object, conn) %>%
#   select(country_iso3c, disease, month)

# augmented_data_val <- repel_augment(model_object, conn, newdata = valdat)
# vroom::vroom_write(augmented_data_val, gzfile("tmp/network_augmented_data_val.csv.gz"))
augmented_data_val <- vroom::vroom(here::here("tmp/network_augmented_data_val.csv.gz"))  %>%
  dplyr::select(country_iso3c, disease, month, outbreak_start,
                shared_borders_from_outbreaks,
                ots_trade_dollars_from_outbreaks,
                fao_livestock_heads_from_outbreaks) %>%
  drop_na() %>%
  mutate(country_iso3c = as.factor(country_iso3c)) %>%
  mutate(disease = as.factor(disease)) %>%
  mutate_if(is.numeric, scale2)

# resample validation
augmented_data_val_bootstrap <- augmented_data_val %>%
  rsample::bootstraps(times = 1) %>%
  pull(splits) %>%
  magrittr::extract2(1) %>%
  as_tibble() %>%
  mutate(outbreak_start_predicted = predict(mod, ., type = "response"))

# get predicted outbreak start probability by condition
augmented_data_val_compressed <- augmented_data_val_bootstrap %>%
  distinct(country_iso3c, disease, shared_borders_from_outbreaks, ots_trade_dollars_from_outbreaks, fao_livestock_heads_from_outbreaks, outbreak_start_predicted) %>%
  rename(outbreak_start_predict_prob = outbreak_start_predicted)

# get actual outbreak start probability by condition from the full dataset
augmented_data_all_compressed <- augmented_data %>%
  bind_rows(augmented_data_val) %>%
  group_by(country_iso3c, disease, shared_borders_from_outbreaks, ots_trade_dollars_from_outbreaks, fao_livestock_heads_from_outbreaks) %>%
  summarize(outbreak_start_actual_prob = sum(outbreak_start)/n()) %>%
  ungroup()

# compare validation predicted against actual prob
validation_outbreak_comp <- augmented_data_val_compressed %>%
  left_join(augmented_data_all_compressed)

ggplot(validation_outbreak_comp, aes(x = outbreak_start_actual_prob, y = outbreak_start_predict_prob)) +
  geom_point() +
  scale_x_continuous(limits = c(0,1)) +
  #scale_y_continuous(limits = c(0,1)) +
  NULL

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

# Baseline ----------------------------------------------------------------
# For spread forecasting, the practical naïve baseline will be assumption of movement to the most strongly connected locale of the current outbreak.
# given disease, given month, which countries are high/low risk
# baseline disease outbreak probability
# how often does it happen in a given country

# disconnect --------------------------------------------------------------
# DBI::dbDisconnect(conn)

