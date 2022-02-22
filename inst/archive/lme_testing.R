devtools::load_all()
library(lme4)
library(tictoc)

augmented_data <- vroom::vroom(here::here("tmp/network_augmented_data.csv.gz"))

predictor_vars = c("shared_borders_from_outbreaks", "ots_trade_dollars_from_outbreaks", "fao_livestock_heads_from_outbreaks",
                   "target_taxa_population", "human_population", "gdp_dollars")
output_directory = "models"

augmented_data_select <- augmented_data %>%
  drop_na() %>%
  filter(!endemic) %>%
  filter(!outbreak_subsequent_month) # %>%
# filter(!disease_country_combo_unreported) # fails to fit if all these zeros are left in

# mean/sd for scaling predictions
scaling_values <- augmented_data_select %>%
  select(all_of(predictor_vars)) %>%
  gather() %>%
  group_by(key) %>%
  summarize(mean = mean(value), sd = sd(value)) %>%
  ungroup()

augmented_data_compressed <- augmented_data_select %>%
  network_recipe(., predictor_vars, scaling_values) %>%
  group_by_all() %>%
  count() %>%
  ungroup() %>%
  mutate(continent = as.factor(continent)) %>%
  select(country_iso3c, continent, disease, count = n, outbreak_start, everything()) %>%
  arrange(disease, desc(count), country_iso3c)

wgts <- augmented_data_compressed$count

# Version 1 ---------------------------------------------------------------
frm <- as.formula("outbreak_start ~ target_taxa_population + human_population + gdp_dollars + continent +
        (0 + shared_borders_from_outbreaks | disease) +
        (0 + ots_trade_dollars_from_outbreaks | disease) +
        (0 + fao_livestock_heads_from_outbreaks | disease)")

# syntax notes: (https://stats.stackexchange.com/questions/13166/rs-lmer-cheat-sheet)
# (0 + var | disease) = The effect of var within each level of disease (more specifically, the degree to which the var effect within a given level deviates from the global effect of var), while enforcing a zero correlation between the intercept deviations and var effect deviations across levels of disease
# single global estimate for the effect (slope) of fixed vars

RhpcBLASctl::blas_set_num_threads(16)
tic("16 blas threads")
mod <- lme4::glmer(data = augmented_data_compressed,
                   weights = wgts,
                   family = binomial,
                   formula = frm,
                   nAGQ = 0, # adaptive Gaussian quadrature instead the Laplace approximation. The former is known to be better for binomial data.
                   verbose = 2, control = glmerControl(calc.derivs = TRUE))
toc()

write_rds(mod, here::here(paste0(output_directory, "/lme_mod_network1.rds")))
aws.s3::s3saveRDS(mod, bucket = "repeldb/models", object = "lme_mod_network1.rds")

write_csv(scaling_values, here::here(paste0(output_directory, "/network_scaling_values1.csv")))
aws.s3::s3saveRDS(scaling_values, bucket = "repeldb/models", object = "network_scaling_values1.rds")

# predictions
newdat <- augmented_data_select %>%
  network_recipe(., predictor_vars, scaling_values)
preds <- predict(mod, newdat, type = "response")

# AIC        BIC     logLik   deviance   df.resid
# 16660.514  16772.275  -8319.257  16638.514     191008

# Version 2 ---------------------------------------------------------------
frm <- as.formula("outbreak_start ~ target_taxa_population + human_population + gdp_dollars +
        (0 + continent | disease) +
        (0 + shared_borders_from_outbreaks | disease) +
        (0 + ots_trade_dollars_from_outbreaks | disease) +
        (0 + fao_livestock_heads_from_outbreaks | disease)")

# syntax notes: (https://stats.stackexchange.com/questions/13166/rs-lmer-cheat-sheet)
# (0 + var | disease) = The effect of var within each level of disease (more specifically, the degree to which the var effect within a given level deviates from the global effect of var), while enforcing a zero correlation between the intercept deviations and var effect deviations across levels of disease
# single global estimate for the effect (slope) of fixed vars

RhpcBLASctl::blas_set_num_threads(16)
tic("16 blas threads")
mod <- lme4::glmer(data = augmented_data_compressed,
                   weights = wgts,
                   family = binomial,
                   formula = frm,
                   nAGQ = 0, # adaptive Gaussian quadrature instead the Laplace approximation. The former is known to be better for binomial data.
                   verbose = 2, control = glmerControl(calc.derivs = TRUE))
toc()

write_rds(mod, here::here(paste0(output_directory, "/lme_mod_network2.rds")))
aws.s3::s3saveRDS(mod, bucket = "repeldb/models", object = "lme_mod_network2.rds")

write_csv(scaling_values, here::here(paste0(output_directory, "/network_scaling_values2.csv")))
aws.s3::s3saveRDS(scaling_values, bucket = "repeldb/models", object = "network_scaling_values2.rds")

# predictions
newdat <- augmented_data_select %>%
  network_recipe(., predictor_vars, scaling_values)
preds <- predict(mod, newdat, type = "response")

# AIC        BIC     logLik   deviance   df.resid
# 15385.568  15609.091  -7670.784  15341.568     190997

# Version 3 ---------------------------------------------------------------
frm <- as.formula("outbreak_start ~
        (0 + target_taxa_population | disease) +
        (0 + human_population | disease) +
        (0 + gdp_dollars | disease) +
        (0 + continent | disease) +
        (0 + shared_borders_from_outbreaks | disease) +
        (0 + ots_trade_dollars_from_outbreaks | disease) +
        (0 + fao_livestock_heads_from_outbreaks | disease)")

# syntax notes: (https://stats.stackexchange.com/questions/13166/rs-lmer-cheat-sheet)
# (0 + var | disease) = The effect of var within each level of disease (more specifically, the degree to which the var effect within a given level deviates from the global effect of var), while enforcing a zero correlation between the intercept deviations and var effect deviations across levels of disease
# single global estimate for the effect (slope) of fixed vars

RhpcBLASctl::blas_set_num_threads(16)
tic("16 blas threads")
mod <- lme4::glmer(data = augmented_data_compressed,
                   weights = wgts,
                   family = binomial,
                   formula = frm,
                   nAGQ = 0, # adaptive Gaussian quadrature instead the Laplace approximation. The former is known to be better for binomial data.
                   verbose = 2, control = glmerControl(calc.derivs = TRUE))
toc()

write_rds(mod, here::here(paste0(output_directory, "/lme_mod_network3.rds")))
aws.s3::s3saveRDS(mod, bucket = "repeldb/models", object = "lme_mod_network3.rds")

write_csv(scaling_values, here::here(paste0(output_directory, "/network_scaling_values3.csv")))
aws.s3::s3saveRDS(scaling_values, bucket = "repeldb/models", object = "network_scaling_values3.rds")

# predictions
newdat <- augmented_data_select %>%
  network_recipe(., predictor_vars, scaling_values)
preds <- predict(mod, newdat, type = "response")

# AIC        BIC     logLik   deviance   df.resid
# 15364.405  15587.928  -7660.203  15320.405     190997
