devtools::load_all()
library(lme4)
library(tictoc)
# library(insight)
# library(DHARMa)


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
# toc() # 590.689s
# vroom::vroom_write(augmented_data, gzfile("tmp/network_augmented_data.csv.gz"))

augmented_data <- vroom::vroom(here::here("tmp/network_augmented_data.csv.gz"))

augmented_data <- augmented_data %>%
  select(country_iso3c, disease, month, outbreak_start,
         shared_borders_from_outbreaks,
         ots_trade_dollars_from_outbreaks,
         starts_with("fao"),
         -fao_livestock_heads_from_outbreaks) %>%
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

# augmented_data_compressed %>%
#   group_by(disease, outbreak_start) %>%
#   count() %>%
#   ungroup()# outbreak counts of 1 are diseases where the rest of the outbreaks are in the validation set

# how many outbreak starts
table(augmented_data$outbreak_start) #(0.1%)

# lme setup ---------------------------------------------------------------
wgts <- augmented_data_compressed$count
vars <- c("ots_trade_dollars_from_outbreaks", names(augmented_data)[str_starts(names(augmented_data), "fao_")])

frm <- as.formula(paste0("outbreak_start ~
                         0 + (1 | country_iso3c:disease) + ", # baseline intercept for disease in country
                         '(1 + dummy(shared_borders_from_outbreaks, "TRUE") | disease) + ',
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

write_rds(mod, here::here("tmp/lme_mod_fao.rds"))

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

# Baseline ----------------------------------------------------------------
# For spread forecasting, the practical naïve baseline will be assumption of movement to the most strongly connected locale of the current outbreak.
# given disease, given month, which countries are high/low risk
# baseline disease outbreak probability
# how often does it happen in a given country

# disconnect --------------------------------------------------------------
# DBI::dbDisconnect(conn)

