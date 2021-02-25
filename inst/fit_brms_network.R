devtools::load_all()
library(brms)
library(tictoc)

#cmdstanr::install_cmdstan()
#cmdstanr::set_cmdstan_path(path = NULL)

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
  mutate(shared_borders_from_outbreaks = as.factor(shared_borders_from_outbreaks)) %>%
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

# brms setup ---------------------------------------------------------------
vars <- c("ots_trade_dollars_from_outbreaks", "shared_borders_from_outbreaks", "fao_livestock_heads_from_outbreaks")

frm <- as.formula(paste0("outbreak_start|weights(count)  ~
                         0 + (1 | country_iso3c:disease) + ", # baseline intercept for disease in country
                         paste0("(0 + ", vars, "|disease)", collapse = " + "))) #  “variance of trade by disease”
# https://discourse.mc-stan.org/t/are-complex-surveys-feasible-in-brms/17058/25

# Fit ---------------------------------------------------------------------

# threading error:
# Syntax error in '/home/emmamendelsohn/tmp/model-12437f546425.stan', line 97, column 22 to column 23, parsing error:
#   -------------------------------------------------
#   95:  parameters {
#     96:    // compute partial sums of the log-likelihood
#     97:    real partial_log_lik(int[] seq, int start, int end, int[] Y, int[] J_1, vector Z_1_1, vector r_1_1, int[] J_2, vector Z_2_1, vector r_2_1, int[] J_3, vector Z_3_1, vector Z_3_2, vector r_3_1, vector r_3_2, int[] J_4, vector Z_4_1, vector r_4_1) {
#       ^
#         98:      real ptarget = 0;
#         99:      int N = end - start + 1;
#         -------------------------------------------------
#
#           ";" is expected after a top-level variable declaration.
#
#
#         make: *** [make/program:53: /home/emmamendelsohn/tmp/model-12437f546425.hpp] Error 1
tic()
mod <- brms::brm(
  formula = frm,
  data = augmented_data_compressed,
  family = 'bernoulli',
  inits = "0", # how to get inits from lme
  iter = 2000,
  chains = 4,
  cores = 4#,
   # backend = "cmdstanr",
   # threads = threading(30)
)
toc()

write_rds(mod, here::here("tmp/brms_mod_simp.rds"))

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

