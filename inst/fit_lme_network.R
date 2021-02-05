devtools::load_all()
library(lme4)
library(tictoc)

conn <- repeldata::repel_local_conn()
model_object <- network_lme_model()

scale2 <- function(x, na.rm = FALSE) (x - mean(x, na.rm = na.rm)) / sd(x, na.rm)

# # Fitting  ----------------------------------------------------------------
# repel_init(model_object, conn)
# repel_split(model_object, conn)
# valdat <- repel_validation(model_object, conn)

traindat <- repel_training(model_object, conn) %>%
  select(country_iso3c, disease, month)

tic()
augmented_data <- repel_augment(model_object = model_object,
                                conn = conn, newdata = traindat)
toc() # 551.726
vroom::vroom_write(augmented_data, gzfile("tmp/network_augmented_data.csv.gz"))

augmented_data <- vroom::vroom(here::here("tmp/network_augmented_data.csv.gz"))

# model -------------------------------------------------------------------

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

# what percent of events are oie diseases?

augmented_data_compressed <- augmented_data %>%
  #filter(month >= "2010-01-01", month <= "2010-12-01") %>%
  filter(disease %in% get_oie_high_importance_diseases()[1:4]) %>%
  select(-month) %>%
  group_by_all() %>%
  count() %>%
  ungroup() %>%
  select(country_iso3c, disease, count = n, outbreak_start, everything()) %>%
  arrange(disease, desc(count), country_iso3c)

# how many of the events

wgts <- augmented_data_compressed$count

vars <- c("shared_borders_from_outbreaks", "ots_trade_dollars_from_outbreaks", "fao_livestock_heads_from_outbreaks")

RhpcBLASctl::blas_set_num_threads(16)

frm <- as.formula(paste0("outbreak_start ~ (0 | country_iso3c) + ",
                         paste0("(0 + ", vars, "|disease)", collapse = " + "))) # set to not fit intercept (0)
tic("16 blas threads")

mod <- lme4::glmer(data = augmented_data_compressed,
                   weights = wgts,
                   family = binomial,
                   formula = frm,
                   nAGQ = 0, # adaptive Gaussian quadrature instead the Laplace approximation. The former is known to be better for binomial data.
                   verbose = 2, control = glmerControl(calc.derivs = TRUE))
toc()


# Try fitting the model with the adaptive Gaussian quadrature instead the Laplace approximation. The former is known to be better for binomial data.
# You could also give a try to the GLMMadaptive package that can fit the same model using the adaptive Gaussian quadrature.
# By default it is set to 1, (corresponding to the Laplace approximation, see ?glmer). Setting to 0 gives a less exact approximation, but the model is more likely to at least run without errors.
# The documentation on the difference in model implementation between nAGQ=1 and nAGQ=0 is not very detailed (possibly just my ignorance of the terms used). I found the answer to this question: Why can't I match glmer (family=binomial) output with manual implementation of Gauss-Newton algorithm? Helpful in explaining the difference a little more.
# nAGQ = 0 means that the random effects only influence the estimates of the fixed effects through their estimated conditional modes -- therefore, nAGQ = 0 does not completely account for the randomness of the random effects. To fully account for the random effects, they need to be integrated out.
# https://stats.stackexchange.com/questions/77313/why-cant-i-match-glmer-family-binomial-output-with-manual-implementation-of-g

#TODO

# Warning message:
#   In optwrap(optimizer, devfun, start, rho$lower, control = control,  :
#                convergence code 1 from bobyqa: bobyqa -- maximum number of function evaluations exceeded

# output country by one disease only


write_rds(mod, here::here("tmp/lme_mod_v3.rds"))


# Review mods -------------------------------------------------------------

# mod <- read_rds(here::here("tmp/lme_mod_v1.rds"))
# randef <- lme4::ranef(mod)
# randef$disease
# randef$country_iso3c

# disconnect --------------------------------------------------------------
# DBI::dbDisconnect(conn)

