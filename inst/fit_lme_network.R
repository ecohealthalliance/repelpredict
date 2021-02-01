devtools::load_all()
library(lme4)

# repeldata::repel_local_download()
conn <- repeldata::repel_local_conn()
model_object <- network_lme_model()

scale2 <- function(x, na.rm = FALSE) (x - mean(x, na.rm = na.rm)) / sd(x, na.rm)

# Fitting  ----------------------------------------------------------------
# repel_init(model_object, conn)
# repel_split(model_object, conn)
# valdat <- repel_validation(model_object, conn)

traindat <- repel_training(model_object, conn) %>%
  select(country_iso3c, disease, month)

augmented_data <- repel_augment(model_object = model_object,
                                conn = conn, newdata = traindat)
vroom::vroom_write(augmented_data, gzfile("tmp/network_augmented_data.csv.gz"))

augmented_data <- vroom::vroom(here::here("tmp/network_augmented_data.csv.gz"))

# model -------------------------------------------------------------------

augmented_data <- augmented_data %>%
  mutate(country_iso3c = as.factor(country_iso3c)) %>%
  mutate(disease = as.factor(disease)) %>%
  mutate(outbreak_start = outbreak_start>0) %>%
  select(country_iso3c, disease, month, outbreak_start,
         shared_borders_from_outbreaks = shared_border,
         ots_trade_dollars_from_outbreaks = ots_trade_dollars,
         fao_livestock_heads_from_outbreaks = fao_livestock_heads) %>%
  drop_na() %>%
  mutate_if(is.numeric, scale2)

vars <- c("shared_borders_from_outbreaks", "ots_trade_dollars_from_outbreaks", "fao_livestock_heads_from_outbreaks")

all_cores <- parallel::detectCores(logical = FALSE)
cl <- parallel::makePSOCKcluster(all_cores)
doParallel::registerDoParallel(cl)

# V1
# frm <- as.formula(paste0("outbreak_start ~ ",
#                          paste0(vars, " + (", vars, "|disease) + (", vars, "|country_iso3c) ", collapse = " + ")))
# boundary (singular) fit: see ?isSingular
# Warning messages:
#   1: In optwrap(optimizer, devfun, start, rho$lower, control = control,  :
#                   convergence code 1 from bobyqa: bobyqa -- maximum number of function evaluations exceeded
#                 2: In optwrap(optimizer, devfun, start, rho$lower, control = control,  :
#                                 convergence code 1 from bobyqa: bobyqa -- maximum number of function evaluations exceeded

# V2
frm <- as.formula(paste0("outbreak_start ~ (disease | country_iso3c) + ",
                         paste0("(", vars, "|disease)", collapse = " + ")))


mod <- lme4::glmer(data = augmented_data, family = binomial, formula = frm, verbose = TRUE,
                   control = glmerControl(
                     optimizer="bobyqa",
                     check.nobs.vs.nlev="ignore",
                     check.nobs.vs.nRE="ignore"))
write_rds(mod, here::here("tmp/lme_mod_v2.rds"))

#TODO add verbose argument - process
# single threaded calculations, matrix calculations use more cores
# turn on - hpc package - low level blas cores - make 16


parallel::stopCluster(cl = cl)


# Review mods -------------------------------------------------------------

# random effects report variance of the distribution (disease or country) - does not give coefficients to each level

# mod <- read_rds(here::here("tmp/lme_mod_v1.rds"))

# disconnect --------------------------------------------------------------
# DBI::dbDisconnect(conn)


# archive -----------------------------------------------------------------

# frm <- as.formula(paste0("outbreak_start ~ ",
#                          paste0("(", vars, "|disease) + (", vars, " | country_iso3c) ", collapse = " + ")))
# ^ Error in (function (fr, X, reTrms, family, nAGQ = 1L, verbose = 0L, maxit = 100L,  :
#                       (maxstephalfit) PIRLS step-halvings failed to reduce deviance in pwrssUpdate
#                     Calls: <Anonymous> -> do.call -> <Anonymous>

# frm <- as.formula(paste0("outbreak_start ~ ",
#                          paste0("(", vars, "-1|disease) + (", vars, "-1|country_iso3c) ", collapse = " + ")))
# Error in (function (fr, X, reTrms, family, nAGQ = 1L, verbose = 0L, maxit = 100L,  :
#                       (maxstephalfit) PIRLS step-halvings failed to reduce deviance in pwrssUpdate
#                     Calls: <Anonymous> -> do.call -> <Anonymous>


# frm <- as.formula(paste0("outbreak_start ~ (disease | country_iso3c) + ",
#                          paste0("(", vars, "|disease)", collapse = " + ")))
# Error in (function (fr, X, reTrms, family, nAGQ = 1L, verbose = 0L, maxit = 100L,  :
#                       pwrssUpdate did not converge in (maxit) iterations
#                     Calls: <Anonymous> -> do.call -> <Anonymous>

