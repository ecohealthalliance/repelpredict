devtools::load_all(export_all = TRUE)
library(tidyverse)
library(tictoc)
library(mgcv)
library(ceterisParibus)
set.seed(99)

#repeldata::repel_local_download()
conn <- repeldata::repel_local_conn()

# function for bart model predict
gam_predict <- function(model, newdata) {
  predict(object = model, test = newdata, type = "response")
}
# GAM Prep --------------------------------------------------------------------
gam_mod <- nowcast_gam_model()

traindat <- repel_cases_train(conn) %>%
  drop_na(cases) %>%
  sample_n(10000)

augmented_data <- repel_augment.nowcast_gam(model_object = gam_mod, conn = conn, newdata = traindat, lags = 1:10, rare = 100) %>%
  arrange(country_iso3c, disease, taxa, report_year, report_semester)
missing_lags <- is.na(augmented_data$cases_lagged)
augmented_data$cases_lagged[missing_lags] <- 0
augmented_data$lags[missing_lags] <- -10

# GAM model ------------
gam_mod1 <- gam(cases ~ s(lags, bs = c("bs"), by = cases_lagged),
                family = nb(link = "log"),
                data = augmented_data,
                select = TRUE,
                method = "REML")

augmented_data %>% count(condition)

# Next:
# ICE
# SHAP scores - identify parameters of importance for subsets of data

# BART model to predict counts --------------------------------------------------------------------
augmented_data <- repel_augment(model_object = model_object, conn = conn, traindat = traindat, binary_outcome = FALSE) %>%
  arrange(country_iso3c, disease, taxa, report_year, report_semester)

augmented_data <- augmented_data %>%
  filter(cases>0) %>%
  mutate_if(is.numeric, ~log(replace_na(na_if(., 0), 0.5)))

repel_local_disconnect()


