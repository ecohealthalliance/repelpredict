devtools::load_all()
library(tidyverse)
library(tictoc)
library(dbarts)
library(ceterisParibus)

#repeldata::repel_local_download()
conn <- repeldata::repel_local_conn()

# function for bart model predict
bart_predict <- function(model, newdata) {
  apply(predict(model, newdata), 2, mean)
}

# Baseline ----------------------------------------------------------------
#TODO - define seprate outcomes - binary versus count. keep in mind disease_status can be present but with NA cases (so you need to lag disease status)
#TODO - WAHIS: if disease is present and absent, select only present (hosts table)

sample_size <- 150
model_object <- nowcast_baseline_model()
newdata <- repel_cases_train(conn) %>%
  select(all_of(grouping_vars)) %>%
  slice(sample(1:nrow(.), size = sample_size))

augmented_data <- repel_augment(model_object = model_object, conn = conn, newdata = newdata)
assertthat::are_equal(sample_size, nrow(augmented_data))

predicted_data <- repel_predict(model_object = model_object, augmented_data = augmented_data)

repel_forecast(model_object = model_object, conn = conn, newdata = newdata)

scored_data <- repel_score(model_object = model_object, augmented_data = augmented_data)

# BART Prep --------------------------------------------------------------------
model_object <- nowcast_bart_model()

traindat <- repel_cases_train(conn) %>%
  drop_na(cases) %>%
  select(all_of(grouping_vars))

# summary table of taxa by disease - looking at sheep/goat combo
# traindat %>%
#   group_by(taxa, disease) %>%
#   count()

# BART model to predict presence/abense --------------------------------------------------------------------
# https://github.com/vdorie/dbarts/issues/12

augmented_data <- repel_augment(model_object = model_object, conn = conn, traindat = traindat, binary_outcome = TRUE) %>%
  arrange(country_iso3c, disease, taxa, report_year, report_semester)

bart_mod <- bart2(formula = disease_status ~ .,
                  data = augmented_data,
                  test = select(augmented_data, -disease_status),
                  keepTrees = TRUE,
                  verbose = TRUE)

# generated 2020-06-19 4:38pm, 12.gb - full augmented training set
# write_rds(bart_mod, "bart_presence_model.rds")
# bart_mod <- read_rds("bart_presence_model.rds")

# posterior means:
bart_mod_means <- apply(pnorm(bart_mod$yhat.train), 3, mean)
hist(bart_mod_means)
bart_mod_eval <- tibble(predicted = bart_mod_means, actual = augmented_data$disease_status) %>%
  mutate(match = round(predicted) == actual)

# percent accuracy
perc <- table(bart_mod_eval$match)
perc[['TRUE']]/nrow(bart_mod_eval)

# below fails to run, but predict should return the same as yhat.train, I think? https://github.com/vdorie/dbarts/issues/12
# bart_predictions <- predict(object = bart_mod, newdata = select(augmented_data, -disease_status))

bartexp <- DALEX::explain(bart_mod,
                          data = select(augmented_data, -disease_status),
                          y = augmented_data$disease_status,
                          predict_function = bart_predict, label = "BART")
write_rds(bartexp, "bart_presence_model_explainer.rds")

# eek Error: cannot allocate vector of size 176.4 Gb
bartcpm <- ceteris_paribus(bartexp,
                           observations = select(augmented_data, -disease_status),
                           y= augmented_data$disease_status)

# Next:
# ICE
# SHAP scores - identify parameters of importance for subsets of data

# BART model to predict counts --------------------------------------------------------------------
augmented_data <- repel_augment(model_object = model_object, conn = conn, traindat = traindat, binary_outcome = FALSE) %>%
  arrange(country_iso3c, disease, taxa, report_year, report_semester)

augmented_data <- augmented_data %>%
  filter(cases>0)

bart_mod <- bart2(formula = cases ~ .,
                  data = augmented_data,
                  test = select(augmented_data, -cases),
                  keepTrees = TRUE,
                  verbose = TRUE)

# write_rds(bart_mod, "bart_count_model.rds")

bart_predictions <- bart_predict(model = bart_mod, newdata = select(augmented_data, -cases))
augmented_data$cases_predicted <- bart_predictions

plot(augmented_data$cases, augmented_data$cases_predicted)

bartexp <- DALEX::explain(bart_mod,
                          data = select(augmented_data, -cases),
                          y = augmented_data$cases,
                          predict_function = bart_predict,
                          label = "BART")

bartcpm <- ceteris_paribus(bartexp,
                           observations = select(augmented_data, -cases),
                           y= augmented_data$cases)


# how well does it do with missing data - score - expect wider interval on NA data

# need validate(model_object, newdata)

# objects loaded in package (load with sys.file)? or plumber api (separate from this package)

# tidymodels - strip down model objects with butcher?

repel_local_disconnect()


