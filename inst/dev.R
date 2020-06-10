devtools::load_all()
library(tidyverse)
library(tictoc)
library(dbarts)
library(ceterisParibus)
set.seed(99)

#repeldata::repel_local_download()
conn <- repeldata::repel_local_conn()

# function for bart model predict
bart_predict <- function(model, newdata) {
  apply(predict(object = model, test = newdata), 2, mean)
}

# Baseline ----------------------------------------------------------------
sample_size <- 150
model_object <- nowcast_baseline_model()
newdata <- repel_cases_train(conn) %>%
  select("country_iso3c", "report_year", "report_semester", "disease", "taxa") %>%
  slice(sample(1:nrow(.), size = sample_size))

augmented_data <- repel_augment(model_object = model_object, conn = conn, newdata = newdata)
assertthat::are_equal(sample_size, nrow(augmented_data))
# TODO ^ NEED TO HANDLE DUPEs (related to serotypes and disease populations)
# diseases should be either wild or domestic. depends on what they are predominantly classified as. default to domestic.
# later in UI - have a menu selection to allow user to choose from available population
# split out serotypes (avian influenzas can be very diff)
# default to predict separately on serotypes (in interface)
# NEED TO CLEAN SEROTYPE FIELD

predicted_data <- repel_predict(model_object = model_object, augmented_data = augmented_data)

repel_forecast(model_object = model_object, conn = conn, newdata = newdata)

scored_data <- repel_score(model_object = model_object, augmented_data = augmented_data)

# BART Prep --------------------------------------------------------------------
model_object <- nowcast_bart_model()

traindat <- repel_cases_train(conn) %>%
  mutate(cases = ifelse(disease_status == "absent", 0, cases)) %>% # this is temporary
  drop_na(cases) %>%
  select("country_iso3c", "report_year", "report_semester", "disease", "taxa", "disease_status")

# summary table of taxa by disease - looking at sheep/goat combo
# traindat %>%
#   group_by(taxa, disease) %>%
#   count()

augmented_data <- repel_augment(model_object = model_object, conn = conn, traindat = traindat, binary_outcome = TRUE) %>%
  arrange(country_iso3c, disease, taxa, report_year, report_semester)

## Imputation heuristics (to be moved to augment)
# use trend lines for veterinarians, taxa population, gdp
# if the NA is first or last in series, use next or previous value
# if it's in the middle of the time series, linear interpolation


# for now, do predictions for top 20 diseases
top_diseases <- augmented_data %>%
  group_by(disease) %>%
  count(sort = TRUE) %>%
  slice(1:20)

augmented_data <- augmented_data %>%
  filter(disease %in% top_diseases$disease) %>%
  mutate(disease_status = as.logical(disease_status))

# BART model to predict presence/abense --------------------------------------------------------------------
# https://github.com/vdorie/dbarts/issues/12
bart_mod <- bart2(formula = disease_status ~ ., data = augmented_data, test = select(augmented_data, -disease_status),
                  verbose = TRUE)
write_rds(bart_mod, "bart_presence_model.rds")

# to transform to probabilities:
# pnorm(bart_mod$yhat.train)
# posterior means:
# apply(pnorm(bart_mod$yhat.train), 3, mean)
# test against predictions using the predict function:
mean(abs(bart_mod$yhat.train - predict(bart_mod, select(augmented_data, -disease_status))))


bart_predictions <- predict(object = bart_mod, test = select(augmented_data, -disease_status))

bartexp <- DALEX::explain(bart_mod,
                          data = select(augmented_data, -disease_status),
                          y = augmented_data$disease_status,
                          predict_function = bart_predict, label = "BART")
write_rds(bartexp, "bart_presence_model_explainer.rds")

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
  filter(cases>0) %>%
  mutate_if(is.numeric, ~log(replace_na(na_if(., 0), 0.5)))

bart_mod <- bart(x.train = select(augmented_data, -cases),
                 y.train = pull(augmented_data, cases),
                 verbose = TRUE,
                 keeptrees = TRUE)

plot(bart_mod)

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

# baseline needs binary and numeric value

# need validate(model_object, newdata)

# objects loaded in package (load with sys.file)? or plumber api (separate from this package)

# tidymodels - strip down model objects with butcher?

repel_local_disconnect()


