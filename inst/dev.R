devtools::load_all()
library(tidyverse)
library(tictoc)
library(dbarts)
library(ceterisParibus)
set.seed(99)

#repeldata::repel_local_download()
conn <- repeldata::repel_local_conn()

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

predicted_data <- repel_predict(model_object = model_object, augmented_data = augmented_data)

repel_forecast(model_object = model_object, conn = conn, newdata = newdata)

scored_data <- repel_score(model_object = model_object, augmented_data = augmented_data)

# BART --------------------------------------------------------------------
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
non_case_vars <-  c("veterinarian_count", "taxa_population", "gdp_dollars")
for(var in non_case_vars){

  augmented_data <- augmented_data %>%
    mutate(!!paste0(var, "_missing") := is.na(get(var)))

  if(var=="taxa_population"){
    select_vars <- c("country_iso3c", "report_year", "report_semester", "taxa", var)
    group_vars <- c("country_iso3c", "taxa")
    join_vars <- c("country_iso3c", "report_year", "report_semester", "taxa")
  }else{
    select_vars <- c("country_iso3c", "report_year", var)
    group_vars <- c("country_iso3c")
    join_vars <- c("country_iso3c", "report_year")
  }

  interp <- augmented_data %>%
    arrange(country_iso3c, report_year, report_semester, taxa) %>%
    select(all_of(select_vars)) %>%
    distinct() %>%
    group_by_at(group_vars) %>%
    filter(sum(!is.na(get(var))) > 1) %>%
    mutate(!!var := imputeTS::na_interpolation(get(var))) %>%
    ungroup()

  augmented_data <- left_join(augmented_data, interp, by = join_vars) %>%
    mutate(!!var := coalesce(get(paste0(var, ".x")), get(paste0(var, ".y")))) %>%
    select(-ends_with(".x"), -ends_with(".y"))
}

# handling remaining NAs in these vars
map(augmented_data, ~sum(is.na(.)))
# remove rows from countries without GDP data
augmented_data <- augmented_data %>%
  drop_na(gdp_dollars)
# use neighboring country values? for now, assume small values
augmented_data %>% filter(is.na(taxa_population)) %>% distinct(country_iso3c) %>% pull(country_iso3c) %>% countrycode::countrycode(., origin = "iso3c", destination = "country.name")
augmented_data %>% filter(is.na(veterinarian_count)) %>% distinct(country_iso3c) %>% pull(country_iso3c) %>% countrycode::countrycode(., origin = "iso3c", destination = "country.name")
augmented_data <- augmented_data %>%
  mutate(veterinarian_count = ifelse(is.na(veterinarian_count), rpois(sum(is.na(veterinarian_count)), 20), veterinarian_count)) %>%
  mutate(taxa_population = ifelse(is.na(taxa_population), rpois(sum(is.na(taxa_population)), 50), taxa_population))

# case NAs - replace with 0s?
case_vars <- c("cases_lag1", "cases_lag2", "cases_lag3", "cases_border_countries")
for(var in case_vars){
  augmented_data <- augmented_data %>%
    mutate(!!paste0(var, "_missing") := is.na(get(var)))
}
augmented_data <- augmented_data %>%
  mutate_at(.vars = c("cases_lag1", "cases_lag2", "cases_lag3", "cases_border_countries"), ~replace_na(., 0))

# add column to indicate first year country reporting
augmented_data <- augmented_data %>%
  mutate(report_period = as.integer(paste0(report_year, report_semester))) %>%
  group_by(country_iso3c) %>%
  mutate(first_reporting_semester = report_period == min(report_period)) %>%
  ungroup() %>%
  select(-report_period)

# for now, do predictions for top 20 diseases
top_diseases <- augmented_data %>%
  group_by(disease) %>%
  count(sort = TRUE) %>%
  slice(1:20)
augmented_data <- augmented_data %>%
  filter(disease %in% top_diseases$disease) %>%
  mutate(disease_status = as.logical(disease_status))

# BART model to predict presence/abense - https://github.com/vdorie/dbarts/issues/12
bart_mod <- bart2(formula = disease_status ~ ., data = augmented_data, test = select(augmented_data, -disease_status),
              verbose = TRUE)

write_rds(bart_mod, "bart_presence_model.rds")

# to transform to probabilities:
# pnorm(bart_mod$yhat.train)
# posterior means:
# apply(pnorm(bart_mod$yhat.train), 3, mean)
# test against predictions using the predict function:
mean(abs(bart_mod$yhat.train - predict(bart_mod, select(augmented_data, -disease_status))))


# function for bart model predict
bart_predict <- function(model, newdat) {
  apply(predict(object = model, test = newdat), 2, mean)
}

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

# hurdle model
# flter data to >0 cases, predict off this (0s become -0.5, log cases)

# how well does it do with missing data - score - expect wider interval on NA data

# baseline needs binary and numeric value

# need validate(model_object, newdata)

# objects loaded in package (load with sys.file)? or plumber api (separate from this package)

# tidymodels - strip down model objects with butcher?


