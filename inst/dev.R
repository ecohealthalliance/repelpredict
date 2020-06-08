devtools::load_all()
library(tictoc)
library(dbarts)
library(ceterisParibus)
#repeldata::repel_local_download()
conn <- repeldata::repel_local_conn()

# Baseline ----------------------------------------------------------------
sample_size <- 150
model_object <- nowcast_baseline_model()
newdata <- repel_cases_train(conn) %>%
  select("country_iso3c", "report_year", "report_semester", "disease", "taxa") %>%
  slice(sample(1:nrow(.), size = sample_size))

augmented_data <- repel_augment(model_object = model_object, conn = conn, newdata = newdata)
assertthat::are_equal(sample_size, nrow(augmented_data)) # TODO ^ NEED TO HANDLE DUPEs (related to serotypes and disease populations, I think. Should be included in primary grouping vars)
# default to domestic. by disease, predominantly domestic or wild
# maybe menu selection - specify avaialble population
# split out serotypes - avian influenzas can be very diff - default to predict on both (in interface)

predicted_data <- repel_predict(model_object = model_object, augmented_data = augmented_data)

repel_forecast(model_object = model_object, conn = conn, newdata = newdata)

scored_data <- repel_score(model_object = model_object, augmented_data = augmented_data)

# BART --------------------------------------------------------------------
model_object <- nowcast_bart_model()

traindat <- repel_cases_train(conn) %>%
  mutate(cases = ifelse(disease_status == "absent", 0, cases)) %>% # this is temporary
  drop_na(cases) %>%
  select("country_iso3c", "report_year", "report_semester", "disease", "taxa", "disease_status")

# summary table of taxa by disease - looking at sheep/goat combo - forecast needs assertion on taxa type (could propogate from elsewhere) - in error slot of forecast (400 code)
traindat %>%
  group_by(taxa, disease) %>%
  count()

augmented_data <- repel_augment(model_object = model_object, conn = conn, traindat = traindat)

# BART model to predict presence/abense
aug_dat1 <- augmented_data %>%
  select(-cases, -report_period) %>%
  mutate(disease_status = recode(disease_status, "present" = 1, suspected = 1, absent = 0))

bart_mod1 <- bart(select(aug_dat1, -disease_status),
                 aug_dat1$disease_status,
                 ndpost=1000,
                 keeptrees = TRUE)

# function for bart model predict
bart_predict <- function(model, newdat) {
  apply(predict(object = model, test = newdat), 2, mean)
}

bartexp <- DALEX::explain(bart_mod1, data = aug_dat1, y = aug_dat1$disease_status,
                          predict_function = bart_predict, label = "BART")
bartcpm <- ceteris_paribus(bartexp, observations = aug_dat1, y =aug_dat1$disease_status)

# ICE
# SHAP scores - identify parameters of importance for subsets of data

# hurdle model?
# flter data to >0 cases, predict off this - 0s become -0.5

# impute in some way (0, mean, filling in missing) - add another column present/absent, first year reporting...
# gam = rndom effects for missing value flags. bart doesnt but should learn this
# missing values and flags in augment
# how well does it do with missing data - score - widert interval on NA data
# hurdle removes 0, have it predict log of number of cases


# # sep models for present+suspected/absent and for counts
# # prediciton on present/suspected, if it is, then number of log(cases) - 0s filtered out
#
# traindat_augment_mod <- traindat_augment %>%
#   mutate_at(.vars = c("cases", "cases_lag1", "cases_lag2", "cases_lag3"), ~as.numeric(.)) %>%
#   mutate_if(.predicate = is.character, ~as.factor(.)) %>%
#   select(-report_year, -report_period) %>%
#   drop_na(cases)

#model object is list of two objects - augment to add both outcomes - can predict off of either model - score true values error (needed for baseline too)
# baseline needs binary and numeric value


# call dbart package - predict - return list structure with class  (class(obj), nowcast_bart, nowcast_model) (in this order)
# list contains model object, other slots for anything else (eg metadata)

# score(model_object)

# validate(model_object, newdata)

# model object saved as .rds
# drake? (later)
# objects loaded in package (load with sys.file)? or plumber api (separate from this package)
# tidymodels - strip down model objects with butcher?


