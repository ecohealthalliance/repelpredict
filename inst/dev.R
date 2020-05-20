devtools::load_all()
library(tictoc)

conn <- repel_local_conn()

# Baseline ----------------------------------------------------------------
sample_size <- 150
model_object <- nowcast_baseline_model()
newdata <- repel_cases_train(conn) %>%
  select("country_iso3c", "report_year", "report_semester", "disease", "taxa") %>%
  slice(sample(1:nrow(.), size = sample_size))

augmented_data <- repel_augment.nowcast_baseline(model_object = model_object, conn = conn, newdata = newdata)
assertthat::are_equal(sample_size, nrow(augmented_data)) # TODO ^ NEED TO HANDLE DUPEs (related to serotypes and disease populations, I think. Should be included in primary grouping vars)

predicted_data <- repel_predict.nowcast_baseline(model_object = model_object, augmented_data = augmented_data)

scored_data <- repel_score.nowcast_baseline(model_object = model_object, augmented_data = augmented_data)

repel_forecast(model_object = model_object, conn = conn, newdata = newdata)


# BART --------------------------------------------------------------------

traindat <- repel_cases_train(conn) %>%
  mutate(cases = ifelse(disease_status == "absent", 0, cases)) %>% # this is temporary
  drop_na(cases)

#summary table of taxa by disease - looking at sheep/goat combo - forecast needs assertion on taxa type (could propogate from elsewhere) - in error slot of forecast (400 code)
traindat %>%
  group_by(taxa, disease) %>%
  count() %>%
  View

traindat_prepped <- nowcast_prep_bart(traindat = traindat, conn)


# #TODO - NA handling  - taxa fromFAO, most recent value for vets
#
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


