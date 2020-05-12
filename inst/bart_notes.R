
conn <- repel_remote_conn()

library(dbarts)

fit_nowcast_bart <- function(conn){

  traindat <- repel_cases_train(conn)

  # generate lag values (using augument) for last 3 semesters (NA if missing) -> mean imputation if missing

  # get summed lag values of adjacent countries

  # measures of vet capacity - counts

  # population of taxa (maybe from FAO?)

  # GDP (worldbank)

  # call dbart package - predict - return list structure with class  (class(obj), nowcast_bart, nowcast_model) (in this order)
  # list contains model object, other slots for anything else (eg metadata)

  # score(model_object)

  # validate(model_object, newdata)

}

# model object saved as .rds
# drake? (later)
# objects loaded in package (load with sys.file)? or plumber api (separate from this package)
# tidymodels - strip down model objects with butcher?

