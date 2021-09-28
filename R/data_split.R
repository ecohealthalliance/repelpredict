taxa_list <- c( "sheep/goats",
                "cattle", "birds", "camelidae", "dogs", "equidae",
                "cats", "cervidae" , "swine" , "buffaloes", "hares/rabbits")

grouping_vars <- c("country_iso3c", "report_year", "report_semester", "disease", "disease_population", "taxa")

#' @export
repel_split <- function(x, ...){
  UseMethod("repel_split")
}

#' @import repeldata dplyr tidyr readr
#' @importFrom DBI dbDisconnect
#' @importFrom vroom vroom
#' @importFrom here here
#' @export
repel_split.nowcast_model <- function(model_object, conn, clean_disease_names = TRUE){

  # read in static file from inst/nowcast_generate_data_split_lookup.R
  validation_split <- vroom::vroom(system.file("lookup", "nowcast_validation_split_lookup.csv.gz", package = "repelpredict"),
                               col_types = cols(
                                 country_iso3c = col_character(),
                                 taxa = col_character(),
                                 disease = col_character(),
                                 disease_population = col_character(),
                                 report_year = col_integer(),
                                 report_semester = col_integer(),
                                 validation_set = col_logical()
                               ))

  all_dat <- repel_init(model_object, conn) %>%
    arrange(country_iso3c, taxa, disease, disease_population, report_year, report_semester) %>%
    left_join(validation_split,  by = c("country_iso3c", "report_year", "report_semester", "taxa", "disease", "disease_population"))

  assert_that(!any(is.na(all_dat$validation_set)))  # if this fails, rerun inst/nowcast_generate_data_split_lookup.R

  if(clean_disease_names){
    # from inst/network_generate_disease_lookup.R
    diseases_recode <- vroom::vroom(system.file("lookup", "nowcast_diseases_recode.csv",  package = "repelpredict"), col_types = cols(
      disease = col_character(),
      disease_recode = col_character()
    ))
    all_dat <- all_dat %>%
      left_join(diseases_recode, by = "disease") %>%
      select(-disease) %>%
      rename(disease = disease_recode)
    assertthat::assert_that(!any(is.na(unique(all_dat$disease)))) # if this fails, rerun inst/nowcast_generate_disease_lookup.R
  }

  return(all_dat)
}

#' Lookup train/val split for network model data
#' @import repeldata dplyr tidyr readr
#' @importFrom DBI dbDisconnect
#' @importFrom vroom vroom
#' @importFrom here here
#' @param model_object network model object
#' @param conn connection to repel db
#' @export
repel_split.network_model <- function(model_object, conn){

  # read in static file from inst/network_generate_data_split_lookup.R
  validation_split <- vroom::vroom(system.file("lookup", "network_validation_split_lookup.csv.gz", package = "repelpredict"),
                               col_types = cols(
                                 country_iso3c = col_character(),
                                 disease = col_character(),
                                 month = col_date(format = ""),
                                 validation_set = col_logical()
                               )) %>%
    repel_clean_disease_names(model_object, .) %>%
    select(-disease_name_uncleaned)

  all_dat <- repel_init(model_object, conn,
                        outbreak_reports_events = NULL,
                        remove_single_country_disease = TRUE,
                        remove_non_primary_taxa_disease = TRUE) %>%
    arrange(country_iso3c, disease, month) %>%
    left_join(validation_split,  by = c("country_iso3c", "disease", "month"))

  assert_that(!any(is.na(all_dat$validation_set))) # if this fails, rerun inst/network_generate_data_split_lookup.R

  return(all_dat)
}

#' @import repeldata dplyr tidyr readr
#' @importFrom DBI dbDisconnect
#' @importFrom vroom vroom
#' @importFrom here here
#' @export
repel_split.impact_model <- function(model_object, conn){
  NULL
}

#' @export
repel_training <- function(x, ...){
  UseMethod("repel_training")
}

#' Get cases training set (~80%)
#' @param model_object network model object
#' @param conn connection to repel db
#' @import repeldata dplyr tidyr
#' @return a tibble
#' @export
repel_training.repel_model <- function(model_object, conn){
  all_dat <- repel_split(model_object, conn)
  message(paste0("validation set is ", round(100*sum(all_dat$validation_set)/nrow(all_dat)), "% of data"))
  train_dat <- all_dat %>%
    filter(!validation_set) %>%
    select(-validation_set)

  # Adding this check out of curiosity--it may not be necessary
  if(n_distinct(all_dat$country_iso3c) != n_distinct(train_dat$country_iso3c)){
    warning(paste(n_distinct(all_dat$country_iso3c) - n_distinct(train_dat$country_iso3c),"country iso3c(s) are not represented in training dataset"))
  }
  return(train_dat)
}


#' @export
repel_validation <- function(x, ...){
  UseMethod("repel_validation")
}

#' Hold out validation set (~20%)
#' @param model_object network model object
#' @param conn connection to repel db
#' @import repeldata dplyr tidyr
#' @return a tibble
#' @export
repel_validation.repel_model <- function(model_object, conn){
  val_dat <- repel_split(model_object, conn) %>%
    filter(validation_set) %>%
    select(-validation_set)
  return(val_dat)
}
