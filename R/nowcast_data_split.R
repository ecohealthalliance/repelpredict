taxa_list <- c( "sheep/goats",
                "cattle", "birds", "camelidae", "dogs", "equidae",
                "cats", "cervidae" , "swine" , "buffaloes", "hares/rabbits")

grouping_vars <- c("country_iso3c", "report_year", "report_semester", "disease", "disease_population", "taxa")

#' @import repeldata dplyr tidyr readr
#' @importFrom DBI dbDisconnect
#' @importFrom readr read_csv
#' @importFrom here here
#' @noRd
split_annual_reports_animal_hosts <- function(conn, clean_disease_names = TRUE){

  # read in static file from inst/generate_data_split_lookup.R
  validation_split <- read_csv(system.file("lookup", "validation_split_lookup.csv.gz", package = "repelpredict"),
                               col_types = cols(
                                 country_iso3c = col_character(),
                                 taxa = col_character(),
                                 disease = col_character(),
                                 disease_population = col_character(),
                                 report_year = col_integer(),
                                 report_semester = col_integer(),
                                 validation_set = col_logical()
                               ))

  all_dat <- init_annual_reports_animal_hosts(conn) %>%
    arrange(country_iso3c, taxa, disease, disease_population, report_year, report_semester) %>%
    left_join(validation_split,  by = c("country_iso3c", "report_year", "report_semester", "taxa", "disease", "disease_population"))

  assert_that(!any(is.na(all_dat$validation_set)))

  if(clean_disease_names){
    diseases_recode <- read_csv(system.file("lookup", "diseases_recode.csv",  package = "repelpredict"), col_types = cols(
      disease = col_character(),
      disease_recode = col_character()
    ))
    all_dat <- all_dat %>%
      left_join(diseases_recode, by = "disease") %>%
      select(-disease) %>%
      rename(disease = disease_recode)
    assertthat::assert_that(!any(is.na(unique(all_dat$disease))))
  }

  return(all_dat)
}

#' Get cases training set (~80%)
#' @import repeldata dplyr tidyr
#' @return a tibble
#' @export
annual_reports_animal_hosts_training <- function(conn){
  all_dat <- split_annual_reports_animal_hosts(conn)
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


#' Hold out validation set (~20%)
#' @import repeldata dplyr tidyr
#' @return a tibble
#' @export
annual_reports_animal_hosts_validation <- function(conn){
  val_dat <- split_annual_reports_animal_hosts(conn) %>%
    filter(validation_set) %>%
    select(-validation_set)
  return(val_dat)
}
