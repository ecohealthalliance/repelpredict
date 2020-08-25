taxa_list <- c("goats", "sheep",  "sheep/goats", # handling for multiple species, sheep/goats (combine all sheep goats?)
               "cattle", "birds", "camelidae", "dogs", "equidae",
               "cats", "cervidae" , "swine" , "buffaloes", "hares/rabbits")

grouping_vars <- c("country_iso3c", "report_year", "report_semester", "disease", "disease_population", "taxa")

#' @import repeldata dplyr tidyr
#' @importFrom DBI dbDisconnect
#' @importFrom digest digest2int
#' @noRd
repel_cases <- function(conn){
  set.seed(8734)
  all_dat <- tbl(conn, "annual_reports_animal_hosts") %>%
    filter(taxa %in% taxa_list) %>%
    filter(report_semester != "0") %>%
    select(report, country, country_iso3c, report_year, report_semester, taxa, disease, disease_population, disease_status, cases) %>%
    collect() %>%
    mutate_at(.vars = c("report_year", "report_semester", "cases"), ~suppressWarnings(as.integer(.))) %>%
    arrange(country_iso3c, taxa, disease, disease_population, report_year, report_semester) %>%
    group_by(country_iso3c, taxa, disease, disease_population) %>%
    mutate(validation_set = row_number() %in% sample(n(), ceiling(0.2*n()), replace = FALSE)) %>%
    ungroup()
  return(all_dat)
  }

#' Get cases training set (~80%)
#' @import repeldata dplyr tidyr
#' @return a tibble
#' @export
repel_cases_train <- function(conn){
 all_dat <- repel_cases(conn)
 message(paste0("validation set is ", round(100*sum(all_dat$validation_set)/nrow(all_dat)), "% of data"))
 train_dat <- all_dat %>%
    filter(!validation_set) %>%
    select(-validation_set)
 # Adding this check out of curiosity--it may not be necessary
 if(n_distinct(all_dat$country) != n_distinct(train_dat$country)){
   warning("Not all countries are represented in training dataset")
 }
 return(train_dat)
}


#' Hold out validation set (~20%)
#' @import repeldata dplyr tidyr
#' @return a tibble
#' @export
repel_cases_validate <- function(conn){
  repel_cases(conn) %>%
    filter(validation_set) %>%
    select(-validation_set)
}
