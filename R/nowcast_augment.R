#' define generic augment
#' @export
repel_augment <- function(x, ...){
  UseMethod("repel_augment")
}


#' Augment nowcast baseline model object
#' @import repeldata dplyr tidyr
#' @importFrom assertthat has_name assert_that
#' @export
#'
repel_augment.nowcast_baseline <- function(model_object, conn, traindat) {

  get_nowcast_lag(conn, casedat = traindat) %>%
    select(-cases_lag2, -cases_lag3, -disease_status_lag2, -disease_status_lag3) %>%
    mutate_at(.vars = c("disease_status", "disease_status_lag1"),
              ~recode(., "present" = 1, "suspected" = 1, "absent" = 0)) %>%
    # cases and disease_status can have NAs, but assume 0 for lags
    mutate(disease_status_lag1 = replace_na(disease_status_lag1, 0)) %>%
    mutate(cases_lag1 = replace_na(cases_lag1, 0))

}

#' Augment nowcast bart model object
#'
#' @import repeldata dplyr tidyr
#' @importFrom assertthat has_name assert_that
#' @export
#'
repel_augment.nowcast_bart <- function(model_object, conn, traindat) {

  # get lag cases
  traindat_augment <- get_nowcast_lag(conn, casedat = traindat)

  # get summed lag values of adjacent countries
  borders <- tbl(conn, "connect_static_vars") %>%
    filter(shared_border == "t") %>%
    select(country_origin, country_destination) %>%
    collect()

  traindat_augment_borders <- traindat_augment %>%
    select(all_of(grouping_vars)) %>%
    distinct() %>%
    rename(country_origin = country_iso3c) %>%
    left_join(borders,  by = "country_origin") %>%
    rename(country_iso3c = country_destination)

  borders_augment <- get_nowcast_lag(conn, casedat = traindat_augment_borders)

  borders_sum <- borders_augment %>%
    select(-cases) %>%
    pivot_longer(cols = c(cases_lag1, cases_lag2, cases_lag3)) %>%
    group_by(country_origin, disease, disease_population, taxa, report_year, report_semester) %>%
    summarize(cases_border_countries = sum_na(as.integer(value))) %>%
    ungroup() %>%
    rename(country_iso3c = country_origin)

  traindat_augment <- left_join(traindat_augment, borders_sum, by = grouping_vars)

  # vet capacity
  vets <- tbl(conn, "annual_reports_veterinarians") %>%
    collect() %>%
    group_by(country_iso3c, report_year) %>%
    summarize(veterinarian_count = sum_na(suppressWarnings(as.integer(total_count)))) %>% # summarize over different types of vets
    ungroup() %>%
    mutate(report_year = as.integer(report_year)) %>%
    right_join(expand(traindat_augment, country_iso3c, report_year),  by = c("country_iso3c", "report_year")) %>%
    mutate(veterinarian_count_missing = is.na(veterinarian_count)) %>%
    arrange(country_iso3c, report_year) %>%
    group_split(country_iso3c) %>%
    map_df(~na_interp(., "veterinarian_count")) %>%
    select(-veterinarian_count) %>%
    rename(veterinarian_count = veterinarian_count_imputed)

  traindat_augment <- left_join(traindat_augment, vets, by = c("country_iso3c", "report_year"))

  # taxa population
  taxa <- tbl(conn, "country_taxa_population") %>%
    collect() %>%
    rename(report_year = year, taxa_population = population) %>%
    right_join(expand(traindat_augment, country_iso3c, report_year, taxa),  by = c("country_iso3c", "report_year", "taxa")) %>%
    mutate(taxa_population_missing = is.na(taxa_population)) %>%
    arrange(country_iso3c, taxa, report_year) %>%
    group_split(country_iso3c, taxa) %>%
    map_df(~na_interp(., "taxa_population")) %>%
    select(-taxa_population) %>%
    rename(taxa_population = taxa_population_imputed)

  traindat_augment <- left_join(traindat_augment, taxa, by = c("country_iso3c", "report_year", "taxa"))

  # GDP
  gdp <-  tbl(conn, "country_gdp") %>%
    collect() %>%
    rename(report_year = year) %>%
    right_join(expand(traindat_augment, country_iso3c, report_year),  by = c("country_iso3c", "report_year")) %>%
    mutate(gdp_dollars_missing = is.na(gdp_dollars)) %>%
    arrange(country_iso3c, report_year) %>%
    group_split(country_iso3c) %>%
    map_df(~na_interp(., "gdp_dollars")) %>%
    select(-gdp_dollars) %>%
    rename(gdp_dollars = gdp_dollars_imputed)

  traindat_augment <- left_join(traindat_augment, gdp,  by = c("country_iso3c", "report_year"))

  # handle remaining NAs in vets, taxa pop, gdp
  # map(traindat_augment, ~sum(is.na(.)))

  # remove rows from countries without GDP data?
  removing_gdp  <- traindat_augment %>%
    filter(is.na(gdp_dollars))
  na_countries <- unique(removing_gdp$country_iso3c) %>%
    countrycode::countrycode(., origin = "iso3c", destination = "country.name", warn = FALSE)
  na_countries <- na_countries[!is.na(na_countries)]
  traindat_augment <- traindat_augment %>%
    drop_na(gdp_dollars)
  warning(paste("Dropping", nrow(removing_gdp), "rows of data with missing GDP values from following countries:", paste(na_countries, collapse = ", ")))

  # use neighboring country values for vets and taxa? for now, assume small values
  traindat_augment %>% filter(is.na(taxa_population)) %>% distinct(country_iso3c) %>% pull(country_iso3c) %>% countrycode::countrycode(., origin = "iso3c", destination = "country.name")
  traindat_augment %>% filter(is.na(veterinarian_count)) %>% distinct(country_iso3c) %>% pull(country_iso3c) %>% countrycode::countrycode(., origin = "iso3c", destination = "country.name")
  traindat_augment <- traindat_augment %>%
    mutate(veterinarian_count = ifelse(is.na(veterinarian_count), rpois(sum(is.na(veterinarian_count)), 20), veterinarian_count)) %>%
    mutate(taxa_population = ifelse(is.na(taxa_population), rpois(sum(is.na(taxa_population)), 50), taxa_population))


  # recode disease status
  traindat_augment <- traindat_augment %>%
    mutate_at(.vars = c("disease_status_lag1",  "disease_status_lag2",  "disease_status_lag3"),
              ~recode(., "present" = 1, "suspected" = 1, "absent" = 0, .missing = 0)) %>%
    mutate(disease_status = recode(disease_status, "present" = 1, "suspected" = 1, "absent" = 0))

  # case NAs - replace with 0s?
  case_vars <- c("cases_lag1", "cases_lag2", "cases_lag3", "cases_border_countries")
  for(var in case_vars){
    traindat_augment <- traindat_augment %>%
      mutate(!!paste0(var, "_missing") := is.na(get(var))) %>%
      mutate(!!var := replace_na(get(var), 0))
  }

  # add column to indicate first year country reporting
  traindat_augment <- traindat_augment %>%
    mutate(report_period = as.integer(paste0(report_year, report_semester))) %>%
    group_by(country_iso3c) %>%
    mutate(first_reporting_semester = report_period == min(report_period)) %>%
    ungroup() %>%
    select(-report_period)

  return(traindat_augment)

}


#' Augment nowcast GAM model object
#'
#' @import repeldata dplyr tidyr
#' @importFrom assertthat has_name assert_that
#' @export
#'
repel_augment.nowcast_gam <- function(model_object, conn, newdata, rare = 1000) {
  dat <- repel_augment.nowcast_bart(model_object, conn, newdata, lags = lags)
  dat <- dat %>%
    mutate(condition = factor(paste(dat$disease, taxa, sep = "-"))) %>%
    mutate(condition = fct_lump_min(condition, rare, other_level = "rare_condition")) %>%
    mutate(log10_gdp = log10(gdp_dollars)) %>%
    mutate_at(vars(cases_lag1_missing, cases_border_countries_missing, first_reporting_semester, veterinarian_count_missing, taxa_population_missing, gdp_dollars_missing),
              ~as.numeric(!.))
  dat$cases_lagged <- as.matrix(select(dat, matches("^cases_lag\\d+$")))
  dat <- select(dat, -matches("^cases_lag\\d+$"))
  dat$lags <- matrix(lags, ncol = length(lags), nrow = nrow(dat), byrow = TRUE)
  dat$condition_lagged <- matrix(rep(as.integer(dat$condition), length(lags)), nrow = nrow(dat), ncol = length(lags), byrow = FALSE)
  return(dat)
}

#' Adds more NA handling functionality to imputeTS::na_interpolation
#' @import dplyr tidyr
#' @importFrom imputeTS na_interpolation
#' @noRd
na_interp <- function(df, var){
  if(sum(!is.na(df[,var])) == 0){
    out <- mutate(df, !!paste0(var, "_imputed") := NA_integer_)
  }
  if(sum(!is.na(df[,var])) == 1){
    out <- mutate(df,  !!paste0(var, "_imputed") := get(var)[!is.na(get(var))])
  }
  if(sum(!is.na(df[,var])) > 1){
    out <- mutate(df,  !!paste0(var, "_imputed") := imputeTS::na_interpolation(get(var)))
  }
  return(out)
}


#'@noRd
modify_augmented_data <- function(augmented_data, outcome_var){

  stopifnot(outcome_var %in% c("disease_status", "cases"))

  if(outcome_var == "disease_status"){
    modified_data <- augmented_data %>%
      select(-cases)
  }
  if(outcome_var == "cases"){
    modified_data <- augmented_data %>%
      select(-disease_status) %>%
      drop_na(cases) %>%
      filter(cases > 0)
  }
  return(modified_data)
}




