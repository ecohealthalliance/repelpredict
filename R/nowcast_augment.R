#' define generic augment
#' @export
repel_augment <- function(x, ...){
  UseMethod("repel_augment")
}


#' Augment nowcast baseline model object
#' @import repeldata dplyr tidyr
#' @importFrom assertthat has_name assert_that
#' @export
repel_augment.nowcast_baseline <- function(model_object, conn, traindat) {

  lagged_dat <- get_nowcast_lag(conn, casedat = traindat, lags = 1) %>%
    mutate_at(names(.)[str_detect(names(.), "disease_status")],  ~recode(., "present" = 1, "suspected" = 1, "absent" = 0))

  lag_vars <- colnames(lagged_dat)[str_detect(names(lagged_dat), "disease_status_lag|cases_lag")]
  for(var in lag_vars){
    lagged_dat <- lagged_dat %>%
      mutate(!!paste0(var, "_missing") := is.na(get(var))) %>%
      mutate_at(var, ~replace_na(., 0))
  }

  return(lagged_dat)
}

#' Augment nowcast bart model object
#'
#' @import repeldata dplyr tidyr
#' @importFrom assertthat has_name assert_that
#' @export
repel_augment.nowcast_bart <- function(model_object, conn, traindat) {

  # get lag cases
  lagged_dat <- get_nowcast_lag(conn, casedat = traindat, lags = 1:3)

  # get summed lag values of adjacent countries
  borders <- tbl(conn, "connect_static_vars") %>%
    filter(shared_border == "t") %>%
    select(country_origin, country_destination) %>%
    collect()

  lagged_borders <- lagged_dat %>%
    select(all_of(grouping_vars)) %>%
    distinct() %>%
    rename(country_origin = country_iso3c) %>%
    left_join(borders,  by = "country_origin") %>%
    rename(country_iso3c = country_destination) %>%
    get_nowcast_lag(conn, casedat = ., lags = 1:3)

  lagged_borders_sum <- lagged_borders %>%
    select(-cases) %>%
    pivot_longer(cols = c(cases_lag1, cases_lag2, cases_lag3)) %>%
    group_by(country_origin, disease, disease_population, taxa, report_year, report_semester) %>%
    summarize(cases_border_countries = sum_na(as.integer(value))) %>%
    ungroup() %>%
    rename(country_iso3c = country_origin)

  lagged_dat <- left_join(lagged_dat, lagged_borders_sum, by = grouping_vars)

  # vet capacity
  vets <- tbl(conn, "annual_reports_veterinarians") %>%
    collect() %>%
    filter(veterinarian_field %in% c(
      "animal health and welfare activities",
      "veterinary public health activities",
      "laboratories",
      "private clinical practice",
      "academic activities and education",
      "pharmaceutical industry"
    )) %>%
    group_by(country_iso3c, report_year) %>%
    summarize(veterinarian_count = sum_na(suppressWarnings(as.integer(total_count)))) %>% # summarize over different types of vets
    ungroup() %>%
    mutate(report_year = as.integer(report_year)) %>%
    right_join(expand(lagged_dat, country_iso3c, report_year),  by = c("country_iso3c", "report_year")) %>%
    mutate(veterinarian_count_missing = is.na(veterinarian_count)) %>%
    arrange(country_iso3c, report_year) %>%
    group_split(country_iso3c) %>%
    map_dfr(~na_interp(., "veterinarian_count")) %>%
    select(-veterinarian_count) %>%
    rename(veterinarian_count = veterinarian_count_imputed)

  lagged_dat <- left_join(lagged_dat, vets, by = c("country_iso3c", "report_year"))

  # taxa population
  taxa <- tbl(conn, "country_taxa_population") %>%
    collect() %>%
    rename(report_year = year, taxa_population = population) %>%
    right_join(expand(lagged_dat, country_iso3c, report_year, taxa),  by = c("country_iso3c", "report_year", "taxa")) %>%
    mutate(taxa_population_missing = is.na(taxa_population)) %>%
    arrange(country_iso3c, taxa, report_year) %>%
    group_split(country_iso3c, taxa) %>%
    map_dfr(~na_interp(., "taxa_population")) %>%
    select(-taxa_population) %>%
    rename(taxa_population = taxa_population_imputed)

  lagged_dat <- left_join(lagged_dat, taxa, by = c("country_iso3c", "report_year", "taxa"))

  # World Bank indicators
  wbi <-  tbl(conn, "worldbank_indicators") %>%
    collect() %>%
    rename(report_year = year) %>%
    right_join(expand(lagged_dat, country_iso3c, report_year),  by = c("country_iso3c", "report_year")) %>%
    mutate(gdp_dollars_missing = is.na(gdp_dollars),
           human_population_missing = is.na(human_population)) %>%
    arrange(country_iso3c, report_year) %>%
    group_split(country_iso3c) %>%
    map_dfr(~na_interp(., "gdp_dollars") %>%
              na_interp(., "human_population")) %>%
    select(-gdp_dollars,
           -human_population) %>%
    rename(gdp_dollars = gdp_dollars_imputed,
           human_population = human_population_imputed)

  lagged_dat <- left_join(lagged_dat, wbi,  by = c("country_iso3c", "report_year"))

  # remove rows from countries without GDP data?
  removing_gdp  <- lagged_dat %>%
    filter(is.na(gdp_dollars))
  na_countries <- unique(removing_gdp$country_iso3c) %>%
    countrycode::countrycode(., origin = "iso3c", destination = "country.name", warn = FALSE)
  na_countries <- na_countries[!is.na(na_countries)]
  lagged_dat <- lagged_dat %>%
    drop_na(gdp_dollars)
  warning(paste("Dropping", nrow(removing_gdp), "rows of data with missing GDP values from following countries:", paste(na_countries, collapse = ", ")))

  # recode disease status
  lagged_dat <- lagged_dat %>%
    mutate_at(names(.)[str_detect(names(.), "disease_status")],  ~recode(., "present" = 1, "suspected" = 1, "absent" = 0))

  lag_vars <- colnames(lagged_dat)[str_detect(names(lagged_dat), "disease_status_lag|cases_lag|cases_border_countries")]
  for(var in c(lag_vars)){
    lagged_dat <- lagged_dat %>%
      mutate(!!paste0(var, "_missing") := is.na(get(var))) %>%
      mutate_at(var, ~replace_na(., 0))
  }

  # add column to indicate first year country reporting
  lagged_dat <- lagged_dat %>%
    mutate(report_period = as.integer(paste0(report_year, report_semester))) %>%
    group_by(country_iso3c) %>%
    mutate(first_reporting_semester = report_period == min(report_period)) %>%
    ungroup() %>%
    select(-report_period)

  return(lagged_dat)

}


#' Augment nowcast GAM model object
#'
#' @import repeldata dplyr tidyr
#' @importFrom assertthat has_name assert_that
#' @export
#'
repel_augment.nowcast_gam <- function(model_object, conn, newdata, rare = 1000) {
  dat <- repel_augment.nowcast_bart(model_object, conn, newdata)
  dat <- dat %>%
    mutate(condition = factor(paste(dat$disease, taxa, sep = "-"))) %>%
    mutate(condition = fct_lump_min(condition, rare, other_level = "rare_condition")) %>%
    mutate(log10_gdp = log10(gdp_dollars)) %>%
    mutate_at(vars(cases_lag1_missing, cases_border_countries_missing, first_reporting_semester, veterinarian_count_missing, taxa_population_missing, gdp_dollars_missing),
              ~as.numeric(!.))
  dat$cases_lagged <-
    as.matrix(select(dat, matches("^cases_lag\\d+$")))
  dat <- select(dat, -matches("^cases_lag\\d+$"))
  dat$lags <- matrix(seq_len(ncol(dat$cases_lagged)), ncol = ncol(dat$cases_lagged), nrow = nrow(dat), byrow = TRUE)
  dat$condition_lagged <- matrix(rep(as.integer(dat$condition), ncol(dat$cases_lagged)), nrow = nrow(dat), ncol = ncol(dat$cases_lagged), byrow = FALSE)
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
      select(-cases) %>%
      drop_na(disease_status)
  }
  if(outcome_var == "cases"){
    modified_data <- augmented_data %>%
      select(-disease_status) %>%
      drop_na(cases)
    # filter(cases > 0)
  }
  return(modified_data)
}




