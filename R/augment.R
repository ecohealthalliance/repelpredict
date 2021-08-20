#' define generic augment
#' @export
repel_augment <- function(x, ...){
  UseMethod("repel_augment")
}


#' Augment nowcast baseline model object
#' @import repeldata dplyr tidyr
#' @importFrom assertthat has_name assert_that
#' @importFrom janitor make_clean_names
#' @importFrom purrr map_dfr
#' @export
repel_augment.nowcast_baseline <- function(model_object, conn, newdata) {

  lagged_newdata <- repel_lag(model_object, conn, newdata, lags = 1, control_measures = FALSE)

  ds_names <- names(lagged_newdata)[str_detect(names(lagged_newdata), "disease_status")]
  for(ds in ds_names){
    lagged_newdata <- lagged_newdata %>%
      mutate(!!paste0(ds, "_unreported") := as.integer(!!sym(ds)  == "unreported"))
  }
  lagged_newdata <- lagged_newdata %>%
    mutate_at(ds_names, ~as.integer(recode(., "present" = 1, "unreported" = 0, "absent" = 0)))

  lag_vars <- colnames(lagged_newdata)[str_detect(names(lagged_newdata), "disease_status_lag|cases_lag")]
  lag_vars <- lag_vars[!lag_vars %in% "disease_status_lag1_unreported"]
  for(var in lag_vars){
    lagged_newdata <- lagged_newdata %>%
      mutate(!!paste0(var, "_missing") := as.integer(is.na(get(var)))) %>%
      mutate_at(var, ~replace_na(., 0))
  }

  # disease name lookup/clean
  disease_lookup <- lagged_newdata %>%
    distinct(disease) %>%
    mutate(disease_clean = janitor::make_clean_names(disease))

  lagged_newdata <- lagged_newdata %>%
    left_join(disease_lookup, by = "disease") %>%
    select(-disease) %>%
    rename(disease = disease_clean)

  return(lagged_newdata)
}

#' Augment nowcast bart model object
#'
#' @import repeldata dplyr tidyr
#' @importFrom assertthat has_name assert_that
#' @importFrom here here
#' @importFrom purrr map map_lgl
#' @importFrom stringr str_starts str_ends
#' @export
repel_augment.nowcast_boost <- function(model_object, conn, newdata) {

  # get lag cases
  lagged_newdata <- repel_lag(model_object, conn, newdata, lags = 1:3, control_measures = TRUE)

  # add continent
  lagged_newdata <- lagged_newdata %>%
    mutate(continent = suppressWarnings(countrycode::countrycode(country_iso3c, origin = "iso3c", destination = "continent")))

  # combine lagged 3 yrs control measures (overlaps are ok - this is for str extraction)
  lagged_newdata <- lagged_newdata %>%
    mutate(control_measures_lag = paste(control_measures_lag1, control_measures_lag2, control_measures_lag3, sep = "; "))

  control_list <- get_disease_controls()

  for(control in control_list){
    lagged_newdata <- lagged_newdata %>%
      mutate(!!paste0("control_", make_clean_names(control)) := as.integer(str_detect(control_measures_lag, control)))
  }

  lagged_newdata <- lagged_newdata %>%
    select(-starts_with("control_measures_lag"))

  # get summed lag values of adjacent countries
  borders <- tbl(conn, "connect_static_vars") %>%
    filter(shared_border == "TRUE") %>%
    select(country_origin, country_destination) %>%
    collect()

  lagged_borders <- lagged_newdata %>%
    select(all_of(grouping_vars)) %>%
    distinct() %>%
    rename(country_origin = country_iso3c) %>%
    left_join(borders,  by = "country_origin") %>%
    rename(country_iso3c = country_destination) %>%
    repel_lag(model_object, conn, newdata = ., lags = 1:3, control_measures = FALSE)

  lagged_borders_sum <- lagged_borders %>%
    select(-cases) %>%
    pivot_longer(cols = c(cases_lag1, cases_lag2, cases_lag3)) %>%
    group_by(country_origin, disease, disease_population, taxa, report_year, report_semester) %>%
    summarize(cases_lag_sum_border_countries = sum_na(as.integer(value))) %>%
    ungroup() %>%
    rename(country_iso3c = country_origin)

  lagged_newdata <- left_join(lagged_newdata, lagged_borders_sum, by = grouping_vars)

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
    right_join(expand(lagged_newdata, country_iso3c, report_year),  by = c("country_iso3c", "report_year")) %>%
    mutate(veterinarian_count_missing = is.na(veterinarian_count)) %>%
    arrange(country_iso3c, report_year) %>%
    group_split(country_iso3c) %>%
    map_dfr(~na_interp(., "veterinarian_count")) %>%
    select(-veterinarian_count) %>%
    rename(veterinarian_count = veterinarian_count_imputed)

  lagged_newdata <- left_join(lagged_newdata, vets, by = c("country_iso3c", "report_year"))

  # taxa population
  taxa <- tbl(conn, "country_taxa_population") %>%
    collect() %>%
    mutate(taxa = ifelse(taxa %in% c("goats", "sheep"), "sheep/goats", taxa)) %>%
    group_by(country_iso3c, year, taxa) %>%
    summarize(population = sum(population, na.rm = TRUE)) %>%  # adds up all goats and sheep
    ungroup() %>%
    rename(report_year = year, taxa_population = population) %>%
    right_join(expand(lagged_newdata, country_iso3c, report_year, taxa),  by = c("country_iso3c", "report_year", "taxa")) %>%
    mutate(taxa_population_missing = is.na(taxa_population)) %>%
    arrange(country_iso3c, taxa, report_year) %>%
    group_split(country_iso3c, taxa) %>%
    map_dfr(~na_interp(., "taxa_population")) %>%
    select(-taxa_population) %>%
    rename(taxa_population = taxa_population_imputed)

  lagged_newdata <- left_join(lagged_newdata, taxa, by = c("country_iso3c", "report_year", "taxa"))

  # World Bank indicators
  wbi <-  tbl(conn, "worldbank_indicators") %>%
    collect() %>%
    rename(report_year = year) %>%
    right_join(expand(lagged_newdata, country_iso3c, report_year),  by = c("country_iso3c", "report_year")) %>%
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

  lagged_newdata <- left_join(lagged_newdata, wbi,  by = c("country_iso3c", "report_year"))

  # remove rows from countries without GDP data?
  removing_gdp  <- lagged_newdata %>%
    filter(is.na(gdp_dollars))
  na_countries <- unique(removing_gdp$country_iso3c) %>%
    countrycode::countrycode(., origin = "iso3c", destination = "country.name", warn = FALSE)
  na_countries <- na_countries[!is.na(na_countries)]
  lagged_newdata <- lagged_newdata %>%
    drop_na(gdp_dollars)
  warning(paste("Dropping", nrow(removing_gdp), "rows of data with missing GDP values from following countries:", paste(na_countries, collapse = ", ")))

  # recode disease status - assuming unreported is absent - but mark as unreported
  ds_names <- names(lagged_newdata)[str_detect(names(lagged_newdata), "disease_status")]
  for(ds in ds_names){
    lagged_newdata <- lagged_newdata %>%
      mutate(!!paste0(ds, "_unreported") := as.integer(!!sym(ds)  == "unreported"))
  }
  lagged_newdata <- lagged_newdata %>%
    mutate_at(ds_names, ~as.integer(recode(., "present" = 1, "unreported" = 0, "absent" = 0)))

  # disease ever ... 4 scenarios
  scenarios <- c("country_given_taxa", "continent_given_taxa",
                 "country_any_taxa",  "continent_any_taxa")

  ever <- map(scenarios, function(iter){

    # columns for grouping
    colname <- switch(iter,
                      "country_any_taxa" = "country_iso3c",
                      "country_given_taxa" = "country_iso3c",
                      "continent_given_taxa" = "continent",
                      "continent_any_taxa" = "continent"
    )
    ever <- lagged_newdata

    # get continents
    if(stringr::str_starts(iter, "continent")){
      ever <- mutate(ever, continent = countrycode::countrycode(country_iso3c, origin = "iso3c", destination = "continent"))
      cont_lookup <- distinct(ever, country_iso3c, continent)
    }

    # this effectively excludes taxa from grouping without having to deal with removing the column
    if(stringr::str_ends(iter, "any_taxa")){
      ever <-  mutate(ever, taxa = "")
    }

    ever <- ever %>%
      rename(grp_var = all_of(colname)) %>% # rename grouping column to avoid tidyeval
      arrange(grp_var, disease, report_year, report_semester) %>%
      # get whether disease has occurred within given grouping
      group_by(grp_var, disease, taxa, report_year, report_semester) %>%
      summarize(disease_status = as.integer(1 %in% disease_status)) %>%
      group_by(grp_var, disease, taxa) %>%
      # teasing out first appearance and then inferring whether the disease has exisited before in given grouping
      mutate(cumulative_disease_status = cumsum(disease_status)) %>%
      mutate(first_appearance = cumulative_disease_status == 1 & (row_number() == 1 | lag(cumulative_disease_status) == 0)) %>%
      mutate(ever_in_grp = case_when(first_appearance ~ 1)) %>%
      fill(ever_in_grp, .direction = "down") %>%
      mutate(ever_in_grp =  case_when(first_appearance | is.na(ever_in_grp) ~ 0,
                                      TRUE ~ 1)) %>%
      ungroup() %>%
      select(-first_appearance, -cumulative_disease_status, -disease_status)

    # renaming and cleaning
    names(ever)[names(ever) == "ever_in_grp"] <- paste0("ever_in_", iter)
    names(ever)[names(ever) == "grp_var"] <- colname
    if(stringr::str_starts(iter, "continent")){
      ever <- left_join(ever, cont_lookup,  by = "continent") %>%
        select(-continent)
    }
    if(stringr::str_ends(iter, "any_taxa")){
      ever <- select(ever, -taxa)
    }

    return(ever)
  })

  names(ever) <- scenarios

  for(sc in scenarios){
    if(stringr::str_ends(sc, "any_taxa")){
      lagged_newdata <- left_join(lagged_newdata, ever[[sc]],
                                  by = c("country_iso3c", "report_year", "report_semester", "disease"))
    }else{
      lagged_newdata <- left_join(lagged_newdata, ever[[sc]],
                                  by = c("country_iso3c", "report_year", "report_semester", "disease", "taxa"))
    }
  }

  # mark missingness
  lag_vars <- colnames(lagged_newdata)[str_detect(names(lagged_newdata), "disease_status_lag|cases_lag")]
  lag_vars <- lag_vars[!endsWith(lag_vars, "_unreported")]
  for(var in c(lag_vars)){
    lagged_newdata <- lagged_newdata %>%
      mutate(!!paste0(var, "_missing") := is.na(get(var))) %>%
      mutate_at(var, ~replace_na(., 0))
  }

  # add column to indicate first year country reporting
  lagged_newdata <- lagged_newdata %>%
    mutate(report_period = as.integer(paste0(report_year, report_semester))) %>%
    group_by(country_iso3c) %>%
    mutate(first_reporting_semester = report_period == min(report_period)) %>%
    ungroup() %>%
    select(-report_period)

  # final feature engineering - transformations etc
  lagged_newdata <- lagged_newdata %>%
    mutate(log_gdp_per_capita = prepvar(gdp_dollars/human_population, trans_fn = log10)) %>%
    select(-gdp_dollars) %>%
    mutate(log_veterinarians_per_taxa = prepvar((veterinarian_count+1)/(taxa_population+1), trans_fn = log10)) %>%
    select(-veterinarian_count) %>%
    mutate(log_taxa_population = prepvar(taxa_population+1, trans_fn = log10)) %>%
    select(-taxa_population) %>%
    mutate(log_human_population = prepvar(human_population, trans_fn = log10)) %>%
    select(-human_population) %>%
    # handling data types
    mutate_if(is.character, as.factor) %>%
    mutate_if(is.logical, as.double) %>%
    #reorder
    select(report_year, report_semester, disease, taxa, country_iso3c, continent, disease_population,
           starts_with("cases"),  starts_with("disease_status"),
           starts_with("ever_in"),
           log_human_population, human_population_missing,
           log_taxa_population, taxa_population_missing,
           log_veterinarians_per_taxa, veterinarian_count_missing,
           log_gdp_per_capita, gdp_dollars_missing,
           first_reporting_semester,
           starts_with("control"),
           everything()) # make sure we don't accidentally drop any columns

  assertthat::assert_that(!any(map_lgl(lagged_newdata, ~any(is.infinite(.)))))
  return(lagged_newdata)
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


#' Augment nowcast baseline model object
#' @import repeldata dplyr tidyr dtplyr data.table
#' @importFrom assertthat has_name assert_that
#' @importFrom janitor make_clean_names
#' @importFrom lubridate ymd year
#' @importFrom purrr map_dfr
#' @export
repel_augment.network_model <- function(model_object, conn, newdata, sum_country_imports = TRUE) {

  # check newdata has correct input vars
  assertthat::has_name(newdata, c("country_iso3c", "disease", "month"))
  newdata <- newdata %>%
    select(country_iso3c, disease, month)

  # start lookup table for augmenting
  outbreak_status <- repel_split(model_object, conn)

   # add year column to support joins
  outbreak_status <- outbreak_status %>%
    mutate(year = year(month))

  # World Bank indicators
  wbi <-  tbl(conn, "worldbank_indicators") %>%
    collect() %>%
    right_join(tidyr::expand(outbreak_status, country_iso3c, year),  by = c("country_iso3c", "year")) %>%
    arrange(country_iso3c, year) %>%
    group_split(country_iso3c) %>%
    map_dfr(~na_interp(., "gdp_dollars") %>%
              na_interp(., "human_population")) %>%
    select(-gdp_dollars,
           -human_population) %>%
    rename(gdp_dollars = gdp_dollars_imputed,
           human_population = human_population_imputed)

  outbreak_status <- left_join(outbreak_status, wbi,  by = c("country_iso3c", "year"))

  # taxa population
  disease_taxa_lookup <- vroom::vroom(system.file("lookup", "disease_taxa_lookup.csv", package = "repelpredict")) %>%
    select(-disease_pre_clean)

  taxa_population <- tbl(conn, "country_taxa_population") %>%
    collect() %>%
    mutate(taxa = ifelse(taxa %in% c("goats", "sheep"), "sheep/goats", taxa)) %>%
    group_by(country_iso3c, year, taxa) %>%
    summarize(population = sum(population, na.rm = TRUE)) %>%  # adds up all goats and sheep - should be done before imputation
    ungroup() %>%
    rename(taxa_population = population) %>%
    right_join(tidyr::crossing(country_iso3c = unique(.$country_iso3c),
                               year = seq(min(.$year), max(outbreak_status$year)),
                               taxa = unique(.$taxa))) %>%
    arrange(country_iso3c, taxa, year) %>%
    group_split(country_iso3c, taxa) %>%
    map_dfr(~na_interp(., "taxa_population")) %>%
    select(-taxa_population) %>%
    rename(taxa_population = taxa_population_imputed) %>%
    left_join(disease_taxa_lookup) %>%
    group_by(country_iso3c, year, disease) %>%
    summarize(target_taxa_population = sum(taxa_population, na.rm = TRUE)) %>%
    ungroup()

  outbreak_status <- left_join(outbreak_status, taxa_population,  by = c("country_iso3c", "year", "disease"))
  # ^ may not be most reliable source

  # vet capacity
  vets <- tbl(conn, "annual_reports_veterinarians") %>%
    collect() %>%
    rename(year = report_year) %>%
    filter(veterinarian_field %in% c(
      "animal health and welfare activities",
      "veterinary public health activities",
      "laboratories",
      "private clinical practice",
      "academic activities and education",
      "pharmaceutical industry"
    )) %>%
    group_by(country_iso3c, year) %>%
    summarize(veterinarian_count = sum_na(suppressWarnings(as.integer(total_count)))) %>% # summarize over different types of vets
    ungroup() %>%
    mutate(year = as.integer(year)) %>%
    right_join(tidyr::expand(outbreak_status, country_iso3c, year),  by = c("country_iso3c", "year")) %>%
    arrange(country_iso3c, year) %>%
    group_split(country_iso3c) %>%
    map_dfr(~na_interp(., "veterinarian_count")) %>%
    select(-veterinarian_count) %>%
    rename(veterinarian_count = veterinarian_count_imputed)

  outbreak_status <- left_join(outbreak_status, vets,  by = c("country_iso3c", "year"))

  # which countries have disease outbreak in a given month
  disease_status_present <- outbreak_status %>%
    filter(outbreak_start | outbreak_subsequent_month | endemic) %>%
    select(country_origin = country_iso3c, month, disease)

  outbreak_status <- outbreak_status %>%
    mutate(outbreak_start = as.integer(outbreak_start)) %>%
    select(-validation_set)


  # set up country origin/destination combinations - origin is countries that have the disease present in the given month
  outbreak_status <- outbreak_status %>%
    left_join(disease_status_present, by = c("month", "disease")) %>%
    rename(country_destination = country_iso3c) %>%
    mutate(country_origin = if_else(country_origin == country_destination, NA_character_, country_origin))

  # bring in static vars
  connect_static <- tbl(conn, "connect_static_vars")  %>%
    collect() %>%
    select(-n_migratory_birds, -gc_dist) %>%
    mutate(shared_border = as.logical(shared_border)) %>%
    mutate(n_migratory_wildlife = as.integer(n_migratory_wildlife))

  outbreak_status <- left_join(outbreak_status, connect_static,  by = c("country_destination", "country_origin"))

  #assertthat::assert_that(!any(is.na(outbreak_status$shared_border)))

  # bring in yearly vars
  connect_yearly <- DBI::dbReadTable(conn, "connect_yearly_vars") %>%
    collect() %>%
    mutate(year = as.integer(year))

  # human movement vars
  #  human_movement <- connect_yearly %>%
  #   select(country_origin, # country with disease
  #          country_destination, # country of interest
  #          year, n_human_migrants) %>%
  #   mutate_at(vars(starts_with("n_")), as.numeric)  %>%
  #   arrange(country_origin, country_destination, year) %>%
  #   group_split(country_origin, country_destination) %>%
  #   map_dfr(~na_interp(., "n_human_migrants")) %>%
  #   select(-n_human_migrants) %>%
  #   rename(n_human_migrants = n_human_migrants_imputed)
  #
  # outbreak_status <- left_join(outbreak_status, human_movement, by = c("country_destination", "year", "country_origin"))

  # trade vars
  trade_vars <- connect_yearly %>%
    select(-starts_with("n_")) %>%
    pivot_longer(cols = -c("country_origin" , "country_destination",  "year" ))

  current_year <- lubridate::year(Sys.Date())
  years_to_add = seq(from = max(trade_vars$year)+1, to = current_year)
  trade_vars_latest <- trade_vars %>%
    filter(year == max(year))

  for(yr in years_to_add){
    trade_vars_latest_add <- trade_vars_latest %>%
      mutate(year = yr)
    trade_vars <- bind_rows(trade_vars, trade_vars_latest_add)
  }

  ots_lookup <- DBI::dbReadTable(conn, "connect_ots_lookup") %>%
    collect() %>%
    mutate(source = "ots_trade_dollars") %>%
    select(product_code, group_name = product_fullname_english, source)

  connect_fao_lookup <- DBI::dbReadTable(conn, "connect_fao_lookup") %>%
    collect() %>%
    mutate(source = "fao_livestock_heads") %>%
    mutate(item_code = as.character(item_code)) %>%
    rename(group_name = item, product_code = item_code)

  trade_lookup <- bind_rows(ots_lookup, connect_fao_lookup)

  trade_vars_lookup <- trade_vars %>%
    distinct(name) %>%
    mutate(product_code =  str_remove(name, "trade_dollars_|livestock_heads_")) %>%
    mutate(source = case_when(
      str_detect(name, "trade_dollars_") ~ "ots_trade_dollars",
      str_detect(name, "livestock_heads_") ~ "fao_livestock_heads",
      name == "n_human_migrants" ~ "un_human_migration",
      name == "n_tourists" ~ "un_wto_tourism"
    )) %>%
    left_join(trade_lookup,  by = c("product_code", "source")) %>%
    mutate(group_name = str_extract(group_name, "[^;]+"))

  # sum trade vars over groups
  trade_vars_groups_summed <- trade_vars %>%
    left_join(trade_vars_lookup, by = "name") %>%
    lazy_dt() %>%
    mutate(value = as.numeric(value)) %>%
    group_by(country_origin, country_destination, year, source, group_name) %>% # sum over groups
    summarize(value = sum(value, na.rm = TRUE)) %>% # will turn all NAs into 0
    ungroup() %>%
    as_tibble()

  # get total trade heads/dollars by source (fao, ots)
  trade_vars_groups_total <- trade_vars_groups_summed %>%
    group_by(country_origin, country_destination, year, source) %>%
    summarize(value = sum(value)) %>%
    ungroup() %>%
    as_tibble() %>%
    pivot_wider(names_from = source, values_from = value)

  outbreak_status <- left_join(outbreak_status, trade_vars_groups_total, by = c("country_destination", "year", "country_origin"))

  # do a pca on ots trade groups
  # pca_dat <- trade_vars_groups_summed %>%
  #   filter(source == "ots_trade_dollars" ) %>%
  #   select(-source) %>%
  #   pivot_wider(names_from = group_name, values_from = value) %>%
  #   janitor::clean_names()
  #
  # pca <- prcomp(pca_dat %>% select(-country_origin, -country_destination, -year), )
  # # summary(pca)
  #
  # pca_vals <- pca$rotation %>%
  #   as_tibble() %>%
  #   mutate(var = rownames(pca$rotation))
  #
  # # PC1 interp: high soy and corn
  # pc1 <- pca_vals %>%
  #   select(var, PC1) %>%
  #   arrange(-abs(PC1))
  #
  # # PC2 interp: high trunks, fish, leather
  # pc2 <- pca_vals %>%
  #   select(var, PC2) %>%
  #   arrange(-abs(PC2))
  #
  # pcout <- pca$x %>%
  #   as_tibble() %>%
  #   select(PC1, PC2) %>%
  #   bind_cols(pca_dat %>% select(country_origin, country_destination, year), .) %>%
  #   rename(ots_trade_pc1_soy_corn = PC1, ots_trade_pc2_trunks_fish_leather = PC2)
  #
  # outbreak_status <- left_join(outbreak_status, pcout, by = c("country_destination", "year", "country_origin"))

  # add in all the grouped summed trade values
  # trade_vars_groups_summed <- trade_vars_groups_summed %>%
  #   mutate(group_name = paste0("trade_", group_name)) %>%
  #   select(-source) %>%
  #   as_tibble() %>%
  #   pivot_wider(names_from = group_name, values_from = value) %>%
  #   janitor::clean_names()
  #
  # outbreak_status <- left_join(outbreak_status, trade_vars_groups_summed,  by = c("country_destination", "year", "country_origin"))
  # vroom::vroom_write(outbreak_status, here::here("tmp/network_augment_expanded.csv"))
  # outbreak_status <- vroom(here::here("tmp/network_augment_expanded.csv"))

  if(sum_country_imports){
    # sum all incoming values into destination country
    outbreak_status <- outbreak_status %>%
      lazy_dt() %>%
      select(-country_origin, -year) %>%
      group_by(country_destination, disease, month, outbreak_start,
               gdp_dollars, human_population, target_taxa_population, veterinarian_count ## non-bilaterial values
      ) %>%
      summarize_all(~sum(as.numeric(.), na.rm = TRUE)) %>%  #TODO DEAL WITH NAS in human movement
      ungroup() %>%
      as_tibble()
  }

  # finishing touches
  outbreak_status <- outbreak_status %>%
    select(-suppressWarnings(one_of("year"))) %>%
    mutate(outbreak_start = outbreak_start > 0) %>%
    mutate(endemic = endemic > 0) %>%
    mutate(disease_country_combo_unreported = disease_country_combo_unreported > 0) %>%
    mutate(outbreak_subsequent_month = outbreak_subsequent_month > 0) %>%
    mutate(outbreak_ongoing = outbreak_ongoing > 0) %>%
    mutate(shared_border = as.integer(shared_border)) %>%
    mutate(log_human_population = prepvar(human_population, trans_fn = log10)) %>%
    select(-human_population) %>%
    mutate(log_gdp_dollars = prepvar(gdp_dollars, trans_fn = log10)) %>%
    select(-gdp_dollars) %>%
    mutate(log_target_taxa_population = prepvar((target_taxa_population+1), trans_fn = log10)) %>%
    select(-target_taxa_population) %>%
    mutate(log_veterinarians = prepvar((veterinarian_count+1), trans_fn = log10)) %>%
    select(-veterinarian_count) %>%
    mutate(continent = as.factor(countrycode::countrycode(country_destination,  origin = "iso3c", destination = "continent"))) %>%
    select(country_iso3c = country_destination, continent, suppressWarnings(one_of("country_origin")),
           disease, month, log_gdp_dollars, log_human_population, log_target_taxa_population,
           log_veterinarians,
           outbreak_start,
           outbreak_subsequent_month, outbreak_ongoing, endemic, disease_country_combo_unreported,
           n_migratory_wildlife_from_outbreaks = n_migratory_wildlife,
           shared_borders_from_outbreaks = shared_border,
           ots_trade_dollars_from_outbreaks = ots_trade_dollars,
           fao_livestock_heads_from_outbreaks = fao_livestock_heads,
           everything())

  names(outbreak_status)[str_starts(names(outbreak_status), "trade_")] <- paste0("fao_",
                                                                                 names(outbreak_status)[str_starts(names(outbreak_status), "trade_")],
                                                                                 "_from_outbreaks")

  augmented_newdata <- left_join(newdata, outbreak_status, by = c("country_iso3c", "disease", "month"))

  return(augmented_newdata)
}

