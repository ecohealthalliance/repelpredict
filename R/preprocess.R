#' define generic augment
#' @export
repel_init <- function(x, ...){
  UseMethod("repel_init")
}

#' Preprocess nowcast data
#' @import repeldata dplyr tidyr stringr
#' @importFrom purrr map_chr
#' @export
repel_init.nowcast_model <- function(model_object, conn){

  annual_reports_animal_hosts <- tbl(conn, "annual_reports_animal_hosts") %>%
    mutate(taxa = ifelse(taxa %in% c("goats", "sheep"), "sheep/goats", taxa)) %>%
    filter(taxa %in% taxa_list) %>%
    filter(report_semester != "0") %>%
    select(all_of(grouping_vars), control_measures, disease_status, cases) %>%
    collect() %>%
    group_by_at(grouping_vars) %>%
    summarize(
      cases = as.integer(sum_na(suppressWarnings(as.integer(cases)))),
      disease_status = str_flatten(sort(na.omit(unique(disease_status))), collapse = ","),
      control_measures = paste(na.omit(control_measures), collapse = "; ")
    ) %>%
    ungroup() %>%
    mutate(control_measures = map_chr(str_split(control_measures, pattern = "; "), ~paste(sort(unique(.)), collapse = "; "))) %>%
    mutate(control_measures = ifelse(control_measures == "", "none", control_measures)) %>%
    mutate(disease_status = recode(disease_status,
                                   "absent,present"  = "present",
                                   "absent,suspected" = "suspected",
                                   "present,suspected" = "present"


    ))
  return(annual_reports_animal_hosts)
}


#' Preprocess nowcast data
#' @import repeldata dplyr tidyr stringr
#' @importFrom tidyr expand
#' @importFrom lubridate floor_date ceiling_date
#' @export
repel_init.network_model <- function(model_object, conn){

  immediate_events <- tbl(conn, "outbreak_reports_events") %>%
    filter(!is.na(country_iso3c)) %>% # TODO: fix these events to be assigned to a current country
    collect() %>%
    mutate_at(vars(contains("date")), as.Date) %>%
    filter(str_detect(report_type, "immediate notification"))

  # Remove disease that have have reports in only one country_iso3c
  diseases_keep <- immediate_events %>%
    group_by(disease) %>%
    summarize(n_countries = length(unique(country_iso3c))) %>%
    arrange(desc(n_countries)) %>%
    filter(n_countries > 1 ) %>%
    pull(disease)

  immediate_events_mult <- immediate_events %>%
    filter(disease %in% diseases_keep)

  # get number of events started and ended in a given month
  event_starts <- immediate_events_mult %>%
    mutate(start_month = floor_date(date_of_start_of_the_event, unit = "months")) %>%
    group_by(country_iso3c, disease, start_month) %>%
    summarize(outbreak_start = n())

  event_ends <- immediate_events_mult %>%
    mutate(end_month = ceiling_date(date_of_start_of_the_event, unit = "months")) %>%
    group_by(country_iso3c, disease, end_month) %>%
    summarize(outbreak_end = -n())

  # determine if outbreak is ongoing in a given month
  immediate_events_status <- immediate_events_mult %>%
    tidyr::expand(
      country_iso3c,
      disease,
      month = seq.Date(
        from = floor_date(min(immediate_events_mult$date_of_start_of_the_event, na.rm = TRUE), unit = "months"),
        to = Sys.Date(),
        by = "months")
    ) %>%
    left_join(mutate(event_starts, country_iso3c, disease, month = start_month, outbreak_start, .keep = "none"),  by = c("country_iso3c", "disease", "month")) %>%
    left_join(mutate(event_ends, country_iso3c, disease, month = end_month, outbreak_end, .keep = "none"),  by = c("country_iso3c", "disease", "month")) %>%
    mutate_at(c("outbreak_start", "outbreak_end"), ~coalesce(., 0)) %>%
    group_by(country_iso3c, disease) %>%
    arrange(month) %>%
    mutate(outbreak_ongoing = as.numeric(cumsum(outbreak_start + outbreak_end) > 0)) %>%
    ungroup() %>%
    select(-outbreak_end)

  return(immediate_events_status)

  #   # reshape wide
  #   tl_wide <- tl %>%
  #   select(country_iso3c, disease, month, outbreak_ongoing) %>%
  #   pivot_wider(names_from = country_iso3c, values_from = outbreak_ongoing)
  #
  # tl_all <- tl %>%
  #   select(country_iso3c, disease, month, outbreak_start) %>%
  #   left_join(tl_wide, by = c("disease", "month")) %>%
  #   group_split(country_iso3c) %>%
  #   map_dfr(function(z) {
  #     z[[z$country_iso3c[1]]] <- 0
  #     z
  #   }) %>%
  #   mutate(outbreak_start = outbreak_start > 0)
  #
  # tl_long <- tl_all %>%
  #   pivot_longer(cols = AFG:ZWE)


  # # indicate whether outbreak is ongoing in continent or world in previous month
  # # sum of other countries
  #
  # continent_lookup <- tl %>%
  #   distinct(country_iso3c) %>%
  #   mutate(continent = countrycode::countrycode(country_iso3c, origin = "iso3c", destination = "continent"))
  #
  #
  # tl_ongoing_anywhere <- tl_long %>%
  #   group_by(country_iso3c, disease, month) %>%
  #   summarize(n_outbreaks_anywhere_lag = sum(value)) %>%
  #   ungroup() %>%
  #   mutate(month = floor_date(month+31, unit = "month"))
  #
  # tl_ongoing_continent <- tl_long %>%
  #   left_join(continent_lookup, by = "country_iso3c") %>%
  #   left_join(continent_lookup, by = c("name" = "country_iso3c")) %>%
  #   filter(continent.x == continent.y) %>%
  #   group_by(country_iso3c, disease, month) %>%
  #   summarize(n_outbreaks_continent_lag = sum(value)) %>%
  #   ungroup() %>%
  #   mutate(month = floor_date(month+31, unit = "month"))
  #
  # t2 <- tl %>%
  #   mutate(outbreak_start = outbreak_start > 0) %>%
  #   select(-outbreak_end, -outbreak_ongoing) %>%
  #   left_join(tl_ongoing_anywhere) %>%
  #   left_join(tl_ongoing_continent)
  #
  # # add in trade vars
  # year_lookup <- map_dfr(unique(trade_vars$year), function(yr){
  #   tibble(year = yr, month = seq(ymd(paste0(yr, "-01-01")), ymd(paste0(yr, "-12-01")), by = 'months'))
  # })
  #
  # trade_vars2 <- trade_vars %>%
  #   group_by(country_destination, year, group_name) %>%
  #   summarize(value = sum(value)) %>%
  #   ungroup() %>%
  #   mutate(group_name = str_extract(group_name, "[^;]+")) %>%
  #   mutate(group_name = paste0("trade_", group_name)) %>%
  #   pivot_wider(names_from = group_name, values_from = value) %>%
  #   janitor::clean_names() %>%
  #   rename(country_iso3c = country_destination)  %>%
  #   left_join(year_lookup) %>%
  #   select(-year)
}
