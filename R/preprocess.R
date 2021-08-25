#' define generic function
#' @export
repel_init <- function(x, ...){
  UseMethod("repel_init")
}

#' Preprocess nowcast data
#' @param model_object nowcast model object
#' @param conn connection to repel db
#' @import repeldata dplyr tidyr stringr
#' @importFrom purrr map_chr
#' @importFrom janitor get_dupes
#' @importFrom assertthat are_equal
#' @export
repel_init.nowcast_model <- function(model_object, conn){

  six_month_reports_summary <- tbl(conn, "six_month_reports_summary") %>%
    mutate(country_iso3c = toupper(country_iso3c)) %>%
    mutate(taxa = str_remove(taxa, " \\(mixed herd\\)")) %>%
    mutate(taxa = str_remove(taxa, " \\(mixed group\\)")) %>%
    mutate(taxa = ifelse(taxa %in% c("goats", "sheep"), "sheep/goats", taxa)) %>%
    mutate(taxa = ifelse(taxa %in% c("rabbits", "hares"), "hares/rabbits", taxa)) %>%
    filter(taxa %in% taxa_list) %>%
    mutate(control_measures = na_if(control_measures, "na")) %>%
    select(all_of(grouping_vars), serotype, control_measures, disease_status, cases) %>%
    collect() %>%
    drop_na(country_iso3c) %>%  # few small non-independent countries
    group_by_at(grouping_vars) %>% # summarize over serotype and multiple taxa in same grp
    summarize(
      cases = as.integer(sum_na(suppressWarnings(as.integer(cases)))),
      disease_status = str_flatten(sort(na.omit(unique(disease_status))), collapse = ","),
      control_measures = str_flatten(control_measures, collapse = "; ")
    ) %>%
    ungroup() %>%
    mutate(control_measures = str_split(control_measures, "; ")) %>%
    mutate(control_measures = map(control_measures, ~str_flatten(sort(na.omit(unique(.))), collapse = "; "))) %>%
    mutate(control_measures = ifelse(control_measures == "", "none", control_measures)) %>%
    mutate(disease_status = recode(disease_status,
                                   "absent,present"  = "present",
                                   "present,unreported"  = "present",
                                   "absent,unreported" = "absent"
    ))

  dup_test <- six_month_reports_summary %>%
    janitor::get_dupes(all_of(grouping_vars))
  assertthat::are_equal(0, nrow(dup_test))

  return(six_month_reports_summary)
}


#' Preprocess network data
#' @param model_object network model object
#' @param conn connection to repel db
#' @param outbreak_reports_events optional to provide outbreak_reports_events dataframe. This is used when providing new data. Default is to read full outbreak_reports_events from conn for model fitting.
#' @param remove_single_country_disease whether to remove diseases that occur in only one country. Default is TRUE for model fitting. Use FALSE for new data.
#' @import repeldata dplyr tidyr stringr
#' @importFrom lubridate floor_date ceiling_date
#' @export
repel_init.network_model <- function(model_object,
                                     conn,
                                     outbreak_reports_events = NULL,
                                     remove_single_country_disease = TRUE){

  current_month <- floor_date(Sys.Date(), unit = "month")
  current_year <- year(current_month)
  current_semester <- ifelse(current_month < "2021-07-01", 1, 2)
  current_period <- as.numeric(paste(current_year, recode(current_semester, '1' = '0', '2' = '5'), sep = "."))

  prev_year <- floor_date(current_month - 365,  unit = "month")
  next_century <- floor_date(current_month + 36500, unit = "month")

  if(is.null(outbreak_reports_events)){
    dat <- tbl(conn, "outbreak_reports_events") %>%
      collect()
  }else{
    dat <- outbreak_reports_events
  }

  events <- dat %>%
    filter(!is.na(country_iso3c), country_iso3c != "NA") %>%
    mutate_at(vars(contains("date")), as.Date)

  # Remove disease that have have reports in only one country_iso3c
  if(remove_single_country_disease){
    diseases_keep <- events %>%
      group_by(disease) %>%
      summarize(n_countries = length(unique(country_iso3c))) %>%
      arrange(desc(n_countries)) %>%
      filter(n_countries > 1 ) %>%
      pull(disease)

    events <- events %>%
      filter(disease %in% diseases_keep)
  }

  # dates handling
  events <- events %>%
    arrange(country, disease, report_date) %>%
    mutate(report_month = floor_date(report_date, unit = "months")) %>%
    mutate(date_of_start_of_the_event = floor_date(date_of_start_of_the_event, "month")) %>%
    mutate(date_event_resolved = floor_date(date_event_resolved, "month")) %>%
    select(country_iso3c, disease, outbreak_thread_id, report_type, report_month, date_of_start_of_the_event, date_event_resolved)  %>%
    group_by(outbreak_thread_id) %>%
    mutate(outbreak_start_month = min(c(report_month, date_of_start_of_the_event))) %>%
    mutate(outbreak_end_month = max(coalesce(date_event_resolved, report_month))) %>%  # outbreak end is date event resolved, if avail, or report month. the max accounts for instances where the resolved date is farther in the past than more recent reports in the same thread. see outbreak_thread_id == 10954
    # ^ this assumes that if thread is not marked as resolved, use the last report date as the end month
    # if it's been less than a year, however, keep the event as ongoing (use an end date in the future)
    mutate(outbreak_end_month = if_else(
      outbreak_end_month >= prev_year & all(is.na(date_event_resolved)),
      as.Date(next_century),
      as.Date(outbreak_end_month)
    )) %>%
    ungroup()

  # add in all combos of disease-country-month
  events <- events %>%
    full_join(
      events %>%
        tidyr::expand(
          country_iso3c,
          disease,
          month = seq.Date(
            from =  ymd("2005-01-01"), # start of record keeping
            to = current_month,
            by = "months")), by = c("country_iso3c", "disease"))

  # identify subsequent continuous outbreaks
  events <- events %>%
    mutate(outbreak_subsequent_month = month > outbreak_start_month & month <= outbreak_end_month) %>% # within bounds
    mutate(outbreak_start = month == outbreak_start_month)  %>%
    mutate(disease_country_combo_unreported = is.na(outbreak_thread_id)) %>%
    mutate_at(.vars = c("outbreak_subsequent_month", "outbreak_start"), ~replace_na(., FALSE))

  # identify endemic events
  endemic_status_present <- tbl(conn, "nowcast_boost_augment_predict")  %>% # this should have even coverage by country/disease up to latest reporting period
    inner_join(distinct(events, country_iso3c, disease), copy = TRUE,  by = c("disease", "country_iso3c")) %>%
    mutate(cases = coalesce(cases, predicted_cases)) %>%
    filter(cases > 0) %>%
    select(country_iso3c, report_year, report_semester, disease) %>%
    collect() %>%
    mutate(report_year = as.integer(report_year)) %>%
    mutate(report_semester = as.integer(report_semester))

  #assume last conditions are present conditions
  endemic_status_present_latest <- endemic_status_present %>%
    mutate(report_period = report_year + (report_semester - 1)/2) %>%
    filter(report_period == max(report_period)) %>%
    tidyr::expand(.,
                  country_iso3c,
                  disease,
                  report_period = as.character(format(seq(from = max(.$report_period), to = current_period, by = 0.5), nsmall = 1))) %>%
    mutate(report_year = as.integer(str_sub(report_period, start = 1, end = 4))) %>%
    mutate(report_semester = as.integer(str_sub(report_period, -1)) * 2 + 1 ) %>%
    filter(report_period != min(report_period)) %>%
    select(-report_period)

  endemic_status_present <- bind_rows(endemic_status_present, endemic_status_present_latest)

  year_lookup <- endemic_status_present %>%
    distinct(report_semester, report_year) %>%
    mutate(month = case_when(
      report_semester == 1 ~ list(seq(1, 6)),
      report_semester == 2 ~ list(seq(7, 12))))
  year_lookup <- unnest(year_lookup, month) %>%
    mutate(month = ymd(paste(report_year, month, "01")))

  endemic_status_present <- endemic_status_present %>%
    left_join(year_lookup,  by = c("report_year", "report_semester")) %>%
    select(country_iso3c, month, disease) %>%
    mutate(endemic = TRUE) %>%
    distinct()

  events <- events %>%
    left_join(endemic_status_present, by = c("country_iso3c", "disease", "month")) %>%
    mutate(endemic = replace_na(endemic, FALSE))

  # summarize to id subsequent and endemic
  events <- events %>%
    group_by(country_iso3c, disease, month) %>%
    summarize(outbreak_start = any(outbreak_start),
              outbreak_subsequent_month = any(outbreak_subsequent_month),
              endemic = any(endemic),
              disease_country_combo_unreported = all(disease_country_combo_unreported)) %>%
    ungroup() %>%
    mutate(outbreak_ongoing = outbreak_start|outbreak_subsequent_month)

  #mutate(endemic = ifelse(outbreak_start, FALSE, endemic))
  # ^ this last mutate covers cases where the outbreak makes it into the semester report. the first month of the outbreak should still count.
  # on the other hand, there are outbreaks that are reported when it really is already endemic, eg rabies, so commenting out for now
  # events %>% filter(outbreak_start, endemic) %>% View

  # remove diseases that do not affect primary taxa
  disease_taxa_lookup <- vroom::vroom(system.file("lookup", "disease_taxa_lookup.csv", package = "repelpredict"), show_col_types = FALSE)
  events <- events %>%
    filter(disease %in% unique(disease_taxa_lookup$disease_pre_clean))

  # clean disease names
  events <- repel_clean_disease_names(model_object, df = events)
  assertthat::assert_that(!any(is.na(unique(events$disease)))) # if this fails, rerun inst/nowcast_generate_disease_lookup.R

  return(events)
}

#' Preprocess impact data
#' @import repeldata dplyr tidyr stringr sf spatstat.geom
#' @export
repel_init.impact_model <- function(model_object, conn){

  events <-  tbl(conn, "outbreak_reports_events") %>%
    collect() %>%
    filter(!is.na(country_iso3c), country_iso3c != "NA") %>%
    mutate_at(vars(contains("date")), as.Date)

  events_lookup <- events %>%
    select(report_id, url_report_id, outbreak_thread_id, report_type, event_status, is_final_report, country, country_iso3c, disease, report_date)

  outbreaks <-  tbl(conn, "outbreak_reports_outbreaks") %>%
    collect() %>%
    rename(taxa = species_name) %>%
    inner_join(events_lookup, .,  by = "report_id") %>%
    select(ends_with("_id"), ends_with("_date"), everything())

  outbreaks_summary <- outbreaks %>%
    group_by(outbreak_thread_id, country, country_iso3c, disease, taxa) %>%
    summarize(total_cases = sum(cases, na.rm = TRUE),
              total_deaths = sum(deaths, na.rm = TRUE),
              total_killed_and_disposed = sum(killed_and_disposed, na.rm = TRUE),
              total_slaughtered_for_commercial_use = sum(slaughtered_for_commercial_use, na.rm = TRUE),
              outbreak_start_date = min(outbreak_start_date, na.rm = TRUE),
              outbreak_end_date = max(outbreak_start_date, na.rm = TRUE),
              outbreak_radius = get_bounding_radius(latitude = latitude, longitude = longitude)
    ) %>%
    ungroup()

  return(outbreaks_summary)
}

#### functions to get outbreak distance for impact model

#' Gets UTM from lat/long
#' adapted from https://bookdown.org/robinlovelace/geocompr/reproj-geo-data.html
#' @noRd
lonlat2UTM <- function(longitude, latitude) {

  utm <- (floor((longitude + 180) / 6) %% 60) + 1

  if(latitude > 0) {
    utm + 32600
  }else{
    utm + 32700
  }
}

#' Gets bounding radius of lat/long points
#' @import  dplyr sf spatstat.geom
#' @noRd
get_bounding_radius <- function(latitude, longitude){

  latlong <- tibble(latitude, longitude) %>%
    distinct()

  # use mean lat/long to get utm
  mean_long <- mean(latlong$longitude, na.rm = TRUE)
  mean_lat <- mean(latlong$latitude, na.rm = TRUE)
  epsg_utm <- lonlat2UTM(longitude = mean_long, latitude = mean_lat)

  # convert from latlon to utm
  out <- latlong %>%
    st_as_sf(coords = c("longitude", "latitude"),  crs = "+proj=latlon") %>%
    st_transform(epsg_utm) %>%
    st_coordinates() %>%
    as_tibble() %>%
    distinct()

  # generate bounding circle
  if(nrow(out)==1){
    return(0)
  }
  if(nrow(out)==2){
    a2 <- diff(out$X)^2
    b2 <- diff(out$Y)^2
    dist  <- sqrt(a2 + b2)
    radius <- dist/2
    return(radius)
  }
  if(nrow(out)>2){
    points <- ppp(out$X, out$Y, xrange = range(out$X), yrange = range(out$Y))
    radius <- boundingradius(points)
    return(radius)
  }
}
