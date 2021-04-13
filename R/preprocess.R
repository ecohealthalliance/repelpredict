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

  # read in immediate outbreaks
  events <- tbl(conn, "outbreak_reports_events") %>%
    collect() %>%
    filter(!is.na(country_iso3c), country_iso3c != "NA") %>% # TODO: fix these events to be assigned to a current country
    mutate_at(vars(contains("date")), as.Date) #%>%
  #filter(str_detect(report_type, "immediate notification"))

  # Remove disease that have have reports in only one country_iso3c
  diseases_keep <- events %>%
    group_by(disease) %>%
    summarize(n_countries = length(unique(country_iso3c))) %>%
    arrange(desc(n_countries)) %>%
    filter(n_countries > 1 ) %>%
    pull(disease)

  events <- events %>%
    filter(disease %in% diseases_keep)

  # dates handling
  events <- events %>%
    arrange(country, disease, report_date) %>%
    mutate(report_month = floor_date(report_date, unit = "months")) %>%
    mutate(date_of_start_of_the_event = floor_date(date_of_start_of_the_event, "month")) %>%
    mutate(date_event_resolved = ceiling_date(date_event_resolved, "month")) %>%
    select(country_iso3c, disease, immediate_report, report_type, report_month, date_of_start_of_the_event, date_event_resolved)  %>%
    group_by(immediate_report) %>%
    mutate(outbreak_start_month = min(c(report_month, date_of_start_of_the_event))) %>%
    mutate(outbreak_end_month = coalesce(date_event_resolved, report_month)) %>%  # this isnt exactly right because some events are not marked as resolved. here we assume last report is when it's resolved.
    ungroup()

  # add in all combos of disease-country-month
  events <- events %>%
    full_join(
      events %>%
        tidyr::expand(
          country_iso3c,
          disease,
          month = seq.Date(
            from = floor_date(min(events$date_of_start_of_the_event, na.rm = TRUE), unit = "months"),
            to = Sys.Date(),
            by = "months")), by = c("country_iso3c", "disease"))

# identify subsequent continuous outbreaks
events <- events %>%
  mutate(outbreak_subsequent_month = month > outbreak_start_month & month <= outbreak_end_month) %>%
  mutate(outbreak_start = month == outbreak_start_month)  %>%
  mutate(disease_country_combo_unreported = is.na(immediate_report)) %>%
  mutate_at(.vars = c("outbreak_subsequent_month", "outbreak_start"), ~replace_na(., FALSE))

# identify endemic events
endemic_status_present <- tbl(conn, "nowcast_boost_augment_predict")  %>%
  collect() %>%
  mutate(cases = as.integer(predicted_cases)) %>%
  mutate(cases = coalesce(cases, predicted_cases)) %>%
  filter(cases > 0) %>%
  select(country_iso3c, report_year, report_semester, disease) %>%
  mutate(report_year = as.integer(report_year)) %>%
  mutate(report_semester = as.integer(report_semester))

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
  ungroup() #%>%
#mutate(endemic = ifelse(outbreak_start, FALSE, endemic))
# ^ this last mutate covers cases where the outbreak makes it into the semester report. the first month of the outbreak should still count.
# on the other hand, there are outbreaks that are reported when it really is already endemic, eg rabies, so commenting out for now
# events %>% filter(outbreak_start, endemic) %>% View

# add all combos of disease/country

return(events)

}
