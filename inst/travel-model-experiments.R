library(repeldata)
library(repelpredict)
library(tidyverse)
library(lubridate)
library(tidymodels)
library(stringi)
library(lme4)

events <- collect(tbl(repel_local_conn(), "outbreak_reports_events")) %>%
  mutate_at(vars(contains("date")), as.Date)

immediate_events <- filter(events, report_type == "immediate notification") %>%
  filter(!is.na(country_iso3c)) # TODO: fix these events to be assigned to a current country


immediate_events %>%
  count(reason_for_notification, sort = TRUE)

immediate_events %>%
  count(disease, sort = TRUE)

countries_per_disease <- immediate_events %>%
  group_by(disease) %>%
  summarize(n_countries = length(unique(country_iso3c))) %>%
  arrange(desc(n_countries))


# Remove disease that have have reports in only one country_iso3c
immediate_events2 <- immediate_events %>%
  filter(disease %in% countries_per_disease$disease[countries_per_disease$n_countries > 1])

event_starts <- immediate_events2 %>%
  mutate(start_month = floor_date(date_of_start_of_the_event, unit = "months")) %>%
  group_by(country_iso3c, disease, start_month) %>%
  summarize(outbreak_start = n())
event_ends <- immediate_events2 %>%
  mutate(end_month = ceiling_date(date_of_start_of_the_event, unit = "months")) %>%
  group_by(country_iso3c, disease, end_month) %>%
  summarize(outbreak_end = -n())

tl <- immediate_events2 %>%
  tidyr::expand(
    country_iso3c,
    disease,
    month = seq.Date(
      from = floor_date(min(immediate_events2$date_of_start_of_the_event, na.rm = TRUE), unit = "months"),
      to = Sys.Date(),
      by = "months")
  ) %>%
  left_join(mutate(event_starts, country_iso3c, disease, month = start_month, outbreak_start, .keep = "none")) %>%
  left_join(mutate(event_ends, country_iso3c, disease, month = end_month, outbreak_end, .keep = "none")) %>%
  mutate_at(c("outbreak_start", "outbreak_end"), ~coalesce(., 0)) %>%
  group_by(country_iso3c, disease) %>%
  arrange(month) %>%
  mutate(outbreak_ongoing = as.numeric(cumsum(outbreak_start + outbreak_end) > 0)) %>%
  ungroup()

tl_wide <- tl %>%
  select(country_iso3c, disease, month, outbreak_ongoing) %>%
  pivot_wider(names_from = country_iso3c, values_from = outbreak_ongoing)

tl_all <- tl %>%
  select(country_iso3c, disease, month, outbreak_start) %>%
  left_join(tl_wide, by = c("disease", "month")) %>%
  group_split(country_iso3c) %>%
  map_dfr(function(z) {
    z[[z$country_iso3c[1]]] <- 0
    z
  }) %>%
  mutate(outbreak_start = outbreak_start > 0)

tl_compressed <- tl_all %>%
  select(-month) %>%
  group_by_all() %>%
  count() %>%
  ungroup() %>%
  select(country_iso3c, disease, count = n, outbreak_start, everything()) %>%
  arrange(disease, desc(count), country_iso3c)

frm <- as.formula(paste0(
  "outbreak_start ~ 1 | ",
  paste(stri_subset_regex(colnames(tl_compressed), "^[A-Z]{3}$"), collapse = " + "),
  " - 1 | 1",
  collapse = "")
)
wgts <- tl_compressed$count[seq(1, nrow(tl_compressed), by = n_distinct(tl_compressed$country_iso3c))]
mmod <- mnlogit(frm, weights = wgts, choiceVar = "country_iso3c", data = tl_compressed, ncores = 32, print.level = 5)

#disease_start ~ (trade_var_1 | disease*country_iso3c) + (trade_var_2 | disease*country_iso3c) + (season_or_month | disease*country_iso3c)

glmer
