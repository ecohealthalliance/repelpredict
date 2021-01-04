devtools::load_all()
conn <- repeldata::repel_local_conn()


# Process trade vars (move to wahis) --------------------------------------
# DBI::dbListTables(conn)
#
# connect_static <- DBI::dbReadTable(conn, "connect_yearly_vars") %>%
#   collect()
#
# trade_vars <- connect_static %>%
#   pivot_longer(cols = -c("country_origin" , "country_destination",  "year" ))
#
# ots_lookup <- DBI::dbReadTable(conn, "connect_ots_lookup") %>%
#   collect() %>%
#   mutate(source = "ots_trade_dollars") %>%
#   select(product_code, group_name, source)
#
# connect_fao_lookup <- DBI::dbReadTable(conn, "connect_fao_lookup") %>%
#   collect() %>%
#   mutate(source = "fao_livestock_heads") %>%
#   mutate(item_code = as.character(item_code)) %>%
#   rename(group_name = item, product_code = item_code)
#
# trade_lookup <- bind_rows(ots_lookup, connect_fao_lookup)
#
# trade_vars_lookup <- trade_vars %>%
#   distinct(name) %>%
#   mutate(code =  str_remove(name, "trade_dollars_|livestock_heads_")) %>%
#   mutate(source = case_when(
#     str_detect(name, "trade_dollars_") ~ "ots_trade_dollars",
#     str_detect(name, "livestock_heads_") ~ "fao_livestock_heads",
#     name == "n_human_migrants" ~ "un_human_migration",
#     name == "n_tourists" ~ "un_wto_tourism"
#   )) %>%
#   left_join(trade_lookup, by = c("source" = "source", "code" = "product_code"))
#
# trade_vars2 <- trade_vars %>%
#   left_join(trade_vars_lookup) %>%
#   filter(group_name != "Animals; live") %>%  # remove from OTS because we have FAO
#   mutate(value = as.numeric(value)) %>%
#   mutate(value = replace_na(value, 0)) %>%
#   group_by(country_origin, country_destination, year, source, group_name) %>%
#   summarize(value = sum(value, na.rm = TRUE)) %>%
#   ungroup()
#
# vroom::vroom_write(trade_vars2, here::here("tmp/trade_vars.csv"))


trade_vars <- vroom::vroom(here::here("tmp/trade_vars.csv"))



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
#
# Fit in: lme4
# Fallback: glmmTMB (or brms with MaxLik)
#lme4::glmer(disease_start ~ (trade_var_1 | disease*country_iso3c) + (trade_var_2 | disease*country_iso3c) + (season_or_month | disease*country_iso3c), ...)
# See here: https://mac-theobio.github.io/QMEE/MultivariateMixed.html
