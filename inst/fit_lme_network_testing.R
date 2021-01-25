devtools::load_all()
library(repeldata)
library(repelpredict)
library(tidyverse)
library(lubridate)
library(tidymodels)
library(stringi)
library(lme4)

# trade_vars <- vroom::vroom(here::here("tmp/trade_vars.csv"))
#
# # read in outbreak events - filter for immediate notifications
# events <- collect(tbl(repel_local_conn(), "outbreak_reports_events")) %>%
#   mutate_at(vars(contains("date")), as.Date)
#
# immediate_events <- filter(events, report_type == "immediate notification") %>%
#   filter(!is.na(country_iso3c)) # TODO: fix these events to be assigned to a current country
#
# # review
# immediate_events %>%
#   count(reason_for_notification, sort = TRUE)
#
# immediate_events %>%
#   count(disease, sort = TRUE)
#
# # Remove disease that have have reports in only one country_iso3c
# countries_per_disease <- immediate_events %>%
#   group_by(disease) %>%
#   summarize(n_countries = length(unique(country_iso3c))) %>%
#   arrange(desc(n_countries))
#
# immediate_events2 <- immediate_events %>%
#   filter(disease %in% countries_per_disease$disease[countries_per_disease$n_countries > 1])
#
# # get number of events started and ended in a given month
# event_starts <- immediate_events2 %>%
#   mutate(start_month = floor_date(date_of_start_of_the_event, unit = "months")) %>%
#   group_by(country_iso3c, disease, start_month) %>%
#   summarize(outbreak_start = n())
#
# event_ends <- immediate_events2 %>%
#   mutate(end_month = ceiling_date(date_of_start_of_the_event, unit = "months")) %>%
#   group_by(country_iso3c, disease, end_month) %>%
#   summarize(outbreak_end = -n())
#
# # determine if outbreak is ongoing in a given month
# tl <- immediate_events2 %>%
#   tidyr::expand(
#     country_iso3c,
#     disease,
#     month = seq.Date(
#       from = floor_date(min(immediate_events2$date_of_start_of_the_event, na.rm = TRUE), unit = "months"),
#       to = Sys.Date(),
#       by = "months")
#   ) %>%
#   left_join(mutate(event_starts, country_iso3c, disease, month = start_month, outbreak_start, .keep = "none")) %>%
#   left_join(mutate(event_ends, country_iso3c, disease, month = end_month, outbreak_end, .keep = "none")) %>%
#   mutate_at(c("outbreak_start", "outbreak_end"), ~coalesce(., 0)) %>%
#   group_by(country_iso3c, disease) %>%
#   arrange(month) %>%
#   mutate(outbreak_ongoing = as.numeric(cumsum(outbreak_start + outbreak_end) > 0)) %>%
#   ungroup()
#
# # reshape wide
# tl_wide <- tl %>%
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
# # remove month and get total number of unique combinations of ongoing + outbreak start for each disease/country
# tl_compressed <- tl_all %>%
#   select(-month) %>%
#   group_by_all() %>%
#   count() %>%
#   ungroup() %>%
#   select(country_iso3c, disease, count = n, outbreak_start, everything()) %>%
#   arrange(disease, desc(count), country_iso3c)
#
#
# # indicate whether outbreak is ongoing in continent or world in previous month
# # sum of other countries
#
# continent_lookup <- tl %>%
#   distinct(country_iso3c) %>%
#   mutate(continent = countrycode::countrycode(country_iso3c, origin = "iso3c", destination = "continent"))
#
# tl_long <- tl_all %>%
#   pivot_longer(cols = AFG:ZWE)
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
#
# scale2 <- function(x, na.rm = FALSE) (x - mean(x, na.rm = na.rm)) / sd(x, na.rm)
#
# trade_names <- colnames(trade_vars2)[str_detect(colnames(trade_vars2), "trade_")]
#
# t3 <- left_join(t2,trade_vars2) %>%
#   drop_na() %>%
#   mutate(country_iso3c = as.factor(country_iso3c)) %>%
#   mutate(disease = as.factor(disease)) %>%
#   mutate_at(trade_names, scale2)
#
# vroom::vroom_write(t3, here::here("tmp/lme_dat.csv"))

dat <- vroom::vroom(here::here("tmp/lme_dat.csv"))
trade_names <- colnames(dat)[str_detect(colnames(dat), "trade_")]

# V1
# frm <- as.formula(paste0("outbreak_start ~ ", paste(trade_names, collapse = " + "), " + (1 | country_iso3c) + (1 | disease)"))
# mod <- lme4::glmer(data = dat, family = binomial, formula = frm)
# write_rds(mod, here::here("tmp/lme_mod_v1.rds"))

# V2
trade_names <- c("trade_asses", "trade_buffaloes", "trade_camels", "trade_cattle", "trade_chickens", "trade_ducks", "trade_goats",
                 "trade_horses", "trade_mules", "trade_pigs", "trade_rabbits_and_hares", "trade_rodents_other", "trade_sheep", "trade_turkeys",
                 "trade_meat_and_edible_meat_offal", "trade_dairy_produce", "trade_furskins_and_artificial_fur", "trade_raw_hides_and_skins_other_than_furskins_and_leather")
dat2 <- dat %>%
  select(country_iso3c, disease, month, outbreak_start, starts_with("n_outbreaks"), all_of(trade_names))

frm2 <- as.formula(paste0("outbreak_start ~ ", paste0(trade_names, " + (", trade_names, " | disease) + (", trade_names, "| country_iso3c)", collapse = " + ")))

# Set up parallel
all_cores <- parallel::detectCores(logical = FALSE)
cl <- parallel::makePSOCKcluster(all_cores)
doParallel::registerDoParallel(cl)

mod2 <- lme4::glmer(data = dat2, family = binomial, formula = frm2)
write_rds(mod2, here::here("tmp/lme_mod_v2.rds"))

parallel::stopCluster(cl = cl)

# Questions
# why control for random effects in country/disease here but not nowcast?
# this version doesnt tell where it is coming frm

# Model options
# Fit in: lme4
# Fallback: glmmTMB (or brms with MaxLik)#lme4::glmer(disease_start ~ (trade_var_1 | disease*country_iso3c) + (trade_var_2 | disease*country_iso3c) + (season_or_month | disease*country_iso3c), ...)
# See here: https://mac-theobio.github.io/QMEE/MultivariateMixed.html\

