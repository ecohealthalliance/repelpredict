devtools::load_all()

library(repeldata)
library(repelpredict)
library(tidyverse)
library(lubridate)
library(tidymodels)
library(stringi)
library(lme4)

trade_vars <- vroom::vroom(here::here("tmp/trade_vars.csv"))


# read in outbreak events - filter for immediate notifications
events <- collect(tbl(repel_local_conn(), "outbreak_reports_events")) %>%
  mutate_at(vars(contains("date")), as.Date)

immediate_events <- filter(events, report_type == "immediate notification") %>%
  filter(!is.na(country_iso3c)) # TODO: fix these events to be assigned to a current country

# review
immediate_events %>%
  count(reason_for_notification, sort = TRUE)

immediate_events %>%
  count(disease, sort = TRUE)

# Remove disease that have have reports in only one country_iso3c
countries_per_disease <- immediate_events %>%
  group_by(disease) %>%
  summarize(n_countries = length(unique(country_iso3c))) %>%
  arrange(desc(n_countries))

immediate_events2 <- immediate_events %>%
  filter(disease %in% countries_per_disease$disease[countries_per_disease$n_countries > 1])

# get number of events started and ended in a given month
event_starts <- immediate_events2 %>%
  mutate(start_month = floor_date(date_of_start_of_the_event, unit = "months")) %>%
  group_by(country_iso3c, disease, start_month) %>%
  summarize(outbreak_start = n())

event_ends <- immediate_events2 %>%
  mutate(end_month = ceiling_date(date_of_start_of_the_event, unit = "months")) %>%
  group_by(country_iso3c, disease, end_month) %>%
  summarize(outbreak_end = -n())

# determine if outbreak is ongoing in a given month
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

# reshape wide
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

# remove month and get total number of unique combinations of ongoing + outbreak start for each disease/country
tl_compressed <- tl_all %>%
  select(-month) %>%
  group_by_all() %>%
  count() %>%
  ungroup() %>%
  select(country_iso3c, disease, count = n, outbreak_start, everything()) %>%
  arrange(disease, desc(count), country_iso3c)

# old model code
# frm <- as.formula(paste0(
#   "outbreak_start ~ 1 | ",
#   paste(stri_subset_regex(colnames(tl_compressed), "^[A-Z]{3}$"), collapse = " + "),
#   " - 1 | 1",
#   collapse = "")
# )
# wgts <- tl_compressed$count[seq(1, nrow(tl_compressed), by = n_distinct(tl_compressed$country_iso3c))]
# mmod <- mnlogit(frm, weights = wgts, choiceVar = "country_iso3c", data = tl_compressed, ncores = 32, print.level = 5)

# indicate whether outbreak is ongoing in continent or world in previous month
# sum of other countries

continent_lookup <- tl %>%
  distinct(country_iso3c) %>%
  mutate(continent = countrycode::countrycode(country_iso3c, origin = "iso3c", destination = "continent"))

tl_long <- tl_all %>%
  pivot_longer(cols = AFG:ZWE)

tl_ongoing_anywhere <- tl_long %>%
  group_by(country_iso3c, disease, month) %>%
  summarize(n_outbreaks_anywhere_lag = sum(value)) %>%
  ungroup() %>%
  mutate(month = floor_date(month+31, unit = "month"))

tl_ongoing_continent <- tl_long %>%
  left_join(continent_lookup, by = "country_iso3c") %>%
  left_join(continent_lookup, by = c("name" = "country_iso3c")) %>%
  filter(continent.x == continent.y) %>%
  group_by(country_iso3c, disease, month) %>%
  summarize(n_outbreaks_continent_lag = sum(value)) %>%
  ungroup() %>%
  mutate(month = floor_date(month+31, unit = "month"))

t2 <- tl %>%
  mutate(outbreak_start = outbreak_start > 0) %>%
  select(-outbreak_end, -outbreak_ongoing) %>%
  left_join(tl_ongoing_anywhere) %>%
  left_join(tl_ongoing_continent)

# add in trade vars
year_lookup <- map_dfr(unique(trade_vars$year), function(yr){
  tibble(year = yr, month = seq(ymd(paste0(yr, "-01-01")), ymd(paste0(yr, "-12-01")), by = 'months'))
})

trade_vars2 <- trade_vars %>%
  group_by(country_destination, year, group_name) %>%
  summarize(value = sum(value)) %>%
  ungroup() %>%
  mutate(group_name = str_extract(group_name, "[^;]+")) %>%
  mutate(group_name = paste0("trade_", group_name)) %>%
  pivot_wider(names_from = group_name, values_from = value) %>%
  janitor::clean_names() %>%
  rename(country_iso3c = country_destination)  %>%
  left_join(year_lookup) %>%
  select(-year)

scale2 <- function(x, na.rm = FALSE) (x - mean(x, na.rm = na.rm)) / sd(x, na.rm)

trade_names <- colnames(trade_vars2)[str_detect(colnames(trade_vars2), "trade_")]

t3 <- left_join(t2,trade_vars2) %>%
  drop_na() %>%
  mutate(country_iso3c = as.factor(country_iso3c)) %>%
  mutate(disease = as.factor(disease)) %>%
  mutate_at(trade_names, scale2)

vroom::vroom_write(t3, here::here("tmp/lme_dat.csv"))

# frm <-as.formula(paste0( "outbreak_start ~ ",
#         paste(paste0("(", trade_names , " | disease : country_iso3c)"), collapse = " + ")
# ))

# frm <- as.formula("outbreak_start ~ trade_chickens + trade_pigs + trade_horses + trade_sheep + (1 | country_iso3c) + (1 | disease)")

frm <- as.formula(paste0("outbreak_start ~ ", paste(trade_names, collapse = " + "), " + (1 | country_iso3c) + (1 | disease)"))

mod <- lme4::glmer(data = t3, family = binomial,
                   formula = frm)

write_rds(mod, here::here("tmp/lme_mod_v1.rds"))

# why control for random effects in country/disease here but not nowcast?
# this version doesnt tell where it is coming frm

# mod2 <- lmer(distance ~ age + (1|Subject), data=data1, REML=F) # intercept varies by age (same slope for each subject)
# mod3 <- lmer(distance ~ age + (1|Subject) + (0+age|Subject), data=data1, REML=F) # intercept and slope vary by age for each subject

# disease_start ~ trade_var_1 + (trade_var_1 | disease) + (trade_var_1 | country_iso3c) + trade_var_2 + (trade_var_2 | disease) + (trade_var_2 | country_iso3c)

# looking at random slopes by attitude
# politeness.model = lmer(frequency ~ attitude +
#                           gender + (1+attitude|subject) +
#                           (1+attitude|scenario),
#                         data=politeness,
#                         REML=FALSE)

# Error in pwrssUpdate(pp, resp, tol = tolPwrss, GQmat = GQmat, compDev = compDev,  :
#                        (maxstephalfit) PIRLS step-halvings failed to reduce deviance in pwrssUpdate
#                      In addition: Warning message:
#                        Some predictor variables are on very different scales: consider rescaling




# Fit in: lme4
# Fallback: glmmTMB (or brms with MaxLik)#lme4::glmer(disease_start ~ (trade_var_1 | disease*country_iso3c) + (trade_var_2 | disease*country_iso3c) + (season_or_month | disease*country_iso3c), ...)
# See here: https://mac-theobio.github.io/QMEE/MultivariateMixed.html\


# Tidymodels --------------------------------------------------------------

# why are we controlling for random effects not but not in nowcast

library(tidymodels)
#devtools::install_github("tidymodels/multilevelmod")
library(multilevelmod)
dat <- readr::read_rds(here::here("tmp/lme_dat.rds"))

dat_split <- initial_split(dat)
dat_train <- training(dat_split)
dat_test <- testing(dat_split)

set.seed(9599)
folds <- group_vfold_cv(dat_train, group = country_iso3c)

lme_spec <-
  linear_reg() %>%
  set_mode("regression") %>%
  set_engine("lmer")


