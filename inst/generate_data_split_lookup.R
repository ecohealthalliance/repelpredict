devtools::load_all()
library(tidyverse)
set.seed(2222)

#repeldata::repel_local_download()
conn <- repeldata::repel_local_conn()

all_dat <- init_annual_reports_animal_hosts(conn) %>%
  select(country_iso3c, report_year, report_semester, taxa, disease, disease_population) %>%
  arrange() %>%
  distinct()

# expand grid out to 2030
all_dat <- all_dat %>%
  distinct(country_iso3c, disease, disease_population, taxa) %>%
  expand_grid(report_year = min(all_dat$report_year):2030, report_semester = 1:2) %>%
  arrange(country_iso3c, taxa, disease, disease_population, report_year, report_semester) %>%
  group_by(country_iso3c, taxa, disease, disease_population) %>%
  mutate(validation_set = row_number() %in% sample(n(), ceiling(0.2*n()), replace = FALSE)) %>%
  ungroup()

message(paste0("validation set is ", round(100*sum(all_dat$validation_set)/nrow(all_dat)), "% of data"))

readr::write_csv(all_dat, here::here("inst/lookup/validation_split_lookup.csv"))
