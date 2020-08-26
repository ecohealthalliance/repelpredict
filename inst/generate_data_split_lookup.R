devtools::load_all()
library(tidyverse)
set.seed(2222)

#repeldata::repel_local_download()
conn <- repeldata::repel_local_conn()

all_dat <- tbl(conn, "annual_reports_animal_hosts") %>%
  filter(taxa %in% taxa_list) %>%
  filter(report_semester != "0") %>%
  select(country_iso3c, report_year, report_semester, taxa, disease, disease_population) %>%
  collect() %>%
  arrange() %>%
  distinct()

# adding 2020 onward
last_sem <- all_dat %>%
  arrange(report_year, report_semester) %>%
  distinct(report_year, report_semester) %>%
  tail(1)

if(last_sem$report_semester == 2){
  future_decade <- tibble(report_year = rep(seq(max(last_sem$report_year) + 1, max(last_sem$report_year) + 10), each = 2),
                          report_semester = rep(1:2, 10))
}else{
  future_decade <- tibble(report_year = c(max(last_sem$report_year) + 1,
                                          rep(seq(max(last_sem$report_year) + 2, max(last_sem$report_year) + 10), each = 2),
                                          max(last_sem$report_year) + 11),
                          report_semester = rep(2:1, 10))
}

future_reports <- all_dat %>%
  distinct(country_iso3c, disease, disease_population, taxa) %>%
  expand_grid(future_decade)

all_dat <- all_dat %>%
  bind_rows(future_reports) %>%
  arrange(country_iso3c, taxa, disease, disease_population, report_year, report_semester) %>%
  group_by(country_iso3c, taxa, disease, disease_population) %>%
  mutate(validation_set = row_number() %in% sample(n(), ceiling(0.2*n()), replace = FALSE)) %>%
  ungroup()

message(paste0("validation set is ", round(100*sum(all_dat$validation_set)/nrow(all_dat)), "% of data"))

qsave(all_dat, here::here("inst/lookup/validation_split_lookup.qs"))
