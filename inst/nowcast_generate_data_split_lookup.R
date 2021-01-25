devtools::load_all()
library(tidyverse)
set.seed(2222)

model_object <-  nowcast_boost_model(disease_status_model = NULL, cases_model = NULL)

generate_indices <- function(n, prop){
  init_sample <- sample(n, ceiling(prop*n), replace = FALSE)
  init_sample_next <- ifelse(init_sample < n, init_sample + 1, init_sample - 1)
  return(unique(sort(c(init_sample, init_sample_next))))
}

conn <- repeldata::repel_local_conn()
#repeldata::repel_local_download()

all_dat <- repel_init(model_object, conn) %>%
  select(country_iso3c, report_year, report_semester, taxa, disease, disease_population) %>%
  arrange() %>%
  distinct()

# expand grid out to 2030, for every country
all_dat_grid <- all_dat %>%
  distinct(disease, disease_population, taxa) %>%
  expand_grid(country_iso3c = unique(all_dat$country_iso3c),
              report_year = min(all_dat$report_year):2030,
              report_semester = 1:2) %>%
  arrange(country_iso3c, taxa, disease, disease_population, report_year, report_semester) %>%
  group_by(country_iso3c, taxa, disease, disease_population) %>%
  mutate(validation_set = row_number() %in% generate_indices(n = n(), prop = 0.1))  %>%
  ungroup()

message(paste0("validation set is ", round(100*sum(all_dat_grid$validation_set)/nrow(all_dat_grid)), "% of data"))

readr::write_csv(all_dat_grid, gzfile(here::here("inst/lookup/validation_split_lookup.csv.gz")))
