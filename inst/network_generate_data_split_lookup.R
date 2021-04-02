devtools::load_all()
library(tidyverse)
library(lubridate)
set.seed(2222)

model_object <-  network_lme_model()

conn <- repeldata::repel_local_conn()
#repeldata::repel_local_download()

all_dat <- repel_init(model_object, conn, remove_non_outbreak_events = TRUE) %>%
  distinct(country_iso3c, disease, month) %>%
  arrange()

# expand grid out to 2030, for every country
all_dat_grid <- all_dat %>%
  distinct(disease) %>%
  expand_grid(country_iso3c = unique(all_dat$country_iso3c),
              month = seq(min(all_dat$month), lubridate::ymd("2030-11-01"), by = "months")) %>%
  arrange(country_iso3c, disease, month) %>%
  group_by(country_iso3c, disease) %>%
  mutate(validation_set = row_number() %in% sample(n(), ceiling(0.2*n()), replace = FALSE))  %>%
  ungroup()

message(paste0("validation set is ", round(100*sum(all_dat_grid$validation_set)/nrow(all_dat_grid)), "% of data"))

vroom::vroom_write(all_dat_grid, gzfile(here::here("inst/lookup/network_validation_split_lookup.csv.gz")))
