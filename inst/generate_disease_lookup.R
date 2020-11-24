devtools::load_all()

#repeldata::repel_local_download()
conn <- repeldata::repel_local_conn()

diseases <- init_annual_reports_animal_hosts(conn) %>%
  group_by(disease) %>%
  count() %>%
  ungroup() %>%
  arrange(-n)

hist(diseases$n)
summary(diseases$n)
rare <- quantile(diseases$n, 0.15)

diseases_recode <- diseases %>%
  mutate(disease_recode = janitor::make_clean_names(disease)) %>%
  # mutate(disease_recode = ifelse(n <= rare, "rare_disease", disease_recode)) %>%
  select(-n)

readr::write_csv(diseases_recode, here::here("inst", "lookup", "diseases_recode.csv"))

