devtools::load_all()

model_object <-  nowcast_boost_model(disease_status_model = NULL, cases_model = NULL)

#repeldata::repel_local_download()
conn <- repeldata::repel_remote_conn()

diseases <- repel_init(model_object, conn, clean_disease_names = F) %>%
  group_by(disease) %>%
  count() %>%
  ungroup() %>%
  arrange(-n)

hist(diseases$n)
summary(diseases$n)
rare <- quantile(diseases$n, 0.15)

diseases_recode <- diseases %>%
  mutate(disease_recode = janitor::make_clean_names(disease)) %>%
  select(-n)

readr::write_csv(diseases_recode, here::here("inst", "lookup", "nowcast_diseases_recode.csv"))

diseases_recode_rare <- diseases %>%
  mutate(disease_recode = janitor::make_clean_names(disease)) %>%
  mutate(disease_recode_rare = ifelse(n <= rare, "rare_disease", disease_recode)) %>%
  select(-n)

readr::write_csv(diseases_recode_rare, here::here("inst", "lookup", "nowcast_diseases_recode_rare.csv"))
