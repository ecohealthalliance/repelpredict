devtools::load_all()

#repeldata::repel_local_download()
conn <- repeldata::repel_local_conn()

traindat_diseases <- repel_cases_train(conn) %>%
  group_by(disease) %>%
  count() %>%
  ungroup() %>%
  arrange(-n)

hist(traindat_diseases$n)
summary(traindat_diseases$n)
rare <- quantile(traindat_diseases$n, 0.15)

traindat_diseases_recode <- traindat_diseases %>%
  mutate(disease_recode = ifelse(n <= rare, "rare_disease", disease)) %>%
  select(-n)

readr::write_csv(traindat_diseases_recode, here::here("inst", "lookup", "traindat_diseases_recode.csv"))

