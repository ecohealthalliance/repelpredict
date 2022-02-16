devtools::load_all()

model_object <-  network_lme_model()

#repeldata::repel_local_download()
conn <- repeldata::repel_remote_conn()

diseases <- repel_init(model_object, conn,
                       remove_single_country_disease = FALSE,
                       remove_non_primary_taxa_disease = FALSE,
                       clean_disease_names = FALSE) %>%
  mutate(disease = recode(disease,
                          "high pathogenicity avian influenza es" = "highly pathogenic avian influenza",
                          "influenza a es of high pathogenicity" = "highly pathogenic avian influenza" )) %>% # manual fix - should be addressed in wahis package
  filter(!disease_country_combo_unreported) %>%
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

readr::write_csv(diseases_recode, here::here("inst", "lookup", "network_diseases_recode.csv"))

diseases_recode_rare <- diseases %>%
  mutate(disease_recode = janitor::make_clean_names(disease)) %>%
  mutate(disease_recode_rare = ifelse(n <= rare, "rare_disease", disease_recode)) %>%
  select(-n)

# readr::write_csv(diseases_recode_rare, here::here("inst", "lookup", "network_diseases_recode_rare.csv"))
