devtools::load_all()
conn <- repeldata::repel_local_conn()
model_object <- network_brms_model()

# target taxa by disease

diseases_recode <- vroom::vroom(system.file("lookup", "nowcast_diseases_recode.csv",  package = "repelpredict"), col_types = cols(
  disease = col_character(),
  disease_recode = col_character()
))

outbreak_reports_outbreaks <- tbl(conn, "outbreak_reports_outbreaks") %>%
  select(taxa = species_name, "report_id") %>%
  collect() %>%
  mutate(taxa = ifelse(str_detect(taxa, "goats|sheep"), "sheep/goats", taxa))  %>%
  distinct() %>%
  filter(taxa %in% taxa_list)

disease_taxa_list <- tbl(conn, "outbreak_reports_events") %>%
  distinct(report_id, disease) %>%
  collect() %>%
  left_join(diseases_recode, by = "disease") %>%
  rename(disease_pre_clean = disease, disease = disease_recode) %>%
  drop_na() %>%
  inner_join(outbreak_reports_outbreaks) %>%
  group_by(disease, disease_pre_clean, taxa) %>%
  count() %>%
  ungroup() %>%
  arrange(disease, -n) %>%
  select(-n)

readr::write_csv(disease_taxa_list, here::here("inst", "lookup", "disease_taxa_lookup.csv"))

