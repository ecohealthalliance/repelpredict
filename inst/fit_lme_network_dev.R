devtools::load_all()
# repeldata::repel_local_download()
conn <- repeldata::repel_local_conn()

# Fitting  ----------------------------------------------------------------
traindat <- annual_reports_animal_hosts_training(conn) %>%
  select(all_of(grouping_vars)) %>%
  distinct()
