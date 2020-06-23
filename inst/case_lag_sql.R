

lag_dat <- tbl(conn, "annual_reports_animal_hosts") %>%
  filter(taxa %in% taxa_list) %>%
  filter(report_semester != "0") %>%
  select(report, country, country_iso3c, report_year, report_semester, disease, disease_population, serotype, disease_status, taxa, cases) %>%
  mutate(report_period = glue::glue("{.$report_year}{.$report_semester}")) %>% # temp for filtering
  # left_join(expand(., nesting(country_iso3c, disease, disease_population, taxa), report_period), .,  by = c("country_iso3c", "disease", "disease_population", "taxa", "report_period")) %>%
  # group_by(country_iso3c, disease, disease_population, taxa) %>%
  # mutate(cases_lag1 = lag(cases, order_by = report_period, n = 1, default = NA)) %>%
  # mutate(cases_lag2 = lag(cases, order_by = report_period, n = 2, default = NA)) %>%
  # mutate(cases_lag3 = lag(cases, order_by = report_period, n = 3, default = NA)) %>%
  # mutate(disease_status_lag1 = lag(disease_status, order_by = report_period, n = 1, default = NA)) %>%
  # mutate(disease_status_lag2 = lag(disease_status, order_by = report_period, n = 2, default = NA)) %>%
  # mutate(disease_status_lag3 = lag(disease_status, order_by = report_period, n = 3, default = NA)) %>%
  # ungroup() %>%
  # select(country_iso3c, disease, disease_population, taxa, report_period, disease_status, starts_with("cases"), starts_with("disease_status"))

show_query(lag_dat)
query <- "SELECT 'report', 'country', 'country_iso3c', 'report_year', 'report_semester', 'disease', 'disease_population', 'serotype', 'disease_status', 'taxa', 'cases'
                        FROM (SELECT *
                                FROM 'annual_reports_animal_hosts'
                              WHERE ('taxa' IN ('goats', 'sheep', 'sheep/goats', 'cattle', 'birds', 'camelidae', 'dogs', 'equidae', 'cats', 'cervidae', 'swine', 'buffaloes', 'hares/rabbits')))
                        WHERE ('report_semester' <> '0'"
res <- DBI::dbSendQuery(conn, query)
schema <- dbFetch(res)
