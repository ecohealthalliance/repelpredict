devtools::load_all()

conn <- repel_local_conn()
lookup <- case_lag_lookup(conn)

readr::write_csv(lookup, "inst/files/case_lag_lookup.csv.xz")
#readr::read_csv("inst/files/case_lag_lookup.csv.xz", col_types = case_lag_lookup_specs())
