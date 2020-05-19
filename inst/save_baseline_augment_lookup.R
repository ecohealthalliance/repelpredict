library(repeldata)

conn <- repel_local_conn()
lookup <- baseline_augment_lookup(conn)

readr::write_csv(lookup, "inst/files/baseline_augment_lookup.csv.xz")
# readr::read_csv("inst/files/baseline_augment_lookup.csv.xz", col_types = baseline_augment_lookup_specs())
