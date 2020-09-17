#' @import repeldata dplyr tidyr stringr
#'@noRd
init_annual_reports_animal_hosts <- function(conn){

  tbl(conn, "annual_reports_animal_hosts") %>%
    mutate(taxa = ifelse(taxa %in% c("goats", "sheep"), "sheep/goats", taxa)) %>%
    filter(taxa %in% taxa_list) %>%
    filter(report_semester != "0") %>%
    select(all_of(grouping_vars), disease_status, cases) %>%
    collect() %>%
    group_by_at(grouping_vars) %>%
    summarize(cases = as.integer(sum_na(suppressWarnings(as.integer(cases)))),
              disease_status = str_flatten(sort(unique(disease_status)), collapse = ",")) %>%
    ungroup() %>%
    mutate(disease_status = recode(disease_status,
                                   "absent,present"  = "present",
                                   "absent,suspected" = "suspected",
                                   "present,suspected" = "present"
    ))

}
