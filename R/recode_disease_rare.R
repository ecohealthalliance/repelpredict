#' @import dplyr tidyr
#' @importFrom readr read_csv
#' @importFrom assertthat has_name
#' @noRd
recode_disease_rare <- function(data){

  assertthat::has_name(data, "disease")

  # read in static file from inst/generate_disease_lookup.R
  disease_lookup_rare <- read_csv(system.file("lookup", "nowcast_diseases_recode_rare.csv", package = "repelpredict"),
                                  col_types = cols(
                                    disease = col_character(),
                                    disease_recode = col_character(),
                                    disease_recode_rare = col_character()
                                  )) %>%
    select(disease = disease_recode, disease_recode = disease_recode_rare) %>%
    arrange(disease_recode)

  data_recoded <- data  %>%
    left_join(disease_lookup_rare, by = "disease") %>%
    select(-disease) %>%
    select(report_year, report_semester, disease = disease_recode, everything())

  return(data_recoded)
}
