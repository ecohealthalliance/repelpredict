#' define generic function
#' @export
repel_clean_disease_names <- function(x, ...){
  UseMethod("repel_clean_disease_names")
}


#' Clean network disease names
#' @param model_object network model object
#' @param df dataframe with disease column
#' @import  dplyr tidyr stringr
#' @importFrom vroom vroom
#' @export
repel_clean_disease_names.network_model <- function(model_object, df){

  assertthat::has_name(df, "disease")

  # from inst/network_generate_disease_lookup.R
  diseases_recode <- vroom::vroom(system.file("lookup", "nowcast_diseases_recode.csv",  package = "repelpredict"), col_types = cols(
    disease = col_character(),
    disease_recode = col_character()
  ))
  df %>%
    left_join(diseases_recode, by = "disease", copy = TRUE) %>% # copy TRUE because sometimes cleaning names on the db connection
    select(-disease) %>%
    rename(disease = disease_recode)
}
