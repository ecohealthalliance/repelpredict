#' define generic function
#' @export
repel_clean_disease_names <- function(x, ...){
  UseMethod("repel_clean_disease_names")
}


#' Clean disease names
#' @param model_object model object (network or nowcast)
#' @param df dataframe with disease column
#' @import  dplyr tidyr stringr
#' @importFrom vroom vroom
#' @export
repel_clean_disease_names.repel_model <- function(model_object, df){

  assertthat::has_name(df, "disease")

  # from inst/nowcast_generate_disease_lookup.R
  nowcast_diseases_recode <- vroom::vroom(system.file("lookup", "nowcast_diseases_recode.csv",  package = "repelpredict"), col_types = cols(
    disease = col_character(),
    disease_recode = col_character()
  ))

  # from inst/network_generate_disease_lookup.R
  network_diseases_recode <- vroom::vroom(system.file("lookup", "network_diseases_recode.csv",  package = "repelpredict"), col_types = cols(
    disease = col_character(),
    disease_recode = col_character()
  ))

  diseases_recode <- switch(tail(class(model_object), 2)[1],
                      "nowcast_model" = nowcast_diseases_recode,
                      "network_model" = network_diseases_recode)

  if(is.null(diseases_recode)) diseases_recode <- bind_rows(nowcast_diseases_recode, network_diseases_recode) %>% distinct

  df %>%
    left_join(diseases_recode, by = "disease", copy = TRUE) %>% # copy TRUE because sometimes cleaning names on the db connection
    rename(disease_name_uncleaned = disease, disease = disease_recode)
}
