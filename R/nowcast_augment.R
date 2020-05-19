#' define generic augment
repel_augment <- function(x){
  UseMethod("repel_augment")
}


#' Augment nowcast baseline model object
#' @import repeldata dplyr tidyr
#'
repel_augment.nowcast_baseline <- function(model_object, conn, newdata) {

  # check newdata has correct input vars
  assertthat::has_name(newdata, c("country_iso3c", "report_year", "report_semester", "disease", "taxa"))

  # lookup table for augmenting
  model_lookup <- get_baseline_augment_lookup()

  # augment with cases and last three semesters
  augmented_data <- newdata %>%
    mutate(report_period = as.integer(paste0(report_year, report_semester))) %>%
    left_join(model_lookup, by = c("country_iso3c", "disease", "taxa", "report_period"))

  return(augmented_data)
}
