#' define generic augment
repel_augment <- function(x){
  UseMethod("repel_augment")
}


#' Augment nowcast baseline model object
#'
#'

repel_augment.nowcast_baseline <- function(model_object, conn, newdata) {

  assertthat::has_name(newdata, c("country_iso3c", "report_year", "report_semester", "disease", "taxa"))


  augmented_data <- newdata %>%
    mutate(report_period = as.numeric(paste0(report_year, report_semester))) %>% # temp for filtering
    left_join(model_lookup, by = c("country_iso3c", "disease", "taxa", "report_period"))

  return(augmented_data)
}
