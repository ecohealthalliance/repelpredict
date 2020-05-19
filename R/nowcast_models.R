#' Produces nowcast baseline model object
#' @import repeldata dplyr tidyr
#' @return a list with description and classes
nowcast_baseline_model <- function(){
  structure(list(description = "Nowcast baseline model",
                 class = c("nowcast_baseline", "nowcast_model")))
}
