#' Produces nowcast baseline model object
#' @import repeldata dplyr tidyr
#' @return a list with description and classes
#' @export
nowcast_baseline_model <- function(){
  structure(list(description = "Nowcast baseline model"),
                 class = c("nowcast_baseline", "nowcast_model"))
}


#' Produces nowcast bart model object
#' @import repeldata dplyr tidyr
#' @return a list with description and classes
#' @export
nowcast_bart_model <- function(){
  structure(list(description = "Nowcast bart model"),
            class = c("nowcast_bart", "nowcast_model"))
}

