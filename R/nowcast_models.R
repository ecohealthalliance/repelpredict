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


#' Produces nowcast gam model object
#' @import repeldata dplyr tidyr
#' @return a list with description and classes
#' @export
nowcast_gam_model <- function(){
  structure(list(description = "Nowcast GAM model"),
            class = c("nowcast_gam", "nowcast_model"))
}

#' Produces nowcast xgboost model object
#' @import repeldata dplyr tidyr
#' @return a list with description and classes
#' @export
nowcast_boost_model <- function(){
    structure(list(description = "Nowcast XGBoost model"),
            class = c("nowcast_boost", "nowcast_model"))
}

