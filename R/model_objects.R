#' Produces nowcast baseline model object
#' @import repeldata dplyr tidyr
#' @return a list with description and classes
#' @export
nowcast_baseline_model <- function(){
  structure(list(description = "Nowcast baseline model"),
            class = c("nowcast_baseline", "nowcast_model", "repel_model"))
}

#' Produces nowcast gam model object
#' @import repeldata dplyr tidyr
#' @return a list with description and classes
#' @export
nowcast_gam_model <- function(){
  structure(list(description = "Nowcast GAM model"),
            class = c("nowcast_gam", "nowcast_model", "repel_model"))
}

#' Produces nowcast xgboost model object
#' @import repeldata dplyr tidyr
#' @return a list with description and classes
#' @export
nowcast_boost_model <- function(disease_status_model = NULL,
                                cases_model = NULL){
  structure(list(description = "Nowcast XGBoost model",
                 disease_status_model = disease_status_model,
                 cases_model = cases_model),
            class = c("nowcast_boost", "nowcast_tree", "nowcast_model", "repel_model"))
}


#' Produces network lme model object
#' @import repeldata dplyr tidyr
#' @return a list with description and classes
#' @export
network_lme_model <- function(network_model = NULL, network_scaling_values = NULL){
  structure(list(description = "Network LME model",
                 network_model = network_model,
                 network_scaling_values = network_scaling_values),
            class = c("network_lme", "network_model", "repel_model"))
}

#' Produces network brms model object
#' @import repeldata dplyr tidyr
#' @return a list with description and classes
#' @export
network_brms_model <- function(network_model = NULL, network_scaling_values = NULL){
  structure(list(description = "Network BRMS model",
                 network_model = network_model,
                 network_scaling_values = network_scaling_values),
            class = c("network_brms", "network_model", "repel_model"))
}
