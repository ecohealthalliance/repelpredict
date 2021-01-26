#' Produces network lme model object
#' @import repeldata dplyr tidyr
#' @return a list with description and classes
#' @export
network_lme_model <- function(network_model = NULL){
  structure(list(description = "Network LME model",
                 network_model = network_model),
            class = c("network_lme", "network_model", "repel_model"))
}
