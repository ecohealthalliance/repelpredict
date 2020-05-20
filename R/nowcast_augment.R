#' define generic augment
repel_augment <- function(x, ...){
  UseMethod("repel_augment")
}


#' Augment nowcast baseline model object
#' @import repeldata dplyr tidyr
#' @importFrom assertthat has_name assert_that
#'
repel_augment.nowcast_baseline <- function(model_object, conn, newdata) {

  get_nowcast_lag(newdata)

}
