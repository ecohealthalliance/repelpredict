#' @noRd
na_empty <- function(x){
  na_var <- switch(class(x), "numeric" = NA_integer_, "character" = NA_character_)
  if(!length(x))  x <- na_var
  return(x)
}

#'@noRd
sum_na <- function(x){
  ifelse(all(is.na(x)), NA_integer_, sum(as.numeric(x), na.rm = TRUE))
}
