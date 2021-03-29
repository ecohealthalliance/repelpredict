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

#' Adds more NA handling functionality to imputeTS::na_interpolation
#' @import dplyr tidyr
#' @importFrom imputeTS na_interpolation
#' @noRd
na_interp <- function(df, var){
  if(sum(!is.na(df[,var])) == 0){
    out <- mutate(df, !!paste0(var, "_imputed") := NA_integer_)
  }
  if(sum(!is.na(df[,var])) == 1){
    out <- mutate(df,  !!paste0(var, "_imputed") := get(var)[!is.na(get(var))])
  }
  if(sum(!is.na(df[,var])) > 1){
    out <- mutate(df,  !!paste0(var, "_imputed") := imputeTS::na_interpolation(get(var)))
  }
  return(out)
}

#' @noRd
scale2 <- function(x, na.rm = FALSE) (x - mean(x, na.rm = na.rm)) / sd(x, na.rm)
