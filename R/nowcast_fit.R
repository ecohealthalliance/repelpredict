# define generic fit
#' @export
repel_fit <- function(x, ...){
  UseMethod("repel_fit")
}

#' Fit nowcast BART model object
#' @return list containing predicted count and whether disease is expected or not (T/F)
#' @import dplyr tidyr
#' @importFrom dbarts bart2
#' @importFrom qs qsave
#' @export
repel_fit.nowcast_bart <- function(model_object,
                                   augmented_data,
                                   outcome_var,
                                   output_directory,
                                   verbose = interactive()) {

  modified_data <- modify_augmented_data(augmented_data = augmented_data,
                                         outcome_var = outcome_var)

  assertthat::assert_that(all(map_lgl(modified_data, ~any(!is.na(.)))))

  bart_mod <- dbarts::bart2(formula = as.formula(paste(outcome_var, "~.")),
                    data = modified_data,
                    test = select(modified_data, -!!outcome_var),
                    keepTrees = TRUE,
                    combineChains = TRUE,
                    verbose = verbose)
  # note: test = Explanatory variables for test (out of sample) data. Should have same column structure as x.train. bart will generate draws of f(x) for each x which is a row of x.test.

  invisible(bart_mod$fit$state)

  qsave(list(bart_mod, modified_data), paste0(output_directory, "/bart_mod_", outcome_var, ".qs"))

  return(bart_mod)
}
