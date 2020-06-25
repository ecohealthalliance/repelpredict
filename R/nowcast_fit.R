# define generic fit
repel_fit <- function(x, ...){
  UseMethod("repel_fit")
}

#' Fit nowcast BART model object
#' @return list containing predicted count and whether disease is expected or not (T/F)
#' @import dplyr tidyr
#' @importFrom dbarts bart2
#'
repel_fit.nowcast_bart <- function(model_object,
                                   augmented_data,
                                   outcome_var,
                                   output_directory,
                                   verbose = interactive()) {

  modified_data <- modify_augmented_data(augmented_data = augmented_data,
                                         outcome_var = outcome_var)

  bart_mod <- bart2(formula = as.formula(paste(outcome_var, "~.")),
                    data = modified_data,
                    test = select(modified_data, -!!outcome_var),
                    keepTrees = TRUE,
                    verbose = verbose)

  write_rds(bart_mod, paste0(output_directory, "/bart_mod_", outcome_var, ".rds"))

  return(bart_mod)
}
