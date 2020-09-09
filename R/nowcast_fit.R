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
                                   output_directory,
                                   n_chains = dbarts::guessNumCores(),
                                   verbose = interactive()) {

  # set threads for computing
  existing_blas <- RhpcBLASctl::blas_get_num_procs()
  RhpcBLASctl::blas_set_num_threads(1)
  on.exit(RhpcBLASctl::blas_set_num_threads(existing_blas))

  # Status model ------------------------------------------------------------
  # modify data prior to running
  modified_disease_status_data <- augmented_data %>%
    drop_na(disease_status) %>%
    select(-cases, -report_year)

  # confirm no NAs or Inf
  assertthat::assert_that(all(map_lgl(modified_disease_status_data, ~all(!is.na(.)))))
  assertthat::assert_that(all(map_lgl(modified_disease_status_data, ~all(!is.infinite(.)))))
  assertthat::assert_that(all(map_lgl(modified_disease_status_data, ~all(!is.nan(.)))))
  assertthat::assert_that(all(map_lgl(modified_disease_status_data, ~all(!is.null(.)))))

  # run model
  message("Running disease status (binary) model")
  bart_mod_disease_status <- dbarts::bart2(formula = as.formula(paste("disease_status", "~.")),
                                           data = modified_disease_status_data,
                                           test = select(modified_disease_status_data, -disease_status),
                                           n.chains = n_chains,
                                           keepTrees = TRUE,
                                           combineChains = TRUE,
                                           verbose = verbose)
  ## note: test = Explanatory variables for test (out of sample) data. Should have same column structure as x.train. bart will generate draws of f(x) for each x which is a row of x.test.

  # save model
  ## note: "To do this, one must “touch” the sampler's state object before saving"
  invisible(bart_mod_disease_status$fit$state)
  qsave(list(bart_mod_disease_status, modified_disease_status_data), paste0(output_directory, "/bart_mod_", "disease_status", ".qs"))

  # Case model ------------------------------------------------------------
  # modify data prior to running (only running on positive counts)
  modified_cases_data <- augmented_data %>%
    drop_na(cases) %>%
    filter(cases > 0) %>%
    select(-disease_status, -report_year)

  # confirm no NAs or Inf
  assertthat::assert_that(all(map_lgl(modified_cases_data, ~all(!is.na(.)))))
  assertthat::assert_that(all(map_lgl(modified_cases_data, ~all(!is.infinite(.)))))
  assertthat::assert_that(all(map_lgl(modified_cases_data, ~all(!is.nan(.)))))
  assertthat::assert_that(all(map_lgl(modified_cases_data, ~all(!is.null(.)))))

  # run model
  message("Running cases (count) model")
  bart_mod_cases <- dbarts::bart2(formula = as.formula(paste("cases", "~.")),
                                           data = modified_cases_data,
                                           test = select(modified_cases_data, -cases),
                                           n.chains = n_chains,
                                           keepTrees = TRUE,
                                           combineChains = TRUE,
                                           verbose = verbose)
  ## note: test = Explanatory variables for test (out of sample) data. Should have same column structure as x.train. bart will generate draws of f(x) for each x which is a row of x.test.

  # save model
  ## note: "To do this, one must “touch” the sampler's state object before saving"
  invisible(bart_mod_cases$fit$state)
  qsave(list(bart_mod_cases, modified_cases_data), paste0(output_directory, "/bart_mod_", "cases", ".qs"))

  return(NULL)
}
