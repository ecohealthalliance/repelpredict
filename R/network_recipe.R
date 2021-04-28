# because we're not using the recipe package for the network model, here is a function for preprocessing augmented data for fitting or predict

#'@noRd
network_recipe <- function(augmented_data,
                           predictor_vars,
                           scaling_values) {

  predictor_vars <- predictor_vars[!predictor_vars %in% "continent"] # remove continent (factor) for scaling purposes

  assert_that(all(sort(scaling_values$key) == sort(predictor_vars)))

  prescale_augmented_data <- augmented_data %>%
    select(country_iso3c,
           suppressWarnings(one_of("continent")),
           disease,
           suppressWarnings(one_of("outbreak_start")), # needed for model fitting but not prediction
           !!predictor_vars) %>%
    mutate(country_iso3c = as.factor(country_iso3c)) %>%
    mutate(disease = as.factor(disease))

  scaled_augmented_data <- prescale_augmented_data %>%
    mutate(unique_id = row_number()) %>% # in case there are duplicates (e.g., in bootstrap validation)
    pivot_longer(cols = all_of(predictor_vars)) %>%
    left_join(scaling_values, by = c("name" = "key")) %>%
    mutate(value = (value - `mean`) / `sd`) %>%
    select(-mean, -sd) %>%
    pivot_wider(names_from = "name", values_from = "value") %>%
    select(-unique_id)

  return(scaled_augmented_data)

}
