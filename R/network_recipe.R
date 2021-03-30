# because we're not using the recipe package for the network model, here is a function for preprocessing augmented data for fitting or predict

#'@noRd
network_recipe <- function(augmented_data,
                           predictor_vars) {

  augmented_data %>%
    select(country_iso3c, disease, month, outbreak_start,
           !!predictor_vars) %>%
    drop_na() %>%
    mutate(country_iso3c = as.factor(country_iso3c)) %>%
    mutate(disease = as.factor(disease)) %>%
    mutate_if(is.numeric, scale2)

}
