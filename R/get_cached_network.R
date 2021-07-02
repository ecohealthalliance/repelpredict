#' Get cached network model augmented data and model predictions
#' @param conn repeldata connection
#' @return dataframe containing full dataset and predictions
#' @import dplyr tidyr repeldata DBI stringr
#' @importFrom countrycode countrycode
#' @export
get_cached_network_predictions <- function(conn){

  network_lme_augment_predict <- dbReadTable(conn, name = "network_lme_augment_predict") %>%
    select(-db_network_etag) %>%
    rename(outbreak_ongoing = outbreak_subsequent_month)

  country_lookup <- tibble(country_iso3c = unique(network_lme_augment_predict$country_iso3c)) %>%
    mutate(country = countrycode::countrycode(country_iso3c, origin = "iso3c", destination = "country.name"))

  disease_lookup <- tibble(disease = unique(network_lme_augment_predict$disease)) %>%
    mutate(disease_clean = str_to_title(str_replace_all(disease, "_", " ")))

  network_lme_augment_predict <- left_join(network_lme_augment_predict, country_lookup) %>%
    left_join(disease_lookup) %>%
    as_tibble()

  return(network_lme_augment_predict)
}

#' Get cached model random effect coefficients
#' @return dataframe with coefficient by disease and variable
#' @import dplyr tidyr lme4
#' @importFrom aws.s3 s3readRDS
#' @export
get_cached_network_random_effects <- function(){

  model_object <-  network_lme_model(
    network_model = aws.s3::s3readRDS(bucket = "repeldb/models", object = "lme_mod_network.rds"),
    network_scaling_values = aws.s3::s3readRDS(bucket = "repeldb/models", object = "network_scaling_values.rds")
  )

  lme_mod <- model_object$network_model

  randef <- ranef(lme_mod)
  randef_disease <- randef$disease %>%
    tibble::rownames_to_column(var = "disease") %>%
    as_tibble()%>%
    pivot_longer(-disease, names_to = "variable", values_to = "coef")

  return(randef_disease)
}

