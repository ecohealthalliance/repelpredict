devtools::load_all()
library(ggplot2)

conn <- repeldata::repel_remote_conn()

network_lme_augment_predict <- get_cached_network_predictions(conn)
randef <- get_cached_network_random_effects()


# Variable importance plots -----------------------------------------------

vi <- get_network_variable_importance(network_lme_augment_predict, randef,
                                      country_iso3c = "USA", diseases = get_oie_high_importance_diseases() )

plots <- vi %>%
  filter(str_detect(variable, "from_outbreaks")) %>%
  group_split(disease) %>%
  map(., function(x){
    outbreak_prob <- signif(unique(x$predicted_outbreak_probability), 2)
    country_name <- unique(x$country)
    disease_name <- str_to_title(str_replace_all(unique(x$disease), "_", " "))
    year <- year(unique(x$month))

    ggplot(x) +
      geom_vline(aes(xintercept = 0), color = "gray50") +
      geom_point(aes(x = variable_importance, y = variable, color = pos), size = 2) +
      geom_segment(aes(x = variable_importance, xend = 0, y = variable, yend = variable, color = pos)) +
      scale_color_manual(values = c("TRUE" = "#0072B2", "FALSE" = "#D55E00")) +
      labs(y = "", x = "Variable importance", title = glue::glue("{year} {disease_name} Outbreak in {country_name} Variable Importance ({outbreak_prob} probability outbreak)")) +
      theme_minimal() +
      theme(legend.position = "none",
            axis.text = element_text(size = 10),
            title = element_text(size = 10),
            plot.title.position = "plot") +
      NULL
  })

#TODO
# 1. disagg by which countries are contributing to risk
# 2. for each country, disease status (nowcast) and import risk (travel)- see shiny

