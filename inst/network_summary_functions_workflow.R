devtools::load_all()
library(ggplot2)
library(ggiraph)

breaks_by <- function(k) {
  step <- k
  function(y) seq(floor(min(y)), ceiling(max(y)), by = step)
}

# Get data ----------------------------------------------------------------
conn <- repeldata::repel_remote_conn()

network_lme_augment_predict <- get_cached_network_predictions(conn)
disagg_network_predict <- get_cached_disagg_network_predictions(conn)
vroom::vroom_write(disagg_network_predict, gzfile("tmp/disagg_network_predict.csv.gz"))
randef <- get_cached_network_random_effects()

nowcast_boost_augment_predict <- get_cached_nowcast_predictions(conn)

# Disease status summary --------------------------------------------------
disease_status <- get_disease_status_predict(nowcast_boost_augment_predict,
                                             network_lme_augment_predict,
                                             country_iso3c = "USA",
                                             diseases = get_oie_high_importance_diseases())

cases_plots <- disease_status %>%
  group_split(disease) %>%
  map(., function(cases_df){
    ggplot(cases_df, aes(x = yr, y = cases_coalesced, fill = status_coalesced, color = status_coalesced)) +
      geom_point_interactive(mapping = aes(tooltip = label, fill = status_coalesced, color = status_coalesced), pch = 21, size = 4) +
      scale_color_manual(values = c("reported present" = "#E31A1C", "unreported, predicted present" = "#FB9A99",
                                    "reported absent" = "#1F78B4", "unreported, predicted absent" = "#A6CEE3")) +
      scale_fill_manual(values = c("reported present" = "#E31A1C", "unreported, predicted present" = "#FB9A99",
                                   "reported absent" = "#1F78B4", "unreported, predicted absent" = "#A6CEE3")) +
      labs(title = paste(unique(cases_df$country), unique(cases_df$disease_clean), sep = ": "),
           y = "Cases", fill = "", color = "") +
      theme_minimal() +
      theme(axis.text.x = element_blank(), axis.title.x = element_blank())
  })


presence_plots <-  disease_status %>%
  rename(Predicted = predicted_status, Reported = actual_status) %>%
  pivot_longer(cols = c("Predicted", "Reported"), names_to = "type") %>%
  mutate(missing = factor(if_else(value, "Present", "Absent", "Missing"), levels = c("Missing", "Present", "Absent"))) %>%
  select(-value) %>%
  group_split(disease) %>%
  map(., function(presence_df){
    ggplot(presence_df, aes(x = yr, y = type, fill = missing)) +
      geom_tile(color = "white") +
      scale_x_continuous(breaks = breaks_by(1)) +
      scale_fill_manual(values = c(Present = "#E31A1C", Absent = "#1F78B4", Missing = "#FFFFFF")) +
      labs(fill = "", color = "") +
      theme_minimal() +
      theme(axis.title.x = element_blank(), panel.grid = element_blank(), axis.title.y = element_blank(), axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1))
  })

# absent/endemic/outbreak/import (instead of presence_plots?)
import_prob_plots <-  disease_status %>%
  group_split(disease) %>%
  map(., function(presence_df){
    ggplot(presence_df, aes(x = yr, y = 1, fill = predicted_semester_outbreak_probability)) +
      geom_tile(color = "white") +
      scale_x_continuous(breaks = breaks_by(1)) +
      viridis::scale_fill_viridis() +
      labs(fill = "", color = "") +
      theme_minimal() +
      theme(axis.title.x = element_blank(),
            panel.grid = element_blank(),
            axis.title.y = element_blank(),
            axis.text.y = element_blank(),
            axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1))
  })

# Variable importance by disease-----------------------------------------------
plots <- randef %>%
  filter(str_detect(variable, "from_outbreaks")) %>%
  filter(disease %in% get_oie_high_importance_diseases()) %>%
  mutate(pos = coef > 0) %>%
  group_split(disease) %>%
  map(., function(x){
    title <- unique(x$disease_clean)
    x %>%
      mutate(variable_clean = forcats::fct_reorder(variable_clean, coef)) %>%
      ggplot() +
      geom_hline(aes(yintercept = 0), color = "gray50") +
      geom_point(aes(x = variable_clean, y = coef, color = pos), size = 2) +
      geom_segment(aes(x = variable_clean, xend = variable_clean, y = coef, yend = 0, color = pos)) +
      #    scale_y_continuous(limits = c(xmin,  xmax)) +
      scale_color_manual(values = c("TRUE" = "#0072B2", "FALSE" = "#D55E00")) +
      labs(y = "Coefficient", x = "", title = title) +
      coord_flip() +
      theme_minimal() +
      theme(legend.position = "none",
            axis.text = element_text(size = 12),
            title = element_text(size = 12),
            plot.title.position = "plot") +
      NULL

  })


# Variable importance by disease and country-----------------------------------------------
vi <- get_network_variable_importance(network_lme_augment_predict, randef,
                                      country_iso3c = "USA",
                                      diseases = get_oie_high_importance_diseases())

plots <- vi %>%
  filter(str_detect(variable, "from_outbreaks")) %>%
  group_split(disease) %>%
  map(., function(x){
    outbreak_prob <- signif(unique(x$predicted_outbreak_probability), 2)
    country_name <- unique(x$country)
    disease_name <- unique(x$disease_clean)
    year <- year(unique(x$month))

    ggplot(x) +
      geom_vline(aes(xintercept = 0), color = "gray50") +
      geom_point(aes(x = variable_importance, y = variable_clean, color = pos), size = 2) +
      geom_segment(aes(x = variable_importance, xend = 0, y = variable_clean, yend = variable_clean, color = pos)) +
      scale_color_manual(values = c("TRUE" = "#0072B2", "FALSE" = "#D55E00")) +
      labs(y = "", x = "Variable importance", title = glue::glue("{year} {disease_name} Outbreak in {country_name} Variable Importance ({outbreak_prob} probability outbreak)")) +
      theme_minimal() +
      theme(legend.position = "none",
            axis.text = element_text(size = 10),
            title = element_text(size = 10),
            plot.title.position = "plot") +
      NULL
  })

#TODO Dissagg above results by country
