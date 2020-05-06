library(repelpredict)
library(repeldata)
library(tidyverse)


# single input
conn <- repel_remote_conn()
nowcast_baseline_predict(conn,
                         country_iso3c = "IND",
                         year = 2018,
                         semester = 2,
                         disease = "foot-and-mouth disease",
                         taxa = "swine",
                         validate = TRUE)

# df input
validation_data <- repel_cases_validate(conn)

test <- validation_data %>%
  distinct(country_iso3c, report_year, report_semester, disease, taxa) %>%
  slice(1:200)

out <- pmap_df(test, ~nowcast_baseline_predict(conn,
                                     country_iso3c = .$country_iso3c,
                                     year = .$report_year,
                                     semester = .$report_semester,
                                     disease = .$disease,
                                     taxa = .$taxa))


tlist <- tibble()
for(i in 1:nrow(test)){
  out <- nowcast_baseline_predict(conn,
                           country_iso3c = test$country_iso3c[i],
                           year = test$report_year[i],
                           semester = test$report_semester[i],
                           disease = test$disease[i],
                           taxa = test$taxa[i],
                           validate = TRUE)
  tlist <- bind_rows(tlist, out)
}
DBI::dbDisconnect(conn)
