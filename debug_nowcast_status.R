devtools::load_all()
library(DBI)
conn <- repeldata::repel_remote_conn()

disease_status_model_new <- aws.s3::s3readRDS(bucket = "repeldb/models", object = "boost_mod_disease_status.rds")
disease_status_model_old <- read_rds(here::here("boost_mod_disease_status.rds")) # version from march 2021

input_data_new <- disease_status_model_new$pre$actions$recipe$recipe$template
input_data_old <- disease_status_model_old$pre$actions$recipe$recipe$template

###### Example 1: FMD presence but no cases
fmd_new <- input_data_new %>% filter(disease == "foot_and_mouth_disease", country_iso3c == "AFG", report_year == 2005, report_semester == 1)
# https://wahis.oie.int/#/report-smr/view?reportId=20038&period=SEM01&areaId=2&isAquatic=false
# ^ was unclear that some taxa were "present" without cases (eg buffalo)

fmd_old <- input_data_old %>% filter(disease == "foot_and_mouth_disease", country_iso3c == "AFG", report_year == 2005, report_semester == 1)
# https://prospero.ecohealthalliance.org/rstudio/files/repel-infrastructure/scraper/data-raw/wahis-raw-annual-reports/AFG_2005_sem1.html
# ^ taxa marked as present but no cases reported

###### Example 2 FMD absence
fmd_new2 <- input_data_new %>% filter(disease == "foot_and_mouth_disease", country_iso3c == "ABW", report_year == 2011, report_semester == 1)
# https://wahis.oie.int/#/report-smr/view?reportId=24124&period=SEM01&areaId=1&isAquatic=false
# marked multiple species as absent, but from control measures we can get taxa list

fmd_old2 <- input_data_old %>% filter(disease == "foot_and_mouth_disease", country_iso3c == "ABW", report_year == 2011, report_semester == 1)
# https://prospero.ecohealthalliance.org/rstudio/files/repel-infrastructure/scraper/data-raw/wahis-raw-annual-reports/ABW_2011_sem1.html
