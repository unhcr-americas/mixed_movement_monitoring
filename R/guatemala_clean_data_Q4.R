if (!require(tidyverse)) install.packages("tidyverse")
if (!require(janitor)) install.packages("janitor")
if (!require(rstudioapi)) install.packages("rstudioapi")
if (!require(pak)) install.packages("pak")
if (!require(riddle)) pak::pkg_install("edouard-legoupil/riddle")


Sys.unsetenv("USE_UAT")
# Set ridl resource ---------------------------------------------------

country_name <- "Guatemala"
raw_data_ridl <- 'https://ridl.unhcr.org/dataset/587768a4-503d-44e3-9e7f-d47705c8bb74/resource/6bb90a14-096f-4c6d-88e7-cf92e16a336f/download/survey-xlsx-data__aeayoj8x7kjb6qokomjyup_data.xlsx'   

# read data ---------------------------------------------------------------

df_raw <- resource_fetch(raw_data_ridl)|>
  readxl::read_xlsx()|>
  clean_names()

# bring family_doc data ---------------------------------------------------

df_fam_doc <- resource_fetch(raw_data_ridl)|>
  readxl::read_xlsx(sheet = "family_status_countries")|>
  clean_names() |> 
  select(questionnaire_family_status_countries_label_country,
         questionnaire_family_status_countries_b09_fam_status,
         submission_id, submission_uuid) |> 
  distinct() |> 
  summarise(questionnaire_family_status_countries_label_country = paste(questionnaire_family_status_countries_label_country, collapse = ";"),
            questionnaire_family_status_countries_b09_fam_status = paste(questionnaire_family_status_countries_b09_fam_status, collapse = ";"), 
            .by= c(submission_id, submission_uuid)) 


df_wrangle <- df_raw |> 
  left_join(df_fam_doc,
            by = c('id' = 'submission_id',
                   'uuid' = 'submission_uuid' )) 



# select core questions ---------------------------------------------------

df_wrangle <- df_wrangle |> 
  mutate(a_introduction_a03_date = ymd(a_introduction_a03_date)) |> 
  select(a_introduction_a04_country = a_introduction_label_co_i,
         start,                                                                                                           
         end,                                                                                                             
         today,
         id = id,
         uuid = uuid,
         gps_location = a06_gps_location,                                                                                                 
         gps_latitude = a_introduction_a06_gps_location_latitude,                                                                            
         gps_longitude = a_introduction_a06_gps_location_longitude,                                                                           
         gps_altitude = a_introduction_a06_gps_location_altitude,                                                                            
         gps_precision = a_introduction_a06_gps_location_precision,
         A001_Organization_eng = a_introduction_a01_organization_eng,
         A001_Organization_esp = a_introduction_a01_organization_esp,
         A002_staff = a_introduction_a02_staff,
         A003_date = a_introduction_a03_date,
         A004_location = a_introduction_a05_location,
         consent_text = a_introduction_consent_text,
         A006_consent = a_introduction_a07_consent,
         B001_sex = questionnaire_b_demographics_b01_sex,
         B002_age = questionnaire_b_demographics_b02_age,
         B004_nationality = questionnaire_b_demographics_b03_nationality,
        #  B004_nationality_other = questionnaire_b_biodata_b004_nationality_other_3,
         B004_nationality_stateless = questionnaire_b_demographics_b003_nationality_stateless,
         B004_travel_with = questionnaire_b_demographics_b04_travel_with,
         B005_family_adults = questionnaire_b_demographics_b05_fam_adults,
         B006_family_children = questionnaire_b_demographics_b06_fam_children,
         B007_family_children_5less = questionnaire_b_demographics_b07_fam_children_5less,
         # B008_family_country = questionnaire_b_demographics_b08_fam_country,
         B008_family_country = questionnaire_family_status_countries_label_country,
         B009_family_status = questionnaire_family_status_countries_b09_fam_status,
         Country_origin = questionnaire_c_co_o_label_co_o,
         C001_when_left_coo = questionnaire_c_co_o_c01_co_o_left_when,
         C004_reasons_to_leave_origin = questionnaire_c_co_o_c02_co_o_left_reasons,
         C004_reasons_to_leave_origin_other = questionnaire_c_co_o_c02_co_o_left_reasons_other_2,
         habitual_residence_yn = questionnaire_c03_habitual_residence,
         B011_habitual_residence = questionnaire_d_co_r_d01_co_r,
         B011_habitual_residence_other = questionnaire_d_co_r_d01_co_r_other,
         C001_when_left_cor = questionnaire_d_co_r_d02_co_r_left_when,
         C004_reasons_to_leave_habitual_residence = questionnaire_d_co_r_d03_co_r_left_reason,
         C004_reasons_to_leave_habitual_residence_other = questionnaire_d_co_r_d03_co_r_left_reason_other_2,
         B010_documentation_residence = questionnaire_d_co_r_d05_co_r_document,
         D006_documentation_residence_possession = questionnaire_d_co_r_d06_co_r_document_poss,
         D007_documentation_residence_valid = questionnaire_d_co_r_d07_co_r_document_valid, 
         E002_transit_country_001 = questionnaire_e_journey_co_d_e01_countries_crossed,
         I002_protinc_journey_type = questionnaire_e_journey_co_d_e02_incidents,
         I002_protinc_journey_type_other =questionnaire_e_journey_co_d_e02_incidents_other_2,
         D001a_destination_country = questionnaire_e_journey_co_d_e03_destination_country,
         D001a_destination_country_reason = questionnaire_e_journey_co_d_e04_destination_reasons,
         D001a_destination_country_reason_other = questionnaire_e_journey_co_d_e04_destination_reasons_other_2,
         D005_destination_notreach = questionnaire_e_journey_co_d_e05_destination_notreach,
         E006_reasons_not_return = questionnaire_e_journey_co_d_e04_reasons_not_return,
         E006_reasons_not_return_other = questionnaire_e_journey_co_d_e04_reasons_not_return_other_2,
         E006_reasons_return = questionnaire_e_journey_co_d_e06_reasons_return,
         E006_reasons_return_other = questionnaire_e_journey_co_d_e06_reasons_return_other_2,
         E007_risk_return = questionnaire_e_journey_co_d_e07_risk_return,
         E001_arrival_date = questionnaire_f_now_arrival_f01_arrival_date,
         F02_stay_howlong = questionnaire_f_now_arrival_f02_stay_howlong,
         NMeals = questionnaire_f_now_arrival_f03_nmeals,
         PercFoodSec = questionnaire_f_now_arrival_f04_food_sec,
         B010_documentation = questionnaire_f_now_arrival_f05_documents,
         B010_documentation_other = questionnaire_f_now_arrival_f05_documents_other_2,
         Mainneeds = questionnaire_f_now_arrival_f06_mainneeds,
         Mainneeds_other = questionnaire_f_now_arrival_f06_mainneeds_other_2,
         L001_Spec_needs = questionnaire_g_needs_g01_spec_needs,
         Comment = questionnaire_g_needs_comments_end)


# keep consent equal yes --------------------------------------------------

df_wrangle <- df_wrangle |> 
  filter(A006_consent == "yes")

# keep only Q3 data -------------------------------------------------------

df_wrangle <- df_wrangle |> 
  filter(A003_date >= ymd("2023-10-01") & A003_date <= ymd("2023-12-31"))
    
# remove line break -------------------------------------------------------

df_wrangle <- df_wrangle |> 
  mutate(across(
    where(is.character),
    str_trim
  ))

# fix other nationality ---------------------------------------------------

df_wrangle <- df_wrangle |> 
  mutate(B004_nationality = countrycode::countrycode(B004_nationality, origin = "iso3c", destination = "country.name"))


# Country of destination --------------------------------------------------

df_wrangle <- df_wrangle |> 
   mutate(D001a_destination_country = countrycode::countrycode(D001a_destination_country, 
                                                               origin = "iso3c",
                                                               destination = "country.name"))

# COR ---------------------------------------------------------------------

df_wrangle <- df_wrangle |> 
  mutate(B011_habitual_residence = countrycode::countrycode(B011_habitual_residence,
                                                            origin = "iso3c",
                                                            destination = "country.name")) 

# Transit -----------------------------------------------------------------

df_wrangle <- df_wrangle |> 
  mutate(E002_transit_country_001 = str_replace_all(E002_transit_country_001, c(" " = ";"))) 

# remove possible duplication in uuid -------------------------------------

# df_wrangle |> 
#   group_by(uuid) |> 
#   filter(n() >= 2) |> 
#   writexl::write_xlsx("teste.xlsx")
#   distinct(uuid) |> 
#   pull(uuid)

# remove unnecessary rows -------------------------------------------------

df_wrangle <- df_wrangle |>
  filter(!uuid %in% c("",
                      "",
                      "",
                      "",
                      "",
                      "",
                      ""))

# write locally the clean data --------------------------------------------
file_name <- paste0(tolower(country_name), '_mm_questionnaire_q4_2023_clean.csv')

write_csv(df_wrangle, paste0('./data-wrangle/', file_name))

# write clean data to ridl ------------------------------------------------


dataset_id <- gsub("(.*dataset/)(.*)(/resource.*)", "\\2",raw_data_ridl)


m <- resource_metadata(
  type = "data",
  url = file_name,
  name = paste0(country_name, ": Mixed movement Q4 2023 - Cleaned"),
  upload = httr::upload_file(paste0('data-wrangle/', file_name)),
  format = "csv",
  file_type = "microdata",
  date_range_start = paste0(min(df_wrangle$today)),
  date_range_end = paste0(max(df_wrangle$today)),
  version = "1",
  visibility = "public",
  process_status = "cleaned",
  identifiability = "anonymized_enclave"
  
)


r <- resource_upload(dataset_id,
                     m)

