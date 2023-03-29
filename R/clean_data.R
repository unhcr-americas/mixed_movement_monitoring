if (!require(tidyverse)) install.packages("tidyverse")
if (!require(janitor)) install.packages("janitor")
if (!require(rstudioapi)) install.packages("rstudioapi")
if (!require(pak)) install.packages("pak")
if (!require(riddle)) pak::pkg_install("edouard-legoupil/riddle")


Sys.unsetenv("USE_UAT")
# Set ridl resource ---------------------------------------------------

rstudioapi::showDialog(title = "Set the parameters for ridl resource!",
                       message = "You will now enter   <b>country_name</b> and the precise URL for the csv datain RIDL  <b>raw_data_ridl</b>" )

#country_name <- rstudioapi::askForPassword(prompt = 'country_name ')
country_name <- rstudioapi::showPrompt(  title = "country_name", message = "Enter Country name", default = "")
# country_name <- "Country"

#raw_data_ridl <- rstudioapi::askForPassword(prompt = 'raw_data_ridl')
raw_data_ridl <- rstudioapi::showPrompt(  title = "raw_data_ridl", message = "Enter precise URL for the csv datain RIDL", default = "")
#raw_data_ridl <- 'URL to your CSV file in RIDL'   
 

# read data ---------------------------------------------------------------

df_raw <- resource_fetch(raw_data_ridl) |> 
  read_csv2() |> 
  clean_names()


# select core questions ---------------------------------------------------

df_wrangle <- df_raw |> 
  mutate(a_introduction_a003_date = ymd(a_introduction_a003_date)) |> 
  select(start,                                                                                                           
         end,                                                                                                             
         today,
         uuid = uuid,
         gps_ubicacion = ubicacion_gps,                                                                                                 
         gps_latitude = a_introduction_ubicacion_gps_latitude,                                                                            
         gps_longitude = a_introduction_ubicacion_gps_longitude,                                                                           
         gps_altitude = a_introduction_ubicacion_gps_altitude,                                                                            
         gps_precision = a_introduction_ubicacion_gps_precision,  
         A001_Organization_eng = a_introduction_a001_organization_eng,
         A001_Organization_esp = a_introduction_a001_organization_esp,
         A002_staff = a_introduction_a002_staff,
         A003_date = a_introduction_a003_date,
         A004_location = a_introduction_a004_location,
         consent_text = a_introduction_consent_text,
         A006_consent = a_introduction_a006_consent,
         B001_sex = questionnaire_b_biodata_b001_sex,
         B002_age = questionnaire_b_biodata_b002_age,
         B004_nationality = questionnaire_b_biodata_b004_nationality,
         B004_nationality_other = questionnaire_b_biodata_b004_nationality_other,
         B004_nationality_stateless = questionnaire_b_biodata_b004_nationality_stateless,
         B008_education = questionnaire_b_biodata_b008_education,
         B010_documentation = questionnaire_b_biodata_b010_documentation,
         B011_habitual_residence = questionnaire_b_biodata_b011_habitual_residence,
         intention = questionnaire_b_biodata_intention,
         B012_appliedprot = questionnaire_b_biodata_b012_appliedprot,
         C004_reasons_to_leave_origin = questionnaire_reasoncoo_c004_reasons_to_leave_origin,
         C001_when_left_coo = questionnaire_c_leaving_c001_when_left_coo,
         C004_reasons_to_leave_habitual_residence = questionnaire_c_leaving_c004_reasons_to_leave_habitual_residence,
         D001a_destination_country = questionnaire_d_destination_d001a_destination_country,
         D002_intention_return_ven = questionnaire_d_destination_d004_why_return_ven,
         D004_why_return_ven = questionnaire_d_destination_d004_why_return_ven,
         D004_why_return_ven_other = questionnaire_d_destination_d004_why_return_ven_other,
         D005_return_state_ven = questionnaire_d_destination_d005_return_state_ven,
         E001_arrival_date = questionnaire_e_arrival_e001_arrival_date,
         E002_transit_country_001 = questionnaire_e_arrival_e002_transit_country_001,
         E003_arrival_how = questionnaire_e_arrival_e003_arrival_how,
         I001_prot_inc_yn = questionnaire_i_protection_i001_prot_inc_yn,
         I002_protinc_type = questionnaire_i_protection_group_mistreat_country_i002_protinc_type,
         IB001_prot_inc_yn = questionnaire_i_protection_ib001_prot_inc_yn,
         IB002_protinc_type = questionnaire_i_protection_group_mistreat_other_country_ib002_protinc_type,
         journey_food = questionnaire_begin_needs_group_wfp_journey_food,
         NMeals = questionnaire_begin_needs_group_wfp_n_meals,
         PercFoodSec = questionnaire_begin_needs_group_wfp_perc_food_sec,
         Acceswater = questionnaire_begin_needs_group_wfp_acceswater,
         WorryRsnFirst = questionnaire_begin_needs_worry_rsn_first,
         Mainneeds = questionnaire_begin_needs_mainneeds,
         L001_Spec_needs = questionnaire_begin_needs_l001_spec_needs
         )
  

# keep consent equal yes --------------------------------------------------

df_wrangle <- df_wrangle |> 
  filter(A006_consent == "yes")


# remove unnecessary rows -------------------------------------------------

df_wrangle <- df_wrangle |>
  filter(!uuid %in% c("",
                      "",
                      "",
                      "",
                      "",
                      "",
                      ""))


# add country -------------------------------------------------------------

df_wrangle <- df_wrangle |> 
  mutate(country = country_name)


# write locally the clean data --------------------------------------------
file_name <- paste0(tolower(country_name), '_mm_questionnaire_clean_', tolower(month.name[month(max(df_wrangle$today))]), "_",year(max(df_wrangle$today)),".csv")

write_csv(df_wrangle, paste0('./data-wrangle/', file_name))

# write clean data to ridl ------------------------------------------------


dataset_id <- gsub("(.*dataset/)(.*)(/resource.*)", "\\2",raw_data_ridl)


m <- resource_metadata(
  type = "data",
  url = file_name,
  name = paste0(country_name, ": Mixed movement - Cleaned", " - ",month.name[month(max(df_wrangle$today))], " ",year(max(df_wrangle$today))),
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

