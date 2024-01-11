if (!require(tidyverse)) install.packages("tidyverse")
if (!require(janitor)) install.packages("janitor")
if (!require(rstudioapi)) install.packages("rstudioapi")
if (!require(pak)) install.packages("pak")
if (!require(riddle)) pak::pkg_install("edouard-legoupil/riddle")
library(lubridate)

Sys.unsetenv("USE_UAT")
# Set ridl resource ---------------------------------------------------

# rstudioapi::showDialog(title = "Set the parameters for ridl resource!",
#                        message = "You will now enter   <b>country_name</b> and the precise URL for the csv datain RIDL  <b>raw_data_ridl</b>" )

#country_name <- rstudioapi::askForPassword(prompt = 'country_name ')
# country_name <- rstudioapi::showPrompt(  title = "country_name", message = "Enter Country name", default = "")
country_name <- "Mexico"

#raw_data_ridl <- rstudioapi::askForPassword(prompt = 'raw_data_ridl')
# raw_data_ridl <- rstudioapi::showPrompt(  title = "raw_data_ridl", message = "Enter precise URL for the csv datain RIDL", default = "")
raw_data_ridl <- 'https://ridl.unhcr.org/dataset/219eed22-36f2-4b15-8b9c-901791f84aed/resource/8f81a7e5-10d4-47d2-8a1f-131001ae1bb5/download/survey-csv-data__aemxvakijzxdkveakhx4bm_data.csv'

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
         id = id,
         uuid = uuid,
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
         D002_intention_return_ven = questionnaire_d_destination_d002_intention_return_ven,
         D004_why_return_ven = questionnaire_d_destination_d004_why_return_ven,
         D004_why_return_ven_other = questionnaire_d_destination_d004_why_return_ven_other,
         D005_return_state_ven = questionnaire_d_destination_d005_return_state_ven,
         E001_arrival_date = questionnaire_e_arrival_e001_arrival_date,
         E002_transit_country_001 = questionnaire_e_arrival_e002_transit_country_001,
         E003_arrival_how = questionnaire_e_arrival_e003_arrival_how,
         F004_asy_app = questionnaire_f_asylum_f004_asy_app,
         F004a_asyapp_outcome = questionnaire_f_asylum_f004a_asyapp_outcome,
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



# fix question labels -----------------------------------------------------

df_wrangle <- df_wrangle |> 
  mutate(journey_food = gsub("1", "local_restaurants", journey_food),
         journey_food = gsub("2", "canteens", journey_food),
         journey_food = gsub("3", "local_population", journey_food),
         journey_food = gsub("4", "local_market", journey_food),
         journey_food = gsub("5", "occasionally_eaten", journey_food),
         journey_food = gsub("6", "other", journey_food),
         ### this category doesn't exist in the regional database, so we recoded as 'other'
         NMeals = case_when(NMeals == 1 ~ "none",
                            NMeals == 2 ~ "one",
                            NMeals == 3 ~ "two",
                            NMeals == 4 ~ "three_more",
                            TRUE ~ NA_character_),
         PercFoodSec = case_when(PercFoodSec == 1 ~ "no_difficulties",
                                 PercFoodSec == 2 ~ "less_expensive",
                                 PercFoodSec == 3 ~ "skipped_meals",
                                 PercFoodSec == 4 ~ "whole_day_without_eat",
                                 TRUE ~ NA_character_),
         Acceswater = gsub("1", "market", Acceswater),
         Acceswater = gsub("2", "local_communities", Acceswater),
         Acceswater = gsub("3", "humanitarian_organizations", Acceswater),
         Acceswater = gsub("4", "rivers_lakes_rainwater", Acceswater),
         Acceswater = gsub("5", "other", Acceswater), 
         
         WorryRsnFirst = gsub("1", "spread_illness", WorryRsnFirst),
         WorryRsnFirst = gsub("2", "cover_food", WorryRsnFirst),
         WorryRsnFirst = gsub("3", "cover_essential_needs", WorryRsnFirst),
         WorryRsnFirst = gsub("4", "dependants_care", WorryRsnFirst),
         WorryRsnFirst = gsub("5", "limitations_of_movement", WorryRsnFirst),
         WorryRsnFirst = gsub("6", "deportation", WorryRsnFirst),
         # WorryRsnFirst = gsub("7", "physical_security", WorryRsnFirst),   ### this category doesn't exist in the regional database
         # WorryRsnFirst = gsub("8", "corpses_observation", WorryRsnFirst),   ### this category doesn't exist in the regional database
         WorryRsnFirst = gsub("99", "didnt_have_worry", WorryRsnFirst),   ### this category doesn't exist in the regional database
         # WorryRsnFirst = gsub("9", "fear_of_getting_lost", WorryRsnFirst),   ### this category doesn't exist in the regional database
         
         Mainneeds = gsub("10", "other", Mainneeds),
         # Mainneeds = gsub("11", "clothes_shoes", Mainneeds),   ### this category doesn't exist in the regional database
         # Mainneeds = gsub("12", "none", Mainneeds),   ### this category doesn't exist in the regional database
         Mainneeds = gsub("1", "food_family", Mainneeds),
         Mainneeds = gsub("2", "food_children", Mainneeds),
         Mainneeds = gsub("3", "cash", Mainneeds),
         Mainneeds = gsub("4", "drinking_water", Mainneeds),
         Mainneeds = gsub("5", "health_care", Mainneeds),
         Mainneeds = gsub("6", "child_care", Mainneeds),
         Mainneeds = gsub("7", "shelter", Mainneeds),
         Mainneeds = gsub("8", "internet_telephone", Mainneeds),
         Mainneeds = gsub("9", "legal", Mainneeds),
         
  )


# keep consent equal yes --------------------------------------------------

df_wrangle <- df_wrangle |> 
  filter(A006_consent == "yes")


# remove line break -------------------------------------------------------

df_wrangle <- df_wrangle |> 
  mutate(across(
    where(is.character),
    str_trim
  ))

# fix other nationality ---------------------------------------------------

df_wrangle <- df_wrangle |> 
  mutate(B004_nationality = str_replace(B004_nationality,"Other", B004_nationality_other))


# fix nationality ---------------------------------------------------------

df_wrangle <- df_wrangle |> 
  mutate(B004_nationality = case_when(B004_nationality == "Brazil" ~ "Brasil",
                                      B004_nationality == "Burkina faso" ~ "Burkina Faso",
                                      B004_nationality == "Camerun" ~ "Cameroon",
                                      B004_nationality == "elSalvador" ~ "El Salvador",
                                      B004_nationality == "PanamÃ¡" ~ "Panama",
                                      B004_nationality == "PerÃº" ~ "Peru",
                                      B004_nationality == "RepublicaDominicana" ~ "Republica Dominicana",
                                      B004_nationality == "stateless" ~ "Stateless",
                                      TRUE ~ B004_nationality),
         B004_nationality_other = case_when(!str_detect(B004_nationality, "other") ~ NA_character_,
                                            TRUE ~ B004_nationality_other))


# create dual nationality ---------------------------------------------

df_wrangle <- df_wrangle |> 
  mutate(dual_nationality = case_when(str_detect(B004_nationality, ";") ~ 1L,
                                      TRUE ~ 0L))


# Country of destination --------------------------------------------------

df_wrangle <- df_wrangle |> 
  mutate(D001a_destination_country = case_match(D001a_destination_country,
                                                "elSalvador" ~ "El Salvador",
                                                .default = D001a_destination_country
  ))

# COR ---------------------------------------------------------------------

df_wrangle <- df_wrangle |> 
  mutate(B011_habitual_residence = case_match(B011_habitual_residence,
                                              "elSalvador" ~ "El Salvador",
                                              "other" ~ "Other",
                                              "CostaRica" ~ "Costa Rica",
                                              "RepublicaDominicana" ~ "Republica Dominicana",
                                              "Brazil" ~ "Brasil",
                                              "TrinidadYTobago" ~ "Trinidad and Tobago",
                                              .default = B011_habitual_residence
  ))

# Transit -----------------------------------------------------------------
df_wrangle <- df_wrangle |> 
  mutate(E002_transit_country_001 = str_replace_all(E002_transit_country_001, c(" " = ";")))

df_wrangle <- df_wrangle |> 
  mutate(E002_transit_country_001 = str_replace_all(E002_transit_country_001, c("elSalvador" = "El Salvador",
                                                                                "TrinidadYTobago" = "Trinidad and Tobago",
                                                                                "CostaRica" = "Costa Rica",
                                                                                "RepublicaDominicana" = "Republica Dominicana",
                                                                                "Brazil" = "Brasil")))

# remove unnecessary rows -------------------------------------------------

df_wrangle <- df_wrangle |>
  filter(!uuid %in% c("",
                      "",
                      "",
                      "",
                      "",
                      "",
                      ""))

# fix rows when CoO and CoR are equal -------------------------------------

df_wrangle <- df_wrangle |> 
  mutate(C001_when_left_cor = case_when(B004_nationality != B011_habitual_residence ~ C001_when_left_coo,
                                        TRUE ~ NA_character_),
         C001_when_left_coo = case_when(B004_nationality == B011_habitual_residence ~ C001_when_left_coo,
                                        TRUE ~ NA_character_),
         C004_reasons_to_leave_origin = case_when(B004_nationality == B011_habitual_residence ~ C004_reasons_to_leave_habitual_residence,
                                                  TRUE ~ C004_reasons_to_leave_origin),
         C004_reasons_to_leave_habitual_residence = case_when(B004_nationality == B011_habitual_residence ~ NA_character_,
                                                              TRUE ~ C004_reasons_to_leave_habitual_residence))


# add country -------------------------------------------------------------

df_wrangle <- df_wrangle |> 
  mutate(country = country_name)

# select dates -------------------------------------------------------------

df_wrangle <- df_wrangle |> 
  filter(A003_date >= '2023-01-01')


# write locally the clean data --------------------------------------------
file_name <- paste0(tolower(country_name), '_mm_questionnaire_clean_', tolower(month.name[month(max(df_wrangle$today))]), "_",year(max(df_wrangle$today)),".csv")

write_csv(df_wrangle, paste0('./data-wrangle/', file_name))

# write clean data to ridl ------------------------------------------------


dataset_id <- gsub("(.*dataset/)(.*)(/resource.*)", "\\2",raw_data_ridl)


m <- resource_metadata(
  type = "data",
  url = file_name,
  name = paste0(country_name, ": Mixed movement - Cleaned"),
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

