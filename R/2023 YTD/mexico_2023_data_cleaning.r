if (!require(tidyverse)) install.packages("tidyverse")
if (!require(janitor)) install.packages("janitor")
if (!require(rstudioapi)) install.packages("rstudioapi")
if (!require(pak)) install.packages("pak")
if (!require(riddle)) pak::pkg_install("edouard-legoupil/riddle")
library(lubridate)

Sys.unsetenv("USE_UAT")
# Set ridl resource ---------------------------------------------------

# rstudioapi::showDialog(title = "Set the parameters for ridl resource!",
                       # message = "You will now enter   <b>country_name</b> and the precise URL for the csv datain RIDL  <b>raw_data_ridl</b>" )

#country_name <- rstudioapi::askForPassword(prompt = 'country_name ')
# country_name <- rstudioapi::showPrompt(  title = "country_name", message = "Enter Country name", default = "")
country_name <- "Mexico"

#raw_data_ridl <- rstudioapi::askForPassword(prompt = 'raw_data_ridl')
# raw_data_ridl <- rstudioapi::showPrompt(  title = "raw_data_ridl", message = "Enter precise URL for the csv datain RIDL", default = "")
raw_data_ridl <- 'https://ridl.unhcr.org/dataset/c5d5cb38-17b3-4ae1-9d27-74546b817243/resource/55cbfd5c-0199-4f13-900d-a3c847717ea7/download/survey-xlsx-data__am7exzqgay4xxww5ecjmtb_data.xlsx'

# read data ---------------------------------------------------------------

df_raw <- resource_fetch(raw_data_ridl) |> 
  readxl::read_xlsx(col_types = "text") |> 
  clean_names()

# select core questions ---------------------------------------------------

df_wrangle <- df_raw |> 
  mutate(a_introduction_a003_date = ymd(a_introduction_a003_date)) |> 
  select(start,                                                                                                           
         end,                                                                                                             
         today,
         id = id,
         uuid = uuid,
         A001_Organization_eng = a_introduction_a001_organization_esp,
         A001_Organization_esp = a_introduction_a001_organization_esp,
         A002_staff = a_introduction_a002_staff,
         A003_date = a_introduction_a003_date,
         A004_location = a_introduction_a004_location,
         consent_text = a_introduction_consent_text,
         A006_consent = a_introduction_a006_consent,
         B001_sex = questionnaire_b_biodata_b001_sex,
         B002_age = questionnaire_b_biodata_b002_age,
         B004_nationality = questionnaire_b_biodata_b004_nationality,
         B004_nationality_africa = questionnaire_b_biodata_b004_nationality_africa,
         B004_nationality_other = questionnaire_b_biodata_b004_nationality_other,
         nationality_dual = questionnaire_b_demographics_b004a_second_nationality,                                                                  
         nationality_dual_other = questionnaire_b_demographics_b004a_second_nationality_other,
         B004_nationality_stateless = questionnaire_b_biodata_b004_nationality_stateless,
         B008_education = questionnaire_b_biodata_b008_education,
         B010_documentation = questionnaire_b_biodata_b010_documentation,
         B011_habitual_residence = questionnaire_b_biodata_b011_habitual_residence,
         intention = questionnaire_b_biodata_intention,
         B012_appliedprot = questionnaire_b_biodata_b012_appliedprot,
         appliedprot_mex = questionnaire_f_now_arrival_f004_asy_app,
         C004_reasons_to_leave_origin = questionnaire_reasoncoo_c004_reasons_to_leave_origin,
         C001_when_left_coo = questionnaire_c_leaving_c001_when_left_coo,
         C004_reasons_to_leave_habitual_residence = questionnaire_c_leaving_c004_reasons_to_leave_habitual_residence,
         D001a_destination_country = questionnaire_d_destination_d001a_destination_country,
         D002_intention_return_ven = questionnaire_d_destination_d002_intention_return_ven,
         D004_why_return_ven = questionnaire_d_destination_d004_why_return_ven,
         D004_why_return_ven_other = questionnaire_d_destination_d004_why_return_ven_other_2,
         D005_return_state_ven = questionnaire_d_destination_d005_return_state_ven,
         E001_arrival_date = questionnaire_e_arrival_e001_arrival_date,
         E002_transit_country_001 = questionnaire_e_arrival_e002_transit_country_001,
         E003_arrival_how = questionnaire_e_arrival_e003_arrival_how,
         F004_asy_app = questionnaire_f_now_arrival_f004_asy_app,
         F004a_asyapp_outcome = questionnaire_f_now_arrival_f004a_asyapp_outcome,
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
  ) |> 
  mutate(B012_appliedprot = case_when(B012_appliedprot == "yes" | appliedprot_mex == "yes" ~ "yes",
                           B012_appliedprot == "no" & appliedprot_mex == "no" ~ "no",
                           is.na(B012_appliedprot) & appliedprot_mex == "no" ~ "no",
                           is.na(appliedprot_mex) & B012_appliedprot == "no" ~ "no",
                           TRUE ~ NA)) |> 
  select(-c(appliedprot_mex))


# fix question labels -----------------------------------------------------

df_wrangle <- df_wrangle |> 
  mutate(journey_food = gsub("1", "local_restaurants", journey_food),
         journey_food = gsub("2", "canteens", journey_food),
         journey_food = gsub("3", "local_population", journey_food),
         journey_food = gsub("4", "local_market", journey_food),
         journey_food = gsub("5", "occasionally_eaten", journey_food),
         journey_food = gsub("6", "other", journey_food),
         ### this category doesn't exist in the regional database, so we recoded as 'other'
         NMeals = case_when(NMeals == "1" ~ "none",
                            NMeals == "2"  ~ "one",
                            NMeals == "3" ~ "two",
                            NMeals == "4"  ~ "three_more",
                            TRUE ~ NA_character_),
         PercFoodSec = case_when(PercFoodSec == "1" ~ "no_difficulties",
                                 PercFoodSec == "2"  ~ "less_expensive",
                                 PercFoodSec == "3" ~ "skipped_meals",
                                 PercFoodSec == "4"  ~ "whole_day_without_eat",
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


# remove unnecessary rows -------------------------------------------------

df_wrangle <- df_wrangle |>
  filter(!uuid %in% c("f283f11f-46cf-496e-bbee-b091627c83b6",
                      "7307621f-44e1-4f0b-a00e-5005cb5e7e77",
                      "37676224-e8af-4d04-a3f4-ccdb5c6b5a77",
                      "e57cc309-4c49-440b-8bd0-b5fc89530047",
                      "90374640-74ad-4524-bd82-8e2f6f813f7b",
                      "2fec3bde-36fe-4f57-b820-2abcc22fa787",
                      "04317193-a636-40d3-a0f4-0214d1f3c112", 
                      "5f29d08e-1eff-41d2-bdda-af8f36343f3c"))


# fix nationality ---------------------------------------------------------

df_wrangle <- df_wrangle |> 
  mutate(B004_nationality = case_when(B004_nationality == "Africa" ~ "",
                                      B004_nationality == "Other" ~ "",
                                      is.na(B004_nationality) ~ "",
                                      TRUE ~ B004_nationality),
         B004_nationality_other = case_when(is.na(B004_nationality_other) ~ "",
                                            TRUE ~ B004_nationality_other),
         B004_nationality_africa = case_when(B004_nationality_africa == "Other" ~ "",
                                             is.na(B004_nationality_africa) ~ "",
                                             TRUE ~ B004_nationality_africa),
         nationality_dual = case_when(nationality_dual == "other" ~ "",
                                      is.na(nationality_dual) ~ "",
                                      TRUE ~ nationality_dual),
         nationality_dual_other = case_when(is.na(nationality_dual_other) ~ "",
                                            TRUE ~ nationality_dual_other),
         B004_nationality = paste0(B004_nationality,";", 
                                   B004_nationality_other,";",
                                   B004_nationality_africa,";",
                                   nationality_dual,";",
                                   nationality_dual_other
                                   ),
         B004_nationality = str_replace_all(B004_nationality,"^[;]+|[;]+$",""),
         B004_nationality = str_replace_all(B004_nationality,"[;]+",";"),
  ) |> 
  select(-c(B004_nationality_africa, nationality_dual, nationality_dual_other))



df_wrangle <- df_wrangle |> 
  mutate(B004_nationality = case_when(B004_nationality == "BANGLADESH" ~ "Bangladesh",
                                      B004_nationality == "Belice" ~ "Belize",
                                      B004_nationality == "Burkina_Faso" ~ "Burkina Faso",
                                      B004_nationality == "Camerun" ~ "Cameroon",
                                      B004_nationality == "Costa_de_Marfil" ~ "Cote d'Ivoire",
                                      B004_nationality == "elSalvador" ~ "El Salvador",
                                      B004_nationality == "Estados Unidos" ~ "USA",
                                      B004_nationality == "Estados Unidos de América" ~ "USA",
                                      B004_nationality == "Etiopia" ~ "Ethiopia",
                                      B004_nationality == "Guatemala;elSalvador" ~ "Guatemala;El Salvador",
                                      B004_nationality == "Haiti;Brazil" ~ "Haiti;Brasil",
                                      B004_nationality == "mexicana" ~ "Mexico",
                                      B004_nationality == "Mexicana" ~ "Mexico",
                                      B004_nationality == "Mexicano" ~ "Mexico",
                                      B004_nationality == "Nicaragua;CostaRica" ~ "Nicaragua;Costa Rica",
                                      B004_nationality == "Nicaragua;elSalvador" ~ "Nicaragua;El Salvador",
                                      B004_nationality == "peru" ~ "Peru",
                                      B004_nationality == "Perú" ~ "Peru",
                                      B004_nationality == "PERU;Ecuador" ~ "Peru;Ecuador",
                                      B004_nationality == "Peruana" ~ "Peru",
                                      B004_nationality == "Republica_Democrática_Congo" ~ "Democratic Republic of the Congo",
                                      B004_nationality == "RepublicaDominicana" ~ "Republica Dominicana",
                                      B004_nationality == "Somali" ~ "Somalia",
                                      B004_nationality == "Tchad" ~ "Chad",
                                      B004_nationality == "Venezuela;Brazil" ~ "Venezuela;Brasil",
                                      B004_nationality == "Venezuela;Europe" ~ "Venezuela;Other",
                                      B004_nationality == "Venezuela;TrinidadYTobago" ~ "Venezuela;Trinidad and Tobago",
                                      TRUE ~ B004_nationality),
         B004_nationality_other = NA_character_
  )



df_wrangle |> 
  pull(B004_nationality) |> 
  unique()

# create dual nationality ---------------------------------------------

df_wrangle <- df_wrangle |> 
  mutate(dual_nationality = case_when(str_detect(B004_nationality, ";") ~ 1L,
                                      TRUE ~ 0L))


# Country of destination --------------------------------------------------

df_wrangle <- df_wrangle |> 
  mutate(D001a_destination_country = case_match(D001a_destination_country,
                                                "Europe" ~ "Other",
                                                .default = D001a_destination_country
  ))

# COR ---------------------------------------------------------------------

df_wrangle <- df_wrangle |> 
  mutate(B011_habitual_residence = case_match(B011_habitual_residence,
                                              "elSalvador" ~ "El Salvador",
                                              "other" ~ "Other",
                                              "Africa" ~ "Other",
                                              "Europe" ~ "Other",
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
                                                                                "Brazil" = "Brasil",
                                                                                "other" = "Other",
                                                                                "Europe" = "Other")))


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

# # select dates -------------------------------------------------------------
# 
# df_wrangle <- df_wrangle |> 
#   filter(A003_date >= '2023-03-01')
# 
# df_wrangle <- df_wrangle |> 
#   filter(A003_date < '2023-04-01')

# eliminate interviews to mexican population -------------------------------------------------------------

df_wrangle <- df_wrangle |> 
  filter(B004_nationality != "Mexico" & !B004_nationality_other %in% c("Mexico", "Mexicana")) |> 
  filter(!str_detect(B004_nationality,"Mexico"))

# Clean column organization -------------------------------------------------------------

df_wrangle$A001_Organization_esp[df_wrangle$A001_Organization_esp == 'UNHCR'] <- 'ACNUR'
df_wrangle$A001_Organization_eng[df_wrangle$A001_Organization_eng == 'Otro'] <- 'Other'

# Input missing locations  -------------------------------------------------------------

staff_mexicali <- c("Anel Blanco Ramirez", "Alejandra Mata Garrido", "Giovanna Zamarroni")

df_wrangle$A004_location[df_wrangle$A004_location == 'Other' & df_wrangle$A002_staff %in% staff_mexicali] <- 'Mexicali'


# clean why return other --------------------------------------------------

df_wrangle |> 
  filter(A003_date >= "2023-04-01" & A003_date <= "2023-06-30") |> 
  distinct(id, D004_why_return_ven_other) |> 
  filter(!is.na(D004_why_return_ven_other))


# type of reasons to leave ------------------------------------------------

df_wrangle |> 
  pull(C004_reasons_to_leave_origin) |> 
  unique() |> 
  str_split(" ") |> 
  unlist() |> 
  unique()

df_wrangle |> 
  filter(A003_date >= "2023-04-01"  & A003_date <= "2023-06-30") |> 
  mutate(both_violence_rights_coo = case_when(str_detect(C004_reasons_to_leave_origin, "discrimination|generalized_violence|threats_intimidation|threats_to_my_life_family|victim_violence") &
                                                str_detect(C004_reasons_to_leave_origin, "lack_education|lack_employment|lack_food|lack_medical") ~ 1L,
                                              TRUE ~ 0L),
         both_violence_rights_cor = case_when(str_detect(C004_reasons_to_leave_habitual_residence, "discrimination|generalized_violence|threats_intimidation|threats_to_my_life_family|victim_violence") &
                                                str_detect(C004_reasons_to_leave_habitual_residence, "lack_education|lack_employment|lack_food|lack_medical") ~ 1L,
                                              TRUE ~ 0L)
  ) |> 
  select(both_violence_rights_coo, C004_reasons_to_leave_origin, both_violence_rights_cor, C004_reasons_to_leave_habitual_residence ) |> 
  summarise(both_violence_rights_coo = sum(both_violence_rights_coo, na.rm = TRUE),
            both_violence_rights_cor = sum(both_violence_rights_cor, na.rm = TRUE))



# write locally the clean data --------------------------------------------
file_name <- paste0(tolower(country_name), '_mm_questionnaire_clean_pmt', tolower(month.name[month(max(df_wrangle$today))]), "_",year(max(df_wrangle$today)),".csv")

write_csv(df_wrangle, paste0('./data-wrangle/', file_name))

# write clean data to ridl ------------------------------------------------


dataset_id <- gsub("(.*dataset/)(.*)(/resource.*)", "\\2",raw_data_ridl)


m <- resource_metadata(
  type = "data",
  url = file_name,
  name = paste0(country_name, ": Mixed movement - Cleaned - PMT"),
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

