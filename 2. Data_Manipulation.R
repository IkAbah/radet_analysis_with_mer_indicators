# DATA MANIPULATION TO AID ANALYSIS
# a. Standardize the date format in the date columns
RADET <- RADET %>%
  mutate(across(all_of(contains("date")), ~ as.Date(as.integer(.), origin = "1899-12-30")))

# b. Clean the names
RADET <- clean_names(RADET)

# c. More calculations
RADET <- RADET %>%
  mutate(# Clean the target groups with multiple representations
         target_group_new = case_when(is.na(target_group) | target_group == "" ~ "Gen Pop", 
                                      target_group == "SEXUAL PARTNERS" ~ "SP",
                                      target_group == "TG0" ~ "TRANSGENDER",
                                      target_group == "PRISONS" ~ "PRISON",
                                      TRUE ~ target_group
                                      ),
         # Calculate the age of all clients
         age = round(time_length(difftime(as.Date(date_of_current_art_status, format = "%Y-%m-%d"), as.Date(date_of_birth_yyyy_mm_dd, format = "%Y-%m-%d")), "years"), digits = 0),
         # Define the Age groups according to the MER guidance
         age_group = case_when(age < 0 ~ "unknown",
                               age <= 4 ~ "0-4",
                               age <= 9 ~ "5-9",
                               age <= 14 ~ "10-14",
                               age <= 19 ~ "15-19",
                               age <= 24 ~ "20-24",
                               age <= 29 ~ "25-29",
                               age <= 34 ~ "30-34",
                               age <= 39 ~ "35-39",
                               age <= 44 ~ "40-44",
                               age <= 49 ~ "45-49",
                               age <= 54 ~ "50-54",
                               age <= 59 ~ "55-59",
                               age <= 64 ~ "60-64",
                               age > 64 ~ "65+",
                               TRUE ~ "unknown"
                               ),
         # Calculate the age at the start of ART
         age_at_art_start = round(time_length(difftime(as.Date(art_start_date_yyyy_mm_dd, format = "%Y-%m-%d"), as.Date(date_of_birth_yyyy_mm_dd, format = "%Y-%m-%d")), "years"), digits = 1),
         age_group_at_art_start = case_when(age_at_art_start >= 0 & age_at_art_start <= 4 ~ "0-4",
                                            age_at_art_start > 4 & age_at_art_start <= 9 ~ "5-9",
                                            age_at_art_start > 9 & age_at_art_start <= 14 ~ "10-14",
                                            age_at_art_start > 14 & age_at_art_start <= 19 ~ "15-19",
                                            age_at_art_start > 19 & age_at_art_start <= 24 ~ "20-24",
                                            age_at_art_start > 24 & age_at_art_start <= 29 ~ "25-29",
                                            age_at_art_start > 29 & age_at_art_start <= 34 ~ "30-34",
                                            age_at_art_start > 34 & age_at_art_start <= 39 ~ "35-39",
                                            age_at_art_start > 39 & age_at_art_start <= 44 ~ "40-44",
                                            age_at_art_start > 44 & age_at_art_start <= 49 ~ "45-49",
                                            age_at_art_start > 49 & age_at_art_start <= 54 ~ "50-54",
                                            age_at_art_start > 54 & age_at_art_start <= 59 ~ "55-59",
                                            age_at_art_start > 59 & age_at_art_start <= 64 ~ "60-64",
                                            age_at_art_start > 64 ~ "65+",
                                            TRUE ~ "unknown"
                                            ),
         # Calculate the Duration on ART and create groups
         duration_on_art_months = round(time_length(difftime(as.Date("2025-09-30", format = "%Y-%m-%d"), as.Date(art_start_date_yyyy_mm_dd, format = "%Y-%m-%d")), "months"), digits = 0),
         duration_on_art_group = case_when(duration_on_art_months <= 12 ~ "<1year",
                                           duration_on_art_months <= 24 ~ "1-2years",
                                           duration_on_art_months <= 36 ~ "2-3years",
                                           duration_on_art_months <= 48 ~ "3-4years",
                                           duration_on_art_months <= 60 ~ "4-5years",
                                           duration_on_art_months <= 72 ~ "5-6years",
                                           duration_on_art_months <= 84 ~ "6-7years",
                                           duration_on_art_months <= 96 ~ "7-8years",
                                           duration_on_art_months <= 108 ~ "8-9years",
                                           duration_on_art_months <= 120 ~ "9-10years",
                                           duration_on_art_months > 120 ~ "10years+",
                                           TRUE ~ "unknown"
                                           ),
         tx_curr = ifelse((current_art_status %in% c("Active", "Active Restart"))
                          & (client_verification_outcome %in% c("valid", "Valid") | client_verification_outcome == ""),
                          "Yes", "No"),
         tx_new = ifelse(care_entry_point != "Transfer-in" 
                         & (client_verification_outcome %in% c("valid", "Valid") | client_verification_outcome == "") 
                         & between(as.Date(art_start_date_yyyy_mm_dd), as.Date("2024-10-01"), as.Date("2025-09-30")),
                         "Yes", "No"),
         tx_ml_dead = ifelse(current_art_status == "Died" 
                             & (client_verification_outcome %in% c("valid", "Valid") | client_verification_outcome == "") 
                             & between(as.Date(date_of_current_art_status), as.Date("2024-10-01"), as.Date("2025-09-30")),
                             "Yes", "No"),
         # Convert the CD4 count to a number
         cd4_count = as.numeric(last_cd4_count),
         # Define the CD4 categories
         cd4_category = case_when(is.na(last_cd4_count) ~ "",
                                  is.na(cd4_count) & last_cd4_count %in% c(">200", ">=200", ".>200", "=>200") ~ ">=200",
                                  is.na(cd4_count) & last_cd4_count %in% c("<200", "< 200", "<=200") ~ "<200",
                                  cd4_count < 200 ~ "<200",
                                  cd4_count >= 200 ~ ">=200",
                                  TRUE ~ ""
                                  ),
         ahd = case_when(tx_new == "Yes" & age_group_at_art_start == "0-4" ~ "Yes",
                         tx_new == "Yes" & age_group_at_art_start != "0-4" & cd4_category == "<200" ~ "Yes",
                         tx_new == "Yes" & age_group_at_art_start != "0-4" & cd4_category == ">=200" & clinical_staging_at_last_visit %in% c("STAGE III", "STAGE IV") ~ "Yes",
                         TRUE ~ "No"
                         ),
         # Calculate the Duration on ART before the VL sample was collected
         duration_on_art_months_before_sample_Collection = time_length(difftime(as.Date(date_of_current_viral_load_result_sample_yyyy_mm_dd, format = "%Y-%m-%d"), as.Date(art_start_date_yyyy_mm_dd, format = "%Y-%m-%d")), "months"),
         # Calculate the Turn Around Time for the receipt of Viral load result
         tat_days = round(time_length(difftime(as.Date(date_of_current_viral_load_yyyy_mm_dd, format = "%Y-%m-%d"), as.Date(date_of_current_viral_load_result_sample_yyyy_mm_dd, format = "%Y-%m-%d")), "days"), digits = 0),
         # Categorize the Regimen into DTG-based regimen and other regimen
         regimen_type = ifelse(grepl("DTG", current_art_regimen) | grepl("Dolutegravir", current_art_regimen), "DTG-based regimen", "Others"),
         # Convert the Current VL result column to numeric variable
         current_viral_load = as.numeric(current_viral_load_c_ml),
         # Determine the clients who are eligible for a viral load test
         eligible_for_vl = case_when(tx_curr == "Yes" & (regimen_type == "DTG-based regimen") & (duration_on_art_before_sample_Collection >= 3) ~ "Yes",
                                     tx_curr == "Yes" & (regimen_type == "Others") & (duration_on_art_before_sample_Collection >= 6) ~ "Yes",
                                     TRUE ~ "No"),
         # Get the Viral load results that would count for TX_PVLS_D and TX_PVLS_N
         tx_pvls_d = case_when(is.na(current_viral_load) | current_viral_load == "" ~ "No",
                               tx_curr == "Yes" & (regimen_type == "DTG-based regimen") & (!is.na(current_viral_load)) & (eligible_for_vl == "Yes") & (between(as.Date(date_of_current_viral_load_result_sample_yyyy_mm_dd), as.Date("2024-01-01"), as.Date("2024-12-31"))) & (between(as.Date(date_of_current_viral_load_yyyy_mm_dd), as.Date("2024-01-01"), as.Date("2025-01-31"))) ~ "Yes",
                               tx_curr == "Yes" & (regimen_type == "Others") & (!is.na(current_viral_load)) & (eligible_for_vl == "Yes") & (between(as.Date(date_of_current_viral_load_result_sample_yyyy_mm_dd), as.Date("2024-01-01"), as.Date("2024-12-31"))) & (between(as.Date(date_of_current_viral_load_yyyy_mm_dd), as.Date("2024-01-01"), as.Date("2025-01-31"))) ~ "Yes",
                               TRUE ~ "No"),
         tx_pvls_n  = ifelse(tx_pvls_d == "Yes" & current_viral_load >= 0 & current_viral_load < 1000, "Yes", "No")
         ) %>%
  # Add row_id column
  rowid_to_column()

saveRDS(RADET, "fy25_usaid_radet_27-12-2024.rds")

# DEFINE THE DATASET FOR THE ANALYSIS
dataset <- `fy25_usaid_radet_27-12-2024`
