# CERVICAL CANCER SCREENING AND TESTING CASCADE
# 1. CXCA_SCRN
cxca_scrn <- dataset %>%
  filter(sex == "Female"
         & age >= 15
         & tx_curr == "Yes"
         & between(as.Date(date_of_cervical_cancer_screening_yyyy_mm_dd), as.Date("2024-04-01"), as.Date("2025-03-31"))
         & cervical_cancer_screening_type %in% c("First Time Screening", "Follow-up after previous negative result or suspected cancer", "Post-treatment Follow-up")
         & cervical_cancer_screening_method %in% c("VIA", "HPV", "PAP Smear")
         & result_of_cervical_cancer_screening %in% c("Negative", "Positive", "Suspicious for cancer")
         ) %>%
  group_by(ip, state, l_g_a, facility_name, datim_id) %>%
  summarise(cxca_scrn = n()) %>%
  ungroup()

# 2. CXCA_SCRN_POS
cxca_scrn_pos <- dataset %>%
  filter(sex == "Female"
         & age >= 15
         & between(as.Date(date_of_cervical_cancer_screening_yyyy_mm_dd), as.Date("2024-10-01"), as.Date("2025-03-31"))
         & cervical_cancer_screening_type %in% c("First Time Screening", "Follow-up after previous negative result or suspected cancer", "Post-treatment Follow-up")
         & cervical_cancer_screening_method %in% c("VIA", "HPV", "PAP Smear")
         & result_of_cervical_cancer_screening == "Positive"
         ) %>%
  group_by(ip, state, l_g_a, facility_name, datim_id) %>%
  summarise(cxca_scrn_pos = n()) %>%
  ungroup()

# 3. CXCA_TX
cxca_tx <- dataset %>%
  filter(sex == "Female"
         & age >= 15
         & between(as.Date(date_of_cervical_cancer_screening_yyyy_mm_dd), as.Date("2024-10-01"), as.Date("2025-03-31"))
         & cervical_cancer_screening_type %in% c("First Time Screening", "Follow-up after previous negative result or suspected cancer", "Post-treatment Follow-up")
         & cervical_cancer_screening_method %in% c("VIA", "HPV", "PAP Smear")
         & result_of_cervical_cancer_screening == "Positive"
         & between(as.Date(date_of_precancerous_lesions_treatment_yyyy_mm_dd), as.Date("2024-10-01"), as.Date("2025-03-31"))
         & precancerous_lesions_treatment_methods %in% c("LEEP", "Thermal ablation", "cryotherapy")
         ) %>%
  group_by(ip, state, l_g_a, facility_name, datim_id) %>%
  summarise(cxca_tx = n()) %>%
  ungroup()


# MERGE THE TABLES
cervical_cancer_cascade <- reduce(list(cxca_scrn, cxca_scrn_pos, cxca_tx), left_join)
