# THE TB SCREENING AND TREATMENT CASCADE
# 1. TX_TB_D_screened
tx_tb_d <- dataset %>%
  filter(tx_curr == "Yes"
         & between(as.Date(date_of_tb_screening_yyyy_mm_dd), as.Date("2024-10-01"), as.Date("2025-03-31"))
         & tb_screening_type != ""
         & tb_status %in% c("No signs or symptoms of TB", "Presumptive TB and referred for evaluation", "Presumptive TB")
         ) %>%
  group_by(ip, state, l_g_a, facility_name, datim_id) %>%
  summarise(tx_tb_d = n()) %>%
  ungroup()

# 2. TX_TB_D_screening_type
tx_tb_d_screening_type <- dataset %>%
  filter(tx_curr == "Yes"
         & between(as.Date(date_of_tb_screening_yyyy_mm_dd), as.Date("2024-10-01"), as.Date("2025-03-31"))
         & tb_screening_type != ""
         & tb_status %in% c("No signs or symptoms of TB", "Presumptive TB and referred for evaluation", "Presumptive TB")
         ) %>%
  group_by(ip, state, l_g_a, facility_name, datim_id, tb_screening_type) %>%
  summarise(tx_tb_d_screen_type = n()) %>%
  ungroup() %>%
  pivot_wider(names_from = tb_screening_type, values_from = tx_tb_d_screen_type, values_fill = 0)

# 3. TX_TB_screened_positive
tx_tb_d_screen_pos <- dataset %>%
  filter(tx_curr == "Yes"
         & between(as.Date(date_of_tb_screening_yyyy_mm_dd), as.Date("2024-10-01"), as.Date("2025-03-31"))
         & tb_screening_type != ""
         & tb_status %in% c("Presumptive TB and referred for evaluation", "Presumptive TB")
         ) %>%
  group_by(ip, state, l_g_a, facility_name, datim_id) %>%
  summarise(tx_tb_d_screen_pos = n()) %>%
  ungroup()

# 4. TX_TB_D_Sample_sent
tx_tb_d_sample_sent <- dataset %>%
  filter(tx_curr == "Yes"
         & between(as.Date(date_of_tb_screening_yyyy_mm_dd), as.Date("2024-10-01"), as.Date("2025-03-31"))
         & tb_screening_type != ""
         & tb_status %in% c("Presumptive TB and referred for evaluation", "Presumptive TB")
         & between(as.Date(date_of_tb_sample_collection_yyyy_mm_dd), as.Date("2024-10-01"), as.Date("2025-03-31"))
         ) %>%
  group_by(ip, state, l_g_a, facility_name, datim_id) %>%
  summarise(tx_tb_d_sample_sent = n()) %>%
  ungroup()

# 5. TX_TB_D_TB_Test_Type
tx_tb_d_test_type <- dataset %>%
  filter(tx_curr == "Yes"
         & between(as.Date(date_of_tb_screening_yyyy_mm_dd), as.Date("2024-10-01"), as.Date("2025-03-31"))
         & tb_screening_type != ""
         & tb_status %in% c("Presumptive TB and referred for evaluation", "Presumptive TB")
         & between(as.Date(date_of_tb_sample_collection_yyyy_mm_dd), as.Date("2024-10-01"), as.Date("2025-03-31"))
         & tb_diagnostic_test_type != "" 
         & between(as.Date(date_of_tb_diagnostic_result_received_yyyy_mm_dd), as.Date("2024-10-01"), as.Date("2025-03-31"))
         & tb_diagnostic_result %in% c("NEG",
                                       "Negative",
                                       "Negative ",
                                       "NEGATIVE",
                                       "+",
                                       "-",
                                       "negative",
                                       "-ve",
                                       "Positive",
                                       "NAGETIVE",
                                       "Not suggestive for TB",
                                       "X-ray suggestive",
                                       "MTB Detected (Rifampicin Resistance Detected)",
                                       "MTb Not Detected",
                                       "neg",
                                       "NEGATIVE ",
                                       "Neg",
                                       "PTB NOT DETECTED",
                                       "Negetive",
                                       "NEGTAIVE",
                                       "MTB not detected",
                                       "ng",
                                       "P0SIT",
                                       "MTB Detected (Rifampicin not Resistance)",
                                       "positive",
                                       "POSITIVE",
                                       "POS",
                                       "AFB Negative",
                                       "MTB NOT DETECTED",
                                       "PTB DETECTED",
                                       "NGE",
                                       "MTB detected RIF resistance not detected",
                                       "NEGETIVE",
                                       "NEGEGATIVE",
                                       "Suggestive for TB",
                                       "NAGETINE",
                                       "MTB detected RIF resistance detected",
                                       "NAGETTIVE",
                                       "PTB Detected",
                                       "P0STIVE",
                                       "+ve",
                                       "MTD NOT DETECTED",
                                       "non-reactive",
                                       "negatuive",
                                       "MTB DETECTED",
                                       "AFB Positive",
                                       "Positive ",
                                       "NEGETIVR",
                                       "NOT DETECTED",
                                       "Non-reactive",
                                       "negetive",
                                       "MTB trace RIF resistance indeterminate",
                                       "NOT DEDECTED",
                                       "nagetive",
                                       "Not Detected",
                                       "positive ",
                                       "NEGATIVEW",
                                       "reactive",
                                       "MBT NOT DETECTED",
                                       "non reactive",
                                       "NAGATIVE",
                                       "MTB NOT  DETECTED"
                                       )
         ) %>%
  group_by(ip, state, l_g_a, facility_name, datim_id, tb_diagnostic_test_type) %>%
  summarise(count = n()) %>%
  ungroup() %>%
  pivot_wider(names_from = tb_diagnostic_test_type, values_from = count, values_fill = 0) 

# 6. TX_TB_D_Specimen_Return
tx_tb_d_specimen_return <- dataset %>%
  filter(tx_curr == "Yes"
         & between(as.Date(date_of_tb_screening_yyyy_mm_dd), as.Date("2024-10-01"), as.Date("2025-03-31"))
         & tb_screening_type != ""
         & tb_status %in% c("Presumptive TB and referred for evaluation", "Presumptive TB")
         & between(as.Date(date_of_tb_sample_collection_yyyy_mm_dd), as.Date("2024-10-01"), as.Date("2025-03-31"))
         & tb_diagnostic_test_type != "" 
         & between(as.Date(date_of_tb_diagnostic_result_received_yyyy_mm_dd), as.Date("2024-10-01"), as.Date("2025-03-31"))
         & tb_diagnostic_result %in% c("NEG",
                                       "Negative",
                                       "Negative ",
                                       "NEGATIVE",
                                       "+",
                                       "-",
                                       "negative",
                                       "-ve",
                                       "Positive",
                                       "NAGETIVE",
                                       "Not suggestive for TB",
                                       "X-ray suggestive",
                                       "MTB Detected (Rifampicin Resistance Detected)",
                                       "MTb Not Detected",
                                       "neg",
                                       "NEGATIVE ",
                                       "Neg",
                                       "PTB NOT DETECTED",
                                       "Negetive",
                                       "NEGTAIVE",
                                       "MTB not detected",
                                       "ng",
                                       "P0SIT",
                                       "MTB Detected (Rifampicin not Resistance)",
                                       "positive",
                                       "POSITIVE",
                                       "POS",
                                       "AFB Negative",
                                       "MTB NOT DETECTED",
                                       "PTB DETECTED",
                                       "NGE",
                                       "MTB detected RIF resistance not detected",
                                       "NEGETIVE",
                                       "NEGEGATIVE",
                                       "Suggestive for TB",
                                       "NAGETINE",
                                       "MTB detected RIF resistance detected",
                                       "NAGETTIVE",
                                       "PTB Detected",
                                       "P0STIVE",
                                       "+ve",
                                       "MTD NOT DETECTED",
                                       "non-reactive",
                                       "negatuive",
                                       "MTB DETECTED",
                                       "AFB Positive",
                                       "Positive ",
                                       "NEGETIVR",
                                       "NOT DETECTED",
                                       "Non-reactive",
                                       "negetive",
                                       "MTB trace RIF resistance indeterminate",
                                       "NOT DEDECTED",
                                       "nagetive",
                                       "Not Detected",
                                       "positive ",
                                       "NEGATIVEW",
                                       "reactive",
                                       "MBT NOT DETECTED",
                                       "non reactive",
                                       "NAGATIVE",
                                       "MTB NOT  DETECTED"
                                       )
         ) %>%
  group_by(ip, state, l_g_a, facility_name, datim_id) %>%
  summarise(tx_tb_d_specimen_return = n()) %>%
  ungroup()

# 7. TX_TB_TB_diagnosed
tx_tb_tb_diagnosed <- dataset %>%
  filter(tx_curr == "Yes"
         & between(as.Date(date_of_tb_screening_yyyy_mm_dd), as.Date("2024-10-01"), as.Date("2025-03-31"))
         & tb_screening_type != ""
         & tb_status %in% c("Presumptive TB and referred for evaluation", "Presumptive TB")
         & between(as.Date(date_of_tb_sample_collection_yyyy_mm_dd), as.Date("2024-10-01"), as.Date("2025-03-31"))
         & tb_diagnostic_test_type != "" 
         & between(as.Date(date_of_tb_diagnostic_result_received_yyyy_mm_dd), as.Date("2024-10-01"), as.Date("2025-03-31"))
         & tb_diagnostic_result %in% c("+",
                                       "Positive",
                                       "X-ray suggestive",
                                       "MTB Detected (Rifampicin Resistance Detected)",
                                       "P0SIT",
                                       "MTB Detected (Rifampicin not Resistance)",
                                       "positive",
                                       "POSITIVE",
                                       "POS",
                                       "Suggestive for TB",
                                       "MTB detected RIF resistance detected",
                                       "PTB Detected",
                                       "P0STIVE",
                                       "+ve",
                                       "MTB DETECTED",
                                       "AFB Positive",
                                       "Positive ",
                                       "MTB trace RIF resistance indeterminate",
                                       "positive ",
                                       "reactive"
                                       )
         ) %>%
  group_by(ip, state, l_g_a, facility_name, datim_id) %>%
  summarise(tx_tb_tb_diagnosed = n()) %>%
  ungroup()

# 8. TX_TB_N
tx_tb_n <- dataset %>%
  filter(tx_curr == "Yes"
         & between(as.Date(date_of_tb_screening_yyyy_mm_dd), as.Date("2024-10-01"), as.Date("2025-03-31"))
         & tb_screening_type != ""
         & tb_status %in% c("Presumptive TB and referred for evaluation", "Presumptive TB")
         & between(as.Date(date_of_tb_sample_collection_yyyy_mm_dd), as.Date("2024-10-01"), as.Date("2025-03-31"))
         & tb_diagnostic_test_type != "" 
         & between(as.Date(date_of_tb_diagnostic_result_received_yyyy_mm_dd), as.Date("2024-10-01"), as.Date("2025-03-31"))
         & tb_diagnostic_result %in% c("+",
                                       "Positive",
                                       "X-ray suggestive",
                                       "MTB Detected (Rifampicin Resistance Detected)",
                                       "P0SIT",
                                       "MTB Detected (Rifampicin not Resistance)",
                                       "positive",
                                       "POSITIVE",
                                       "POS",
                                       "Suggestive for TB",
                                       "MTB detected RIF resistance detected",
                                       "PTB Detected",
                                       "P0STIVE",
                                       "+ve",
                                       "MTB DETECTED",
                                       "AFB Positive",
                                       "Positive ",
                                       "MTB trace RIF resistance indeterminate",
                                       "positive ",
                                       "reactive"
                                       )
         & between(as.Date(date_of_start_of_tb_treatment_yyyy_mm_dd), as.Date("2024-10-01"), as.Date("2025-03-31"))
         ) %>%
  group_by(ip, state, l_g_a, facility_name, datim_id) %>%
  summarise(tx_tb_n = n()) %>%
  ungroup()

# MERGE THE TABLES
tb_screening_cascade <- reduce(list(tx_curr, 
                                    tx_tb_d, 
                                    tx_tb_d_screening_type, 
                                    tx_tb_d_screen_pos, 
                                    tx_tb_d_sample_sent,
                                    tx_tb_d_test_type,
                                    tx_tb_d_specimen_return,
                                    tx_tb_tb_diagnosed,
                                    tx_tb_n), 
                               left_join)
