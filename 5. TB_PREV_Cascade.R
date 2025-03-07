# THE TB PREVENTION CASCADE
# 1. TB_PREV_D
tb_prev_d <- dataset %>%
  filter(client_verification_outcome == "valid" | is.na(client_verification_outcome)
         & between(as.Date(date_of_tpt_start_yyyy_mm_dd), as.Date("2024-04-01"), as.Date("2024-10-31"))
         & tpt_type %in% c("Isoniazid-(INH) 300mg",
                           "Isoniazid and Rifapentine-(3HP)",
                           "Isoniazid-(INH) 100mg",
                           "Isoniazid 100mg",
                           "Isoniazid(300mg)/Pyridoxine(25mg)/Cotrimoxazole(960mg)",
                           "Isoniazid 300mg",
                           "Isoniazid/Rifampentine(3HP)",
                           "Isoniazid/Rifampicin(3HR)",
                           "Cotrimoxazole/Isoniazid/Pyridoxine (960/300/25mg)"
                           )
         ) %>%
  group_by(ip, state, l_g_a, facility_name, datim_id) %>%
  summarise(tb_prev_d = n()) %>%
  ungroup()

# 2. TB_PREV_N
tb_prev_n <- dataset %>%
  filter(client_verification_outcome == "valid" | is.na(client_verification_outcome)
         & between(as.Date(date_of_tpt_start_yyyy_mm_dd), as.Date("2024-04-01"), as.Date("2024-10-31"))
         & tpt_type %in% c("Isoniazid-(INH) 300mg",
                           "Isoniazid and Rifapentine-(3HP)",
                           "Isoniazid-(INH) 100mg",
                           "Isoniazid 100mg",
                           "Isoniazid(300mg)/Pyridoxine(25mg)/Cotrimoxazole(960mg)",
                           "Isoniazid 300mg",
                           "Isoniazid/Rifampentine(3HP)",
                           "Isoniazid/Rifampicin(3HR)",
                           "Cotrimoxazole/Isoniazid/Pyridoxine (960/300/25mg)"
                           )
         & between(as.Date(tpt_completion_date_yyyy_mm_dd), as.Date("2024-08-01"), as.Date("2025-03-31"))
         & tpt_completion_status %in% c("IPT Completed", "Treatment completed")
         ) %>%
  group_by(ip, state, l_g_a, facility_name, datim_id) %>%
  summarise(tb_prev_n = n()) %>%
  ungroup()

# MERGE THE TABLES
tb_prevention_cascade <- reduce(list(tb_prev_d, tb_prev_n), left_join)
