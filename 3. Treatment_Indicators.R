# ANALYSE THE MER INIDICATORS FROM THE RADET
# Quarterly Indicators
# 1. TX_CURR
tx_curr <- dataset %>%
  filter(tx_curr == "Yes") %>%
  group_by(ip, state, l_g_a, facility_name, datim_id) %>%
  summarise(tx_curr = n()) %>%
  ungroup()

# 2. TX_NEW
tx_new <- dataset %>%
  filter(tx_new == "Yes"
         & between(art_start_date_yyyy_mm_dd, as.Date("2024-10-01"), as.Date("2024-12-31"))
         ) %>%
  group_by(ip, state, l_g_a, facility_name, datim_id) %>%
  summarise(tx_new = n()) %>%
  ungroup()

# 3. TX_PVLS_D
tx_pvls_d <- dataset %>%
  filter(tx_pvls_d == "Yes") %>%
  group_by(ip, state, l_g_a, facility_name, datim_id) %>%
  summarise(tx_pvls_d = n()) %>%
  ungroup()

# 4. TX_PVLS_N
tx_pvls_n <- dataset %>%
  filter(tx_pvls_n == "Yes") %>%
  group_by(ip, state, l_g_a, facility_name, datim_id) %>%
  summarise(tx_pvls_n = n()) %>%
  ungroup()

# MERGE THE TABLES
treatment_cascade <- reduce(list(tx_curr, tx_new, tx_pvls_d, tx_pvls_n), left_join)