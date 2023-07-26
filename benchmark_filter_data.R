install.packages("bench")
library(bench)

# test de rapidité : filter_data

last_date = ymd("2022-05-23")

## two other options
filter_data = function(last_date) {
  # watch out, it affects global variables
  df_main_index <<- df_main_index_raw %>% filter(date < last_date)
  
  df_dhoust <<- df_dhoust_raw %>% filter(floor_date(date, unit = "month") < floor_date(last_date,unit = "month"))
  df_ip <<- df_ip_raw %>% filter(floor_date(date, unit = "month") < floor_date(last_date,unit = "month"))
  df_nai <<- df_nai_raw %>% filter(floor_date(date, unit = "month") < floor_date(last_date,unit = "month"))
  df_nfci <<- df_nfci_raw %>% filter(floor_date(date, unit = "week",5) < floor_date(last_date, unit = "week",5))
  df_vix <<- df_vix_raw %>% filter(date < last_date)
  df_vrp <<- df_vrp_raw %>% filter(date < last_date)
  df_Rvol22 <<- df_Rvol22_raw %>% filter(date < last_date)
}

filter_data2 = function(last_date, df_main_index_raw, df_dhoust_raw, df_ip_raw, df_nai_raw, df_nfci_raw, df_vix_raw, df_vrp_raw, df_Rvol22_raw) {
  # Filter data and return new DataFrames
  df_main_index_filtered <- df_main_index_raw %>% filter(date < last_date)
  df_dhoust_filtered <- df_dhoust_raw %>% filter(floor_date(date, unit = "month") < floor_date(last_date, unit = "month"))
  df_ip_filtered <- df_ip_raw %>% filter(floor_date(date, unit = "month") < floor_date(last_date, unit = "month"))
  df_nai_filtered <- df_nai_raw %>% filter(floor_date(date, unit = "month") < floor_date(last_date, unit = "month"))
  df_nfci_filtered <- df_nfci_raw %>% filter(floor_date(date, unit = "week", 5) < floor_date(last_date, unit = "week", 5))
  df_vix_filtered <- df_vix_raw %>% filter(date < last_date)
  df_vrp_filtered <- df_vrp_raw %>% filter(date < last_date)
  df_Rvol22_filtered <- df_Rvol22_raw %>% filter(date < last_date)
  
  # Return filtered DataFrames as a list
  return(list(df_main_index_filtered, df_dhoust_filtered, df_ip_filtered, df_nai_filtered, df_nfci_filtered, df_vix_filtered, df_vrp_filtered, df_Rvol22_filtered))
}

filter_data_optimized = function(last_date, df_main_index_raw, df_dhoust_raw, df_ip_raw, df_nai_raw, df_nfci_raw, df_vix_raw, df_vrp_raw, df_Rvol22_raw) {
  # Find the index where the condition is true
  index_main_index <- which(df_main_index_raw$date < last_date)
  index_dhoust <- which(floor_date(df_dhoust_raw$date, unit = "month") < floor_date(last_date, unit = "month"))
  index_ip <- which(floor_date(df_ip_raw$date, unit = "month") < floor_date(last_date, unit = "month"))
  index_nai <- which(floor_date(df_nai_raw$date, unit = "month") < floor_date(last_date, unit = "month"))
  index_nfci <- which(floor_date(df_nfci_raw$date, unit = "week", 5) < floor_date(last_date, unit = "week", 5))
  index_vix <- which(df_vix_raw$date < last_date)
  index_vrp <- which(df_vrp_raw$date < last_date)
  index_Rvol22 <- which(df_Rvol22_raw$date < last_date)
  
  # Subset the DataFrames using the index
  df_main_index_filtered <- df_main_index_raw[index_main_index, ]
  df_dhoust_filtered <- df_dhoust_raw[index_dhoust, ]
  df_ip_filtered <- df_ip_raw[index_ip, ]
  df_nai_filtered <- df_nai_raw[index_nai, ]
  df_nfci_filtered <- df_nfci_raw[index_nfci, ]
  df_vix_filtered <- df_vix_raw[index_vix, ]
  df_vrp_filtered <- df_vrp_raw[index_vrp, ]
  df_Rvol22_filtered <- df_Rvol22_raw[index_Rvol22, ]
  
  # Return filtered DataFrames as a list
  return(list(df_main_index_filtered, df_dhoust_filtered, df_ip_filtered, df_nai_filtered, df_nfci_filtered, df_vix_filtered, df_vrp_filtered, df_Rvol22_filtered))
}


## évaluation avec package bench
sortie_mark <- mark(
  func_global_variable = filter_data(last_date),
  #func_local_variable = filter_data2(last_date, df_main_index_raw, df_dhoust_raw, df_ip_raw, df_nai_raw, df_nfci_raw, df_vix_raw, df_vrp_raw, df_Rvol22_raw)
  
)

sortie_mark

sortie_mark <- mark(
  #func_global_variable = filter_data(last_date),
  func_local_variable = filter_data2(last_date, df_main_index_raw, df_dhoust_raw, df_ip_raw, df_nai_raw, df_nfci_raw, df_vix_raw, df_vrp_raw, df_Rvol22_raw),
  func_optimized = filter_data_optimized(last_date, df_main_index_raw, df_dhoust_raw, df_ip_raw, df_nai_raw, df_nfci_raw, df_vix_raw, df_vrp_raw, df_Rvol22_raw)
  
)

sortie_mark
