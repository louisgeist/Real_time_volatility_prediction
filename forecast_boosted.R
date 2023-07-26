
seq_quotation_date = function(initial_date, h) {
  
  list_days = seq(initial_date, initial_date + days(2 * h + 5), by = "day")
  
  unique_years = year(list_days) %>% unique()
  
  all_holidays = sapply(unique_years, function(y) ymd(holidayNYSE(year = y)))
  
  all_holidays_vec = unlist(all_holidays) # concatène dans un unique vecteur
  
  quoted_days = list_days[wday(list_days) %in% 2:6 & !(list_days %in% all_holidays_vec)]
  
  return(quoted_days[1:(h + 1)])
}

# --- main function ---
forecast_boosted <- function(alpha, beta, gamma, m, theta, K, w1, W2) {
  
}

# ---- speed evaluation ----
source("./data_import_tools.R")

## ---- import  ----
### main index
main_index = "spx"

df_spx = import_spx()
df_ndx = import_ndx()

df_main_index_raw = get(paste0("df_",main_index)) %>%
  dplyr::rename(!!main_index := "value")

### other index
df_dhoust_raw = import_houst()

## ---- estimation ----
end_of_training = ymd("2023-07-01")

df_main_index = df_main_index_raw %>% filter(date < end_of_training)
df_dhoust = df_dhoust_raw %>% filter(date < end_of_training)



df = df_main_index %>% merge(df_dhoust, by = "date")
GM_dhoust = mfGARCH::fit_mfgarch(
  data = df,
  y = main_index,
  x = "value",
  low.freq = "year_month",
  K =  36,
  weighting = "beta.unrestricted"
)



## ---- forecast test ----
source("./forecast.R")


library(bench)
sortie_mark <- mark(
  old_forecast1 = real_time_optimal_forecast(GM_dhoust, 1, df_main_index, df_dhoust)
  
)

sortie_mark

sortie_mark <- mark(
  old_forecast100 = real_time_optimal_forecast(GM_dhoust, 100, df_main_index, df_dhoust)
)

sortie_mark

system.time(real_time_optimal_forecast(GM_dhoust, 1, df_main_index, df_dhoust))

