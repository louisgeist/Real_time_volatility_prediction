library(plyr)
library(dplyr)
library(ggplot2)
library(lubridate)
library(tidyr)
library(zoo)
library(tseries)
library(readr)
library(alfred)
library(mfGARCH)
library(rugarch)

# ----- 1. Data import -----
source("./data_import.R")

# ------ 2. Training ------
source("./estimation.R")

models_list = c("GM_dhoust","GM_ip","GM_nai","GM_nfci","GM_sp_Rvol22","GM_sp_vix","GM_sp_vrp","GM_vix_dhoust")

# ------ 3. Forecast ------
source("./forecast.R")
h = 80

for(model in models_list){
  # variable_name = substr(model, start = 4, stop = nchar(model))
  
  new_forecast = real_time_optimal_forecast(get(model),h,df_spx) %>% dplyr::rename(!!model := "forecast")
  print(names(new_forecast))
  #
  
  if(model == models_list[[1]]){
    df_forecast = new_forecast
  } else{
    df_forecast = df_forecast %>% merge(new_forecast, by = "date")
  }
}


# ----- 4. save in .csv ------

name = paste0("./data_plot/df_forecast_",today(),".csv")

write_csv(df_forecast, name)

print("real_time_data_import done !")


### Example of process

# # ---- Explanatory variables ----
# ## HOUST
# df_dhoust = import_houst()
# 
# # --- Example of use ----
# # Estimation
# df = df_spx %>% merge(df_dhoust, by = "date") %>% as_tibble()
# GM_dhoust = mfGARCH::fit_mfgarch(
#   data = df,
#   y = "spx",
#   x = "value",
#   low.freq = "year_month",
#   K =  36,
#   weighting = "beta.unrestricted"
# )
# 
# # Forecasts
# h = 30
# 
# dhoust_forecast = real_time_optimal_forecast(GM_dhoust,h,df_spx) %>% dplyr::rename("GM_dhoust" = "forecast")
