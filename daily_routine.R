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



# ------ 3. Forecast ------
source("./forecast.R")
h = 80

# GARCH-MIDAS models
GM_models_list = c("GM_dhoust","GM_ip","GM_nai","GM_nfci","GM_Rvol22","GM_vix","GM_vrp","GM_vix_dhoust", "GM_vix_ip", "GM_vix_nai", "GM_vix_nfci")

for(model in GM_models_list){
  # variable_name = substr(model, start = 4, stop = nchar(model))
  
  new_forecast = real_time_optimal_forecast(get(model),h,df_spx) %>% select("date","forecast")%>% dplyr::rename(!!model := "forecast")
  print(names(new_forecast))
  
  
  if(model == GM_models_list[[1]]){
    df_forecast = new_forecast 
  } else{
    df_forecast = df_forecast %>% merge(new_forecast, by = "date")
  }
}



# GARCH11
forecast_garch11  = ugarchforecast(GARCH11, n.ahead = h)@forecast$sigmaFor

df_forecast = df_forecast %>% bind_cols(forecast_garch11[,1]) 
df_forecast = df_forecast %>% dplyr::rename("GARCH11" = paste0("...", length(df_forecast)))


# ----- 4. save in .csv ------
# past data save
index_list = c("dhoust","ip","nai","nfci","Rvol22","vix","vrp")

df_training_data = df_spx

for(index in index_list){
  new_df = get(paste0("df_",index)) %>% dplyr::rename(!!index := "value")
  
  df_training_data = df_training_data %>% merge(new_df, by = "date")
  
  
  #if(index == index_list[[1]]){
  #  df_training_data = new_df
  #} else{
  #  df_training_data = df_training_data %>% merge( new_df, by = "date")
  #}
}

name = paste0("./data_plot/", today(),"_training_data.csv")
write_csv(df_training_data, name)

# forecasts save
name = paste0("./data_plot/", today(),"_forecasts.csv")

write_csv(df_forecast, name)

print("real_time_data_import done !")


### Example of complete process (estimation is done in this script, not using estimation.R for the example)

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
