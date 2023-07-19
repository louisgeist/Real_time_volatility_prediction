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

# General paramaters (for the data download & estimation)
main_index = "spx"

# ----- 0. Tool ------ 
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

qlike <- function(h,sigma){
  return(log(h**2)+ (sigma/h)**2)
}


# ----- 1. Data import -----
final_date = ymd("2019-01-01") #for download
source("./data_import.R")

# stable storage of the data
df_main_index_raw <- df_main_index

df_dhoust_raw <- df_dhoust
df_ip_raw = df_ip
df_nai_raw = df_nai
df_nfci_raw = df_nfci
df_vix_raw = df_vix
df_vrp_raw = df_vrp
df_Rvol22_raw = df_Rvol22


# ------ 2. Training ------

date_end_training = ymd("2015-01-01")
filter_data(date_end_training)

source("./estimation.R")

# ------ 3. Forecasts & evaluation------
source("./forecast.R")
realized_library = read.csv(file = "./realized_library.csv")

n_forecasts = 5

date_to_forecast = seq_quotation_date(date_end_training, n_forecasts) # function implemented in forecast.R

h = 1 # one day forecast

# GARCH-MIDAS models
GM_models_list = c("GM_dhoust","GM_ip","GM_nai","GM_nfci","GM_Rvol22", "GM_vix","GM_vrp","GM_vix_dhoust", "GM_vix_ip", "GM_vix_nai", "GM_vix_nfci")


error_dhoust = double(n_forecasts)
for(i in seq_along(date_to_forecast)){
  print(date_to_forecast)
  
  filter_data(ymd(date_to_forecast[[i]]))
  
  print(df_main_index %>% tail(1))
  
  error_dhoust[[i]] = real_time_optimal_forecast(GM_dhoust, h, df_main_index, df_dhoust)$forecast[[1]]
  
}

error_dhoust

# old functions
for(model in GM_models_list){
  
  new_forecast = real_time_optimal_forecast(get(model),h, df_main_index) %>% select("date","forecast") %>% dplyr::rename(!!model := "forecast")
  if(model == GM_models_list[[1]] ){
    df_forecast = new_forecast 
  } else{
    df_forecast = df_forecast %>% merge(new_forecast, by = "date")
  }
}

df_forecast = df_forecast %>% mutate(date = as.Date(date))

# GARCH11
forecast_garch11  = ugarchforecast(GARCH11, n.ahead = h)@forecast$sigmaFor

df_forecast = df_forecast %>% dplyr::bind_cols(forecast_garch11[,1]) 
df_forecast = df_forecast %>% dplyr::rename("GARCH11" = paste0("...", length(df_forecast)))

# ------ 4. Validation -------
# use of 

realized_library = read.csv(file = "./realized_library.csv")

# compute of the error estimation of for one model !
