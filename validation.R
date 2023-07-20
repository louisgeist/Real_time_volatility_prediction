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

qlike <- function(h,sigma_square){
  return(sigma_square/h -log( sigma_square / h) - 1)
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


n_forecasts = 1 #number of days for the test set
date_to_forecast = seq_quotation_date(date_end_training, n_forecasts) # function implemented in forecast.R


## --- Test set ---
realized_library = read.csv(file = "./realized_library.csv") %>% 
  select(c("date","rv5")) %>% 
  mutate(date = ymd(date))

df_date_to_forecast = data.frame(date = date_to_forecast) %>% 
  merge(realized_library, by = "date")

## --- Forecasts on the test set ---

h = 1 # one day forecast

### ---- GARCH-MIDAS models part ------
GM_models_list = c("GM_dhoust","GM_ip","GM_nai","GM_nfci","GM_Rvol22", "GM_vix","GM_vrp","GM_vix_dhoust", "GM_vix_ip", "GM_vix_nai", "GM_vix_nfci")


error_container <- list()
for (model in GM_models_list) {
  error_container[[model]] <- double(h)
}


Rprof(interval = 0.05)

for(i in seq_along(date_to_forecast)){
  print(date_to_forecast[[i]])
  
  filter_data(ymd(date_to_forecast[[i]])) # data available until the previous day of forecast (because we forecast at horizon 1 at the moment)
  
  real_volatility = df_date_to_forecast$rv5[[i]] * 10**4 #beacause we have multiplied the returns by 10**2 #as.Date(current_date)
  
  for(model in GM_models_list){
    
    # recover of the explanatory variables for the current model & forecast
    var_names = strsplit(model, "_", fixed = TRUE)[[1]]
    
    if(length(var_names)==2){ # 1 explanatory variable
      new_forecast = real_time_optimal_forecast(get(model),h, df_main_index, df_long_term1 = get(paste0("df_",var_names[[2]])))
      
    }else if(length(var_names)==3){# 2 explanatory variables
      new_forecast = real_time_optimal_forecast(get(model),h, df_main_index, df_long_term1 = get(paste0("df_",var_names[[3]])), df_long_term2 = get(paste0("df_",var_names[[2]])))
      
    }else{
      print(paste0("Model name : ", model))
      stop("The model name is not in the correct form.")
    }
    
    # error computation
    error_container[[model]][[i]] = qlike(new_forecast$forecast[[1]], real_volatility)
  }
  
  #df_error = data.frame(date = date_to_forecast, GM_dhoust = error_dhoust)
}

Rprof(NULL)





realized_library$rv5 * 10**4 %>% tail()


