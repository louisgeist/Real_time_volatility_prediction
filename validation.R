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

# ----- 0.Tool ------ 
filter_data = function(first_date, last_date, df_main_index_raw, df_dhoust_raw, df_ip_raw, df_nai_raw, df_nfci_raw, df_vix_raw, df_vrp_raw, df_Rvol22_raw) {
  # Find the index where the conditions are true
  index_main_index <- which(df_main_index_raw$date >= first_date & df_main_index_raw$date < last_date)
  index_dhoust <- which(floor_date(df_dhoust_raw$date, unit = "month") >= floor_date(first_date, unit = "month") & floor_date(df_dhoust_raw$date, unit = "month") < floor_date(last_date, unit = "month"))
  index_ip <- which(floor_date(df_ip_raw$date, unit = "month") >= floor_date(first_date, unit = "month") & floor_date(df_ip_raw$date, unit = "month") < floor_date(last_date, unit = "month"))
  index_nai <- which(floor_date(df_nai_raw$date, unit = "month") >= floor_date(first_date, unit = "month") & floor_date(df_nai_raw$date, unit = "month") < floor_date(last_date, unit = "month"))
  index_nfci <- which(floor_date(df_nfci_raw$date, unit = "week", 5) >= floor_date(first_date, unit = "week", 5) & floor_date(df_nfci_raw$date, unit = "week", 5) < floor_date(last_date, unit = "week", 5))
  index_vix <- which(df_vix_raw$date >= first_date & df_vix_raw$date < last_date)
  index_vrp <- which(df_vrp_raw$date >= first_date & df_vrp_raw$date < last_date)
  index_Rvol22 <- which(df_Rvol22_raw$date >= first_date & df_Rvol22_raw$date < last_date)
  
  # Subset the DataFrames using the index
  df_main_index <<- df_main_index_raw[index_main_index, ]
  df_dhoust <<- df_dhoust_raw[index_dhoust, ]
  df_ip <<- df_ip_raw[index_ip, ]
  df_nai <<- df_nai_raw[index_nai, ]
  df_nfci <<- df_nfci_raw[index_nfci, ]
  df_vix <<- df_vix_raw[index_vix, ]
  df_vrp <<- df_vrp_raw[index_vrp, ]
  df_Rvol22 <<- df_Rvol22_raw[index_Rvol22, ]
}

qlike <- function(h,sigma_square){
  return(sigma_square/h -log( sigma_square / h) - 1)
}


# ----- 1. Data import -----
final_date = ymd("2019-01-01") #for download
source("./data_import.R")

# stable storage of the data
df_main_index_raw <- df_main_index

df_dhoust_raw = df_dhoust
df_ip_raw = df_ip
df_nai_raw = df_nai
df_nfci_raw = df_nfci
df_vix_raw = df_vix
df_vrp_raw = df_vrp
df_Rvol22_raw = df_Rvol22


# ------ 2. Training ------
date_begin_training = ymd("1971-01-05") # first day of availibity of S&P, (NDX is available later)
date_end_training = ymd("2015-01-01")

filter_data(first_date = date_begin_training, last_date = date_end_training, df_main_index_raw, df_dhoust_raw, df_ip_raw, df_nai_raw, df_nfci_raw, df_vix_raw, df_vrp_raw, df_Rvol22_raw)


source("./estimation.R")

# ------ 3. Forecasts & evaluation------
source("./forecast.R")

n_forecasts = 100 #number of days for the test set
date_to_forecast = seq_quotation_date(date_end_training, n_forecasts-1) # function implemented in forecast.R

## --- Forecasts on the test set ---
h_list = c(1,2,5,10,22,44,66) # NB : adding prediction horizons increases the following calculations only slightly
h_max = max(h_list)

## --- Test set ---
realized_library = read.csv(file = "./realized_library.csv") %>% 
  select(c("date","rv5")) %>% 
  mutate(date = ymd(date))


date_to_forecast_plus_h = seq_quotation_date(date_end_training, n_forecasts+h_max+1)
df_date_to_forecast = data.frame(date = date_to_forecast_plus_h) %>%  #(date = date_to_forecast) but now with h>1, we need to add some more dates
  merge(realized_library, by = "date")


### ---- GARCH-MIDAS models part ------
#GM_models_list = c("GM_dhoust","GM_ip","GM_nai","GM_nfci","GM_Rvol22", "GM_vix","GM_vrp","GM_vix_dhoust", "GM_vix_ip", "GM_vix_nai", "GM_vix_nfci") #GM_Rvol_22, "GM_vix_nfci" -> temporarly removed because K=264 makes forecasting really two slow
GM_models_list =  c("GM_dhoust","GM_ip","GM_nai","GM_nfci", "GM_vix","GM_vrp","GM_vix_dhoust", "GM_vix_ip", "GM_vix_nai")

# build of error_array
n_models <- length(GM_models_list)
n_dates <- length(date_to_forecast)

error_array <- array(0, dim = c(n_models, n_dates, length(h_list)))

for (i in seq_along(GM_models_list)) {
  for (date in 1:n_dates) {
    error_array[i, date, ] <- double(length(h_list))
  }
}


Rprof(interval = 0.05)

first_date_for_forecasts = date_end_training - years(1) #needs to be old enough so that enough lags are included


filter_data(first_date = first_date_for_forecasts, last_date = final_date, df_main_index_raw, df_dhoust_raw, df_ip_raw, df_nai_raw, df_nfci_raw, df_vix_raw, df_vrp_raw, df_Rvol22_raw)

# WATCH OUT : if needed, old data needs to be reimported
df_main_index_raw <- df_main_index
df_dhoust_raw <- df_dhoust
df_ip_raw <- df_ip
df_nai_raw <- df_nai
df_nfci_raw <- df_nfci
df_vix_raw <- df_vix
df_vrp_raw <- df_vrp
df_Rvol22_raw <- df_Rvol22
# WATCH OUT

for(i in seq_along(date_to_forecast)){
  print(date_to_forecast[[i]])
  
  filter_data(first_date = first_date_for_forecasts, last_date = ymd(date_to_forecast[[i]]),
              df_main_index_raw, df_dhoust_raw, df_ip_raw, df_nai_raw, df_nfci_raw, df_vix_raw, df_vrp_raw, df_Rvol22_raw
              ) #data available until the previous day of forecast (because we forecast at horizon 1 at the moment)
  
  real_volatility <- df_date_to_forecast$rv5[i:(i+h_max-1)] * 10**4 #because we have multiplied the returns by 10**2 #as.Date(current_date)
  
  for(model_index in seq_along(GM_models_list)){
    model = GM_models_list[[model_index]]
    
    # recover of the explanatory variables for the current model & forecast
    var_names = strsplit(model, "_", fixed = TRUE)[[1]]
    
    if(length(var_names)==2){ # 1 explanatory variable
      new_forecast = real_time_optimal_forecast(get(model),h_max, df_main_index, df_long_term1 = get(paste0("df_",var_names[[2]])))
      
    }else if(length(var_names)==3){# 2 explanatory variables
      new_forecast = real_time_optimal_forecast(get(model),h_max, df_main_index, df_long_term1 = get(paste0("df_",var_names[[3]])), df_long_term2 = get(paste0("df_",var_names[[2]])))
      
    }else{
      print(paste0("Model name : ", model))
      stop("The model name is not in the correct form.")
    }
    
    # error computation
    forecast_list <- new_forecast$forecast[h_list]
    real_volatility_list <- real_volatility[h_list]
    
    error_array[model_index, i, ] <- mapply(qlike, forecast_list, real_volatility_list)
    
  }
}

Rprof(NULL)

summaryRprof("Rprof.out")

saveRDS(error_array, file = "./data_error_array/error_array.rds")

# ---- 4. use of results ---
source(file = "./qlike.error_analysis.R")

error_array_analysis(error_array)



