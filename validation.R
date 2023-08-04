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
library(pracma)

# General paramaters (for the data download & estimation)
main_index = "spx"

# ----- Parameters -----
GM_models_list = c("GM_dhoust","GM_ip","GM_nai","GM_nfci","GM_Rvol22", "GM_vix","GM_vrp","GM_vix_dhoust", "GM_vix_ip", "GM_vix_nai", "GM_vix_nfci") # remark : even if you remove models here, they will still be estimated (but not use of forecasts)

h_list = c(1, 2, 5, 10, 22, 44, 66) # NB : adding prediction horizons increases the following calculations only slightly
n_forecasts = 400 #number of days for the test set

date_begin_training = ymd("1991-01-05") # first day of availability of S&P, (NDX is available later) #ymd("1971-01-05")
date_end_training = ymd("2017-01-01")

cum_evaluation = TRUE # if TRUE, QLIKE on the cumulative forecasts, if FALSE, QLIKE on k-step forecasts

# ----- 0.Tools : loss function ------
qlike <- function(h, sigma_square) {
  return(sigma_square / h - log(sigma_square / h) - 1)
}


# ----- 1. Data import -----
final_date = ymd("2019-01-01") #for download, used as a global variable in "data_import.R"
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

# date_begin/end_traing now defined in the "parameters" section
# date_begin_training = ymd("1991-01-05") # first day of availability of S&P, (NDX is available later) #ymd("1971-01-05")
# date_end_training = ymd("2015-01-01")

filter_data(
  first_date = date_begin_training,
  last_date = date_end_training,
  df_main_index_raw,
  df_dhoust_raw,
  df_ip_raw,
  df_nai_raw,
  df_nfci_raw,
  df_vix_raw,
  df_vrp_raw,
  df_Rvol22_raw
)

source("./estimation.R")

# ------ 3. Forecasts & evaluation------
source("./forecast.R")
source("./forecast_boosted.R")

# n_forecasts now defined in the "parameters" section
#n_forecasts = 500 #number of days for the test set
date_to_forecast = seq_quotation_date(date_end_training, n_forecasts - 1) # function implemented in forecast.R

## --- Redownload of the data on the specific window for forecasts
first_date_for_forecasts = date_end_training - years(1) #needs to be old enough so that enough lags are included

filter_data(
  first_date = first_date_for_forecasts,
  last_date = final_date,
  df_main_index_raw,
  df_dhoust_raw,
  df_ip_raw,
  df_nai_raw,
  df_nfci_raw,
  df_vix_raw,
  df_vrp_raw,
  df_Rvol22_raw
)

## --- Forecasts on the test set ---
# h_list now defined in the "parameters" section
# h_list = c(1, 2, 5, 10, 22, 44, 66) # NB : adding prediction horizons increases the following calculations only slightly
h_max = max(h_list)

## --- Test set ---
realized_library = read.csv(file = "./realized_library.csv") %>%
  select(c("date", "rv5")) %>%
  mutate(date = ymd(date))


date_to_forecast_plus_h = seq_quotation_date(date_end_training, n_forecasts +
                                               h_max + 4)
df_date_to_forecast = data.frame(date = date_to_forecast_plus_h) %>%  #(date = date_to_forecast) but now with h>1, we need to add some more dates
  merge(realized_library, by = "date")


### ---- GARCH-MIDAS models part ------
# GM_models_list now defined in the "parameters" section
#GM_models_list = c("GM_dhoust","GM_ip","GM_nai","GM_nfci","GM_Rvol22", "GM_vix","GM_vrp","GM_vix_dhoust", "GM_vix_ip", "GM_vix_nai", "GM_vix_nfci") 
#GM_models_list =  c("GM_dhoust","GM_ip","GM_nai","GM_nfci", "GM_vix","GM_vrp","GM_vix_dhoust", "GM_vix_ip", "GM_vix_nai") #GM_Rvol_22, "GM_vix_nfci" -> temporarly removed because K=264 makes forecasting really two slow

# build of error_array
n_models <- length(GM_models_list)
n_dates <- length(date_to_forecast)

error_array <- array(0, dim = c(n_models, n_dates, length(h_list)))

for (i in seq_along(GM_models_list)) {
  for (date in 1:n_dates) {
    error_array[i, date,] <- double(length(h_list))
  }
}

# real values array
df_real_volatility = realized_library %>% filter(date >= (date_end_training - days(1)))

real_volatility_array <- array(0, dim = c(n_dates, length(h_list)))

for(date_index in 1:n_dates){
  real_volatility_array[date_index,] <- df_real_volatility$rv5[date_index + h_list] * 10 ** 4
}


# forecasts array
Rprof(interval = 0.05)
for (model_index in seq_along(GM_models_list)) {
  model = GM_models_list[[model_index]]
  
  # recover of the explanatory variables for the current model & forecast
  var_names = strsplit(model, "_", fixed = TRUE)[[1]]
  
  if (length(var_names) == 2) {
    # 1 explanatory variable
    new_forecast = boosted_forecast(model_index,
                                    h_list,
                                    n_forecasts,
                                    df_epsilon,
                                    df_long_term1 = get(paste0("df_", var_names[[2]])))
    
    
  } else if (length(var_names) == 3) {
    # 2 explanatory variables
    new_forecast = boosted_forecast(
      model_index,
      h_list,
      n_forecasts,
      df_epsilon,
      df_long_term1 = get(paste0("df_", var_names[[3]])),
      df_long_term2 = get(paste0("df_", var_names[[2]]))
    )
    
  } else{
    print(paste0("Model name : ", model))
    stop("The model name is not in the correct form.")
  }
  
  if(cum_evaluation == TRUE){
    res_forecast_array = cumsum_on_forecast_array(new_forecast, h_list)
  }
  else{
    res_forecast_array = new_forecast[,h_list]
  }
  
  # error computations
  real_volatility <- df_date_to_forecast$rv5[i:(i + h_max - 1)] * 10 ** 4
  
  error_array[model_index, ,] = mapply(qlike, res_forecast_array, real_volatility_array) %>% 
    pracma::Reshape(n_forecasts, length(h_list))

}

# error computations

Rprof(NULL)

summaryRprof("Rprof.out")




saveRDS(error_array, file = "./data_error_array/error_array.rds")




# ----- 4. use of results -----
source(file = "./qlike.error_analysis.R")

error_array_analysis(error_array, GM_models_list, h_list)
