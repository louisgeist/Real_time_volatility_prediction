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

# README : The working directory must be "Real_time_volatility_prediction"

# ----- Parameters -----
main_index = "spx" # WATCH OUT : at the moment, ndx cannot be used, because we don't have the real values of ndx volatility
GM_models_list = c("GM_dhoust","GM_ip","GM_nai","GM_nfci","GM_Rvol22", "GM_vix","GM_vrp","GM_vix_dhoust", "GM_vix_ip", "GM_vix_nai", "GM_vix_nfci") # remark : even if you remove models here, they will still be estimated (but not use of forecasts)

h_list = c(1, 2, 5, 10, 22, 44, 66) # NB : adding prediction horizons increases the following calculations only slightly
n_forecasts = 250 #number of days for the test set

date_begin_training = ymd("1991-01-05") # first day of availability of S&P, (NDX is available later) #ymd("1971-01-05")
date_end_training = ymd("2014-01-01")

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


### ---- GARCH-MIDAS models part ------
# GM_models_list now defined in the "parameters" section
#GM_models_list = c("GM_dhoust","GM_ip","GM_nai","GM_nfci","GM_Rvol22", "GM_vix","GM_vrp","GM_vix_dhoust", "GM_vix_ip", "GM_vix_nai", "GM_vix_nfci") 
#GM_models_list =  c("GM_dhoust","GM_ip","GM_nai","GM_nfci", "GM_vix","GM_vrp","GM_vix_dhoust", "GM_vix_ip", "GM_vix_nai") #GM_Rvol_22, "GM_vix_nfci" -> temporarly removed because K=264 makes forecasting really two slow

## i) build of error_array
n_models <- length(GM_models_list)
n_dates <- length(date_to_forecast)

error_array <- array(0, dim = c(n_models, n_dates, length(h_list)))

for (i in seq_along(GM_models_list)) {
  for (date in 1:n_dates) {
    error_array[i, date,] <- double(length(h_list))
  }
}

## ii) real values array
realized_library = read.csv(file = "./realized_library.csv") %>%
  select(c("date", "rv5")) %>%
  mutate(date = ymd(date))

df_real_volatility = realized_library %>% filter(date >= (date_end_training - days(1)))

real_volatility_array <- array(0, dim = c(n_dates, h_max))

for(date_index in 1:n_dates){
  real_volatility_array[date_index,] <- df_real_volatility$rv5[(date_index+1): (date_index+h_max)] * 10 ** 4
}

if(cum_evaluation==TRUE){
  real_volatility_array = cumsum_on_forecast_array(real_volatility_array, h_list)
}


## iii) forecasts array
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
                                    df_main_index,
                                    df_long_term1 = get(paste0("df_", var_names[[2]])),
                                    data_last_date = date_end_training)
    
    
  } else if (length(var_names) == 3) {
    # 2 explanatory variables
    new_forecast = boosted_forecast(
      model_index,
      h_list,
      n_forecasts,
      df_main_index,
      df_long_term1 = get(paste0("df_", var_names[[3]])),
      df_long_term2 = get(paste0("df_", var_names[[2]])),
      data_last_date = date_end_training
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
  error_array[model_index, ,] = mapply(qlike, res_forecast_array, real_volatility_array) %>% 
    pracma::Reshape(n_forecasts, length(h_list))
  
}

Rprof(NULL)

summaryRprof("Rprof.out")


### ---- GARCH(1,1) part ------
forecast_garch11  = ugarchforecast(GARCH11, 
                                   n.ahead = h_max
                                   )@forecast$sigmaFor

# manual computation
df_epsilon = df_main_index %>% filter(date >= date_end_training) # new data

omega <- GARCH11@fit$coef[[1]]
alpha <- GARCH11@fit$coef[[2]]
beta <- GARCH11@fit$coef[[3]]


### a) compute of all the sigma^2_t+1
last_epsilon = GARCH11@fit$residuals[[length(GARCH11@fit$residuals)]]
last_sigma2 = GARCH11@fit$sigma[[length(GARCH11@fit$sigma)]] **2

list_sigma2 = double(length(date_to_forecast))

for(date_index in seq_along(date_to_forecast)){ 
  list_sigma2[[date_index]] = omega + alpha * last_epsilon**2 + beta * last_sigma2
  
  # update of "last" variables
  last_sigma2 = list_sigma2[[date_index]]
  last_epsilon = df_epsilon[[2]][[date_index]]
  
}

### b) compute of the predictions at any horizon
garch11_forecast <- function(sigma_tplus1, k, omega, alpha, beta){
  return(omega * (1 - (alpha+beta)^(k-1))/(1-alpha-beta) + (alpha+beta)^(k-1) * sigma_tplus1)
}


forecast_garch11_array <- array(0, dim = c(n_dates, length(h_list)))

for(date in 1:n_dates) {
  list_forecast = adply(1:h_max, .margins = c(1), .fun = function(h) garch11_forecast(list_sigma2[[date]], h, omega, alpha, beta))[[2]]
  
  if(cum_evaluation==TRUE){
    forecast_garch11_array[date,] <- cumsum(list_forecast)[h_list]
  }else{
    forecast_garch11_array[date,] <-list_forecast[h_list]
  }
  
}

error_garch11 = mapply(qlike, forecast_garch11_array, real_volatility_array) %>% 
  pracma::Reshape(n_forecasts, length(h_list))

# add to error_array
error_array = abind::abind(error_array, array(error_garch11, dim = c(1,dim(error_garch11))), along = 1)

### ---- constant forecast ------

forecast_constant_array <- array(0, dim = c(n_dates, length(h_list)))

# ----- 4. use of results -----

x = list(error_array = error_array,
         models = c(GM_models_list,"GARCH(1,1)"),
         h_list = h_list,
         main_index = main_index,
         n_forecasts = n_forecasts,
         date_begin_training = date_begin_training,
         date_end_training = date_end_training,
         cum_evaluation = cum_evaluation
         )

saveRDS(x, file = paste0("./data_error_array/error_array",length(list.files("./data_error_array/"))+1,".rds"))

source(file = "./qlike.error_analysis.R")
error_array_analysis(x$error_array, x$models, x$h_list)
