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

# ----- Parameters -----
origin_date = today() - days(1) # so that at any hour of the day n+1, if we run this script, we got all the data from the date n

main_index = "spx" # "spx" or "ndx" are the main index which are avaible
GM_models_list = c("GM_dhoust","GM_ip","GM_nai","GM_nfci","GM_Rvol22", "GM_vix","GM_vrp","GM_vix_dhoust", "GM_vix_ip", "GM_vix_nai", "GM_vix_nfci") # remark : even if you remove models here, they will still be estimated (but not use of forecasts)

h_list = 1:66 #c(1, 2, 5, 10, 22, 44, 66)
n_forecasts = 1

date_begin_training = ymd("1991-01-05")
date_end_training = origin_date

# ---- Test -----
if( wday(origin_date) %in% c(1,7)){ # saturday & sunday morning : no new data
  q()
}
   

# ----- 1. Data import -----
final_date = origin_date
source("./data_import.R")

# ------ 2. Training ------
source("./estimation.R")

# ------ 3. Forecasts ------
source("./forecast.R")
source("./forecast_boosted.R")

date_to_forecast = seq_quotation_date(date_end_training, n_forecasts - 1) # function implemented in forecast.R

h_max = max(h_list)

### ---- GARCH-MIDAS models part ------

n_models <- length(GM_models_list)
n_dates <- length(date_to_forecast)


## i) forecast array initialization
forecast_array <- array(0, dim = c(n_models, n_dates, length(h_list)))

for (i in seq_along(GM_models_list)){
  for (date in 1:n_dates) {
    forecast_array[i, date,] <- double(length(h_list))
  }
}

## ii) forecast computation
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
      df_long_term2 = get(paste0("df_", var_names[[2]]),
      estimation_last_date = date_end_training)
    )
    
  } else{
    print(paste0("Model name : ", model))
    stop("The model name is not in the correct form.")
  }
  
  # only useful in validation.R
  #if(cum_evaluation == TRUE){
  #  res_forecast_array = cumsum_on_forecast_array(new_forecast, h_list)
  #}
  #else{
  #  res_forecast_array = new_forecast[,h_list]
  #}
  
  forecast_array[model_index, ,] <- new_forecast[,h_list]
  
}

### ---- GARCH(1,1) part -----
forecast_garch11  = ugarchforecast(GARCH11, 
                                   n.ahead = h_max
                                   )@forecast$sigmaFor**2 #in order to predict the sigma^2


forecast_array = abind::abind(forecast_array, array(forecast_garch11[h_list], dim = c(1,length(h_list))), along = 1)


# ----- 4. save of the results -----
## i) forecasts save
x = list(forecast_array = forecast_array,
         models = c(GM_models_list,"GARCH11"),
         h_list = h_list,
         main_index = main_index,
         origin_date = origin_date
)

if(main_index == "spx"){
  saveRDS(x, file = paste0("./data_daily_forecast/spx/",origin_date,"_forecast.rds"))
}else{
  saveRDS(x, file = paste0("./data_daily_forecast/ndx/",origin_date,"_forecast.rds"))
}


## ii) training data save
df_training_data = df_main_index

index_list = c("dhoust","ip","nai","nfci","Rvol22","vix","vrp")
for(index in index_list){
  new_df = get(paste0("df_",index)) %>% dplyr::rename(!!index := "value")
  
  df_training_data = df_training_data %>% left_join(new_df, by = "date")
}

df_training_data = df_training_data %>% select(c("date", main_index ,index_list)) # in order to remove the "year_month" or "year_week" variables


if(main_index == "spx"){
  write_csv(df_training_data, "./data_daily_forecast/spx/training_data.csv")
}else{
  write_csv(df_training_data, "./data_daily_forecast/ndx/training_data.csv")
}