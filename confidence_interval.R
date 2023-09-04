source("../qlike.error_analysis.R")
source("../forecast.R")
source("../eikon_data_preprocessing.R")

library(plyr)
library(dplyr)
library(readxl)
library(lubridate)

# ----- Parameters -----
main_index = "ndx"
models <- c("GM_dhoust", "GM_ip", "GM_nai", "GM_nfci", "GM_Rvol22", "GM_vix", "GM_vrp", "GM_vix_dhoust", "GM_vix_ip", "GM_vix_nai", "GM_vix_nfci", "GARCH11")
h_list = 1:66
list_origin_date = seq_quotation_date(ymd("2023-05-01"), 69 - 10) # 68 because the 68th day is the last day of available data when I am writing the script
                                                                  # -10 to have enough date even for 10 days ahead

h_list_ic <- 1:10

if(main_index == "spx"){
  path_data_eikon = "../data_eikon/spx_29_08_23.xlsx"
}else{
  path_data_eikon = "../data_eikon/ndx_31_08_23.xlsx"
}


# ----- 1. data import -----
# eikon data
five_min_data = read_excel(path_data_eikon) %>% dplyr::rename("date" = "Local Date")
df_RV = compute_realized_volatility(five_min_data)

#----- 2. ratio array build ----
ratio_array <- array(0, dim = c(length(models),length(h_list_ic),length(list_origin_date)))

# eikon data
for(i in seq_along(models)){
  for(i_date in seq_along(list_origin_date)){
    x = df_RV %>% filter(date > list_origin_date[[i_date]])
    ratio_array[i,,i_date] <- x$RV[h_list_ic]
  }
}

# forecast data

for(i_date in seq_along(list_origin_date)){
  x = readRDS(paste0("../data_daily_forecast/",main_index,"/",list_origin_date[i_date],"_forecast.rds"))
  
  forecast_array = x$forecast_array %>% drop()
  forecast_array = forecast_array[,h_list_ic]
  
  ratio_array[,,i_date] <- ratio_array[,,i_date] / forecast_array # true / forecast ratio is now computed
  
}


sorted_ratio_array = aaply(ratio_array, c(1,2), .fun = sort)

#---- 3. Quantile computation ----
level = 0.9

n_lower_quantile = (1-level) / 2 *length(list_origin_date) %>% floor() #watch out that the round doesn't alter to much the level
n_upper_quantile = (1- (1-level) /2)* length(list_origin_date) %>% floor()


get_quantiles <- function(vector){
  return(c(vector[[n_lower_quantile]],vector[[n_upper_quantile]]))
}

quantile_array <- aaply(sorted_ratio_array, c(1,2), .fun = get_quantiles)

saveRDS(quantile_array, file = paste0("../data_daily_forecast/",main_index,"/quantile_array.rds"))
