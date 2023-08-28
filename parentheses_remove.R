# remove of  the parentheses of the model name (GARCH(1,1)), because it causes problems with Rshiny

list_date = seq_quotation_date(ymd("2023-04-01"), 100)
list_date <- as.character(list_date)

for(date in list_date){
  x = readRDS(file = paste0("../data_daily_forecast/", date,"_forecast.rds"))
  x$models[[length(x$models)]] <- "GARCH11"
  
  saveRDS(x, file = paste0("../data_daily_forecast/",date,"_forecast.rds"))
}


