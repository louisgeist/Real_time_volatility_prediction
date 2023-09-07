library(ggplot2)
library(tseries)
library(zoo)
library(plotly)
library(lubridate)
library(tidyr)
rm(list=ls())

t <- list(size = 14)

# ----- I. DATA IMPORT -----
final_date = ymd("2023-07-01")
main_index = "spx"

## all
source(file = "./data_import.R",local = TRUE)
training_data = read.csv("./data_daily_forecast/spx/training_data.csv")

## spx
spx.raw <-
  get.hist.quote(
    instrument = "^GSPC",
    start = as.Date("1971-01-01"),
    end = final_date + days(1),
    quote = "Close"
  )

spx.ret = 100 * diff(log(spx.raw)) #stationary times series

## HOUST
df_HOUST = alfred::get_alfred_series(
  series_id = "HOUST",
  series_name = "HOUST",
  observation_start = "1959-01-01",
  realtime_start = final_date,
  realtime_end = final_date,
  api_key = "4f77313cfd688a6d4d70ccf8e650f038"
)

df_dhoust = data.frame(date = df_HOUST$date)
df_dhoust$value =  c(NA, 100 * diff(log(df_HOUST$HOUST)))

## IP
df_ip_raw = alfred::get_alfred_series(
  series_id = "INDPRO",
  series_name = "IP",
  observation_start = "1959-01-01",
  realtime_start = final_date,
  realtime_end = final_date,
  api_key = "4f77313cfd688a6d4d70ccf8e650f038"
) %>% as_tibble() %>% select(c("date", "IP"))

df_ip = data.frame(date = df_ip_raw$date)
df_ip$value = c(NA, 100 * diff(log(df_ip_raw$IP)))

## ------- First plots ----------

plot(spx.raw)
plot(spx.ret)

## -------- Stationary test ---------
# Remark : 
# - the series we are fitting should be stationary
# - the long-term component should also be stationary

Qtests <- function(series, k, fitdf=0) { #réalise le test de Ljung-Box pour les k premiers horizons horizons la série "series" mise en argument
  pvals <- apply(matrix(1:k), 1, FUN=function(l) {
    pval <- if (l<=fitdf) NA else Box.test(series, lag=l, type="Ljung-Box", fitdf=fitdf)$p.value
    return(c("lag"=l,"pval"=pval))
  })
  return(t(pvals))
}
exogeneisation_residus = function (series,specification){
  lag_max = 36
  for(lag in 0:lag_max){
    my_adf = fUnitRoots::adfTest(series, lags = lag, type = specification)
    tab_p_val_autocorr = Qtests(my_adf@test$lm$residuals, 24, fitdf = length(my_adf@test$lm$coefficients))
    
    non_rejet = c((tab_p_val_autocorr[,2]> 0.05) | is.na(tab_p_val_autocorr[,2]))
    if(sum(non_rejet)==24){ #teste si tous les tests de Ljung-Box sont soit non rejetés, soit n'ont pas été réalisés car le lag était trop faible pour interprétation 
      return(lag)
    }
  }
  return(paste0("Presence of  d'autocorrélation trouvé jusqu'à l'ajout du lag = ",lag_max))
}

lag_spx = exogeneisation_residus(training_data$spx,"nc")
fUnitRoots::adfTest(training_data$spx, lag = lag_spx, type="nc") #p-value lower than 0.01 : we reject that the series got an unit root


### ------- House startings ----------
# Non diff series
plot_ly(data = df_HOUST) %>% add_lines(x = ~date, y = ~HOUST) %>% 
  layout(
    font = list(size = 24),
    xaxis = list(title = NA),
    yaxis = list(title = NA)
    
  ) %>% 
  config(toImageButtonOptions = list(format = "png", width = 750, height = 500))

lag_houst = exogeneisation_residus(df_HOUST$HOUST, "c")
fUnitRoots::adfTest(df_HOUST$HOUST, lag = lag_houst, type ="c")
# HOUST est stationnaire (selon ADF), mais on prend quand même la différence ! 

kpss.test(df_HOUST$HOUST, null = c("Level")) 

# dhoust
plot_ly(data = df_dhoust) %>% add_lines(x = ~date, y = ~value) %>% 
  layout(
    font = list(size = 24),
    xaxis = list(title = NA),
    yaxis = list(title = NA)
    
  ) %>% 
  config(toImageButtonOptions = list(format = "png", width = 750, height = 500))

lag_dhoust = exogeneisation_residus(df_dhoust$value, "nc")
fUnitRoots::adfTest(df_dhoust$value, lag = lag_dhoust, type ="nc") #p-value lower than 0.01 -> stationary

kpss.test(df_dhoust$value, null = c("Level")) 
### --------- Industrial production ---------

plot_ly(data = df_ip_raw) %>% add_lines(x = ~date, y = ~IP) %>% 
  layout(
    font = list(size = 24),
    xaxis = list(title = NA),
    yaxis = list(title = NA)
    
  ) %>% 
  config(toImageButtonOptions = list(format = "png", width = 750, height = 500))


lag_ip_raw = exogeneisation_residus(df_ip_raw$IP, "c")
fUnitRoots::adfTest(df_ip_raw$IP, lag = lag_ip_raw, type ="c") # p value = 0.33 -> we cannot reject UR

kpss.test(df_HOUST$HOUST, null = c("Level"))

# differenciated
plot_ly(data = df_ip) %>% add_lines(x = ~date, y = ~value) %>% 
  layout(
    font = list(size = 24),
    xaxis = list(title = NA),
    yaxis = list(title = NA)
    
  ) %>% 
  config(toImageButtonOptions = list(format = "png", width = 750, height = 500))

lag_ip = exogeneisation_residus(df_ip$value, "nc")
fUnitRoots::adfTest(df_ip$value, lag = lag_ip, type ="nc") # smaller than 0.01 -> stationnary


### ------- Focus on VIX -----------
lag_vix = exogeneisation_residus(training_data$vix,"c")
fUnitRoots::adfTest(training_data$vix, lag = lag_vix, type = "c") #p_value lower than 0.01 -> vix series is stationary

# quickly : all the other assets
stationnarity_adf <- function(time_series){
  lag = exogeneisation_residus(time_series,"c")
  test = fUnitRoots::adfTest(training_data$vix, lag = lag, type = "c")
  
  return(test)
}

df_spx$value %>%stationnarity_adf()

df_ndx$value %>% stationnarity_adf()

df_vrp$value %>% stationnarity_adf()

df_Rvol22$value %>% stationnarity_adf()

df_nai_partial = alfred::get_alfred_series(
  series_id = "CFNAI",
  series_name = "value",
  observation_start = "1959-01-01",
  realtime_start = final_date,
  realtime_end = final_date,
  api_key = "4f77313cfd688a6d4d70ccf8e650f038"
) %>% as_tibble() %>% select(c("date", "value"))

df_nai_partial$value %>% stationnarity_adf()

df_nfci_partial = alfred::get_alfred_series(
  series_id = "NFCI",
  series_name = "value",
  observation_start = "1959-01-01",
  realtime_start = final_date,
  realtime_end = final_date,
  api_key = "4f77313cfd688a6d4d70ccf8e650f038"
) %>% as_tibble() %>% select(c("value", "date"))

df_nfci_partial$value %>% stationnarity_adf()
