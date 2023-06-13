library(tseries)
library(dplyr)
library(tidyr)
library(zoo)


#-----------------
# df_xx are standardized : one column is "date" and the other one is called "xx" and contains the stationary time series
#-----------------



###-------- S&P500 ---------
# Automatic import of S&P500

spx.raw <- get.hist.quote(instrument = "^GSPC", start=as.Date("1971-01-01"), quote="Close")
spx.ret = 100*diff(log(spx.raw)) #stationary times series

df_spx = fortify.zoo(spx.ret)
df_spx = rename(df_spx, c("date"="Index", "spx" = "Close"))


###---- External variables ------
# RV
#download.file("https://realized.oxford-man.ox.ac.uk/images/oxfordmanrealizedvolatilityindices.zip", destfile = "data-raw/OxfordManRealizedVolatilityIndices.zip")
#system("unzip -o data-raw/OxfordManRealizedVolatilityIndices.zip -d data-raw/")

# Daily measures of financial risk

### RVol(22)
Rvol22 = zoo::rollmean(spx.ret**2, 22, align = "right") #the current value and the past 21 values are taken for the mean
df_Rvol22 = Rvol22 %>% fortify.zoo() %>%  drop_na() %>% rename(c("date" = "Index", Rvol22= "Close"))


### vix
vix.raw = get.hist.quote(instrument = "^VIX", start = as.Date("1990-01-01"), quote="Close")
sum(is.na(vix.raw$Close)) #numbers of NA 1990 to 09/06/2023 : 299

df_vix = fortify.zoo(vix.raw)
df_vix = df_vix  %>% drop_na()
df_vix = mutate(df_vix,vix_dailyscaled = Close/252**(1/2)) # vix which has been converted to a daily level
df_vix = df_vix %>% rename(c("date" = "Index", "vix" = "vix_dailyscaled")) %>% select(c("date","vix"))


### VRP (variance risk premium)
vrp = vix.raw/ 252**(1/2) - Rvol22
df_vrp = vrp %>% fortify.zoo() %>% drop_na() %>% rename(c("date" = "Index", vrp = "Close"))


# library(alfred)
# nfci = get_alfred_series("NFCI")
# rv = get_alfred_series("RV")



# Non daily measures

#In order to use properly "fit_mfgarch", we need to complete between two non consecutive dates the dataframe with the value of the first date ; 
#fill_missing_dates() solves this problem
fill_missing_dates = function(partial_df){
  list_day = seq(ymd(partial_df$date[[1]]),today(),by = "days") # saturday and sunday are in excess, causing only a slower script
  list_value = double(length(list_day))
  
  i = 1 # index going through partial_df
  
  for(day_index in 1:length(list_day)){
    day_floor = floor_date(list_day[[day_index]],unit = "month")
    
    if(ymd(partial_df$date[[i]]) == day_floor){
      list_value[[day_index]] = partial_df$dhoust[[i]]
    }
    else{
      i = i + 1
      if(i == length(partial_df$dhoust)){
        break
      }
      if(ymd(partial_df$date[[i]]) == day_floor){
        list_value[[day_index]] = partial_df$dhoust[[i]]
      }
      else{print("error")}
    }
  }
  df = as.data.frame(cbind(list_day,list_value)) %>% mutate(date = as_date(list_day)) %>% select(-c(list_day))
  return(df)
}


# housing starts
HOUST = read.csv("./data/HOUST_1206.csv")
partial_df_dhoust = HOUST %>% rename(c("date" = "DATE","dhoust"="HOUST"))



t = fill_missing_dates(partial_df_dhoust)
t = t 
