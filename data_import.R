library(tseries)
library(dplyr)
library(tidyr)
library(zoo)
library(lubridate)
library(alfred)

source(file = "./data_import_tools.R")

# NB : df_xx are standardized : one column is "date" and the other one is called "xx" and contains the stationary time series

#-------- S&P500 ---------

df_spx = import_spx()

#---- External variables ------


## ----- Daily measures of financial risk -----

### RVol(22)
df_Rvol22 = import_Rvol22()

### vix
df_vix = import_vix()

### VRP (variance risk premium)
df_vrp = import_vrp()


## ----- Non daily measures ----

### housing starts
df_dhoust = import_houst()

df_dhoust2 = import_houst()


#################### from this point onwards, the new data is not automatically fetched.

### industrial production (ip)
IP = read.csv("./data/IP_120623.csv")
partial_df_ip = IP %>% rename(c("date" = "DATE", "value" = "IP"))
df_ip = fill_missing_dates(partial_df_ip)


### nai
nai = read.csv("./data/nai_120623.csv", sep = ";") %>% 
  select(c("Date","CFNAI")) %>%
  mutate(date = ym(Date),value = as.numeric(sub(",", ".", CFNAI, fixed = TRUE))) %>% 
  select(-c("Date",CFNAI))

df_nai = fill_missing_dates(nai, frequency = "month")

ggplot(nai) + geom_line(aes(x = date, y = value))

### nfci
NFCI = read.csv("./data/nfci_120623.csv") %>%
  mutate(date = mdy(Friday_of_Week)) %>%
  select(c("date", "NFCI")) %>%
  rename(c("value" = "NFCI"))

df_NFCI = fill_missing_dates(NFCI, frequency = "week", week_start = 5)