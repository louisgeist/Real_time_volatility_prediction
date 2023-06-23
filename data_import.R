# WARNING : data_import_tools.R needs to be imported
source("./data_import_tools.R")

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

### industrial production (ip)
df_ip = import_ip()

### nai
df_nai = import_nai()

### nfci (weekly)
df_nfci = import_nfci()
