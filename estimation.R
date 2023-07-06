# WARNING : 
# 1. data_import.R should be runned before running this script
# 2. main_index should be a string containing the name of a valid index

# ----- adaptation to the main chosen index
df_main_index = get(paste0("df_",main_index)) %>%
  dplyr::rename(!!main_index := "value")


# ----- fit of normal GARCH ---------
spec_garch = ugarchspec(
  variance.model = list(model = "sGARCH", garchOrder = c(1, 1)),
  mean.model = list(include.mean = TRUE, armaOrder = c(0,0)),
  distribution.model = "norm"
)
GARCH11 = ugarchfit(spec_garch, df_main_index[,2] %>% as.data.frame() )

# forecast_bootstrap = ugarchboot(GARCH11, method = c("Partial", "Full")[1], n.ahead = 30)

# results display
# model
# forecast_bootstrap

# plot(model@fit[["sigma"]], type = "l", ylab = "Sigma")


# ----- GARCH-MIDAS with one explanatory variable --------
## ----- Daily explanatory variables (VIX, Rvol22, vrp) -----

## s&p explained by vix
df = merge(df_main_index, df_vix, by = "date")
GM_vix = mfGARCH::fit_mfgarch(
  data = df,
  y = main_index,
  x = "value",
  K = 3,
  low.freq = "date",
  weighting = "beta.restricted"
)
# plot_weighting_scheme(GM_sp_vix)

## s&p explained by Rvol22
df = merge(df_main_index, df_Rvol22, by = "date")
GM_Rvol22 = mfGARCH::fit_mfgarch(
  data = df,
  y = main_index,
  x = "value",
  K = 264,
  low.freq = "date",
  weighting = "beta.restricted"
)

## s&p explained by vrp
df = merge(df_main_index, df_vrp, by = "date")
GM_vrp = mfGARCH::fit_mfgarch(
  data = df,
  y = main_index,
  x = "value",
  K = 3,
  low.freq = "date",
  weighting = "beta.restricted"
)


## ------ Non daily explanatory variable -------
### dhoust
df = df_main_index %>% merge(df_dhoust, by = "date")
GM_dhoust = mfGARCH::fit_mfgarch(
  data = df,
  y = main_index,
  x = "value",
  low.freq = "year_month",
  K =  36,
  weighting = "beta.unrestricted"
)

### nfci (weekly)
df = df_main_index %>% merge(df_nfci, by = "date")
GM_nfci = mfGARCH::fit_mfgarch(
  data = df,
  y = main_index,
  x = "value",
  low.freq = "year_week",
  K =  52,
  weighting = "beta.restricted"
)
GM_nfci$week_start <-
  5 # should be the same as indicated in data_import.R


### Industrial production
df = df_main_index %>% merge(df_ip, by = "date")
GM_ip = mfGARCH::fit_mfgarch(
  data = df,
  y = main_index,
  x = "value",
  low.freq = "year_month",
  K = 36,
  weighting = "beta.restricted"
)

### Chicago Fed National Activity Index (CFNAI)
df = df_main_index %>% merge(df_nai, by = "date")
GM_nai = mfGARCH::fit_mfgarch(
  data = df ,
  y = main_index,
  x = "value",
  low.freq  = "year_month",
  K = 36,
  weighting = "beta.restricted"
)



# ----- GARCH-MIDAS with two explanatory variables ------
# vix & dhoust
df = df_main_index %>%
  merge(df_vix, by = "date") %>% #value -> value.x
  merge(df_dhoust, by = "date") %>% #value -> value.y
  as_tibble()

GM_vix_dhoust = fit_mfgarch(
  data = df,
  y = main_index,
  x = "value.y",
  K = 36,
  low.freq = "date",
  var.ratio.freq = "year_month",
  weighting = "beta.unrestricted",
  
  x.two = "value.x",
  K.two = 3,
  low.freq.two = "date",
  weighting.two = "beta.restricted"
)

# vix & nfci
df = df_main_index %>%
  merge(df_vix, by = "date") %>% #value -> value.x
  merge(df_nfci, by = "date") %>% #value -> value.y
  as_tibble()

GM_vix_nfci = fit_mfgarch(
  data = df,
  y = main_index,
  x = "value.y",
  K = 52,
  low.freq = "date",
  var.ratio.freq = "year_week",
  weighting = "beta.unrestricted",
  
  x.two = "value.x",
  K.two = 3,
  low.freq.two = "date",
  weighting.two = "beta.restricted"
)

# vix & nai
df = df_main_index %>%
  merge(df_vix, by = "date") %>% #value -> value.x
  merge(df_nai, by = "date") %>% #value -> value.y
  as_tibble()

GM_vix_nai = fit_mfgarch(
  data = df,
  y = main_index,
  x = "value.y",
  K = 36,
  low.freq = "date",
  var.ratio.freq = "year_month",
  weighting = "beta.unrestricted",
  
  x.two = "value.x",
  K.two = 3,
  low.freq.two = "date",
  weighting.two = "beta.restricted"
)
# vix & ip
df = df_main_index %>%
  merge(df_vix, by = "date") %>% #value -> value.x
  merge(df_ip, by = "date") %>% #value -> value.y
  as_tibble()

GM_vix_ip = fit_mfgarch(
  data = df,
  y = main_index,
  x = "value.y",
  K = 36,
  low.freq = "date",
  var.ratio.freq = "year_month",
  weighting = "beta.unrestricted",
  
  x.two = "value.x",
  K.two = 3,
  low.freq.two = "date",
  weighting.two = "beta.restricted"
)
