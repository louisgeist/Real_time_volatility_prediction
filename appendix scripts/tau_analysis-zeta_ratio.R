# 

x = readRDS("./data_daily_forecast/spx/2023-08-15_GM_models.rds")

vix_dhoust = x$GM_vix_dhoust$optim$par
vix = x$GM_vix$optim$par
dhoust = x$GM_dhoust$optim$par

vix_dhoust
vix
dhoust

vix_ratio = vix_dhoust[["theta.two"]]/ vix[["theta"]]
dhoust_ratio = vix_dhoust[["theta"]]/ dhoust[["theta"]]

print(vix_ratio)
print(dhoust_ratio)
