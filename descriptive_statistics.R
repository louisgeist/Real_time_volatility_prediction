library(ggplot2)
rm(list=ls())

source(file = "./data_import.R",local = TRUE)

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

lag_spx = exogeneisation_residus(spx.ret,"nc")
fUnitRoots::adfTest(spx.ret, lag = lag_spx, type="nc") #p-value lower than 0.01 : we reject that the series got an unit root


### ------- Focus on VIX -----------
lag_vix = exogeneisation_residus(vix.ret$Price,"c")

plot(vix.ret$Date, vix.ret$Price, type = "l")

fUnitRoots::adfTest(vix.ret$Price, lag = lag_vix, type = "nc") #p_value lower than 0.01 -> vix series is stationary

### ------- House startings ----------
# Non diff series

lag_houst = exogeneisation_residus(HOUST$HOUST, "c")
fUnitRoots::adfTest(HOUST$HOUST, lag = lag_houst, type ="c")

# HOUST est stationnaire, mais on prend quand même la différence ! (selon ADF)
kpss.test(HOUST$HOUST, null = c("Level"), lshort = TRUE) #p-value lower than 0.01
kpss.test(HOUST$HOUST, null = c("Level"), lshort = FALSE) #p-value 0.039

# dhoust
ggplot(df_dhoust) + geom_line(aes(x = date, y = dhoust))

lag_dhoust = exogeneisation_residus(df_dhoust$dhoust, "nc")
fUnitRoots::adfTest(df_dhoust$dhoust, lag = lag_houst, type ="nc") #p-value lower than 1 -> stationary
