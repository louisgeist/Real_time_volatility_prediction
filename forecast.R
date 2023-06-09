### ----- Forecast with a GARCH-MIDAS ------
# what follows relies on the "mfGARCH" package

# simulation of a GARCH-MIDAS. No hypothesis on the innovation's law : bootstrap.

simulate_garchmidas <- function(x, h){
  # x is a mfGARCH object obtained by fit_mfgarch
  # h is the horizon of simulation

  # Build of the boostrap set for the innovations
  # innovations
  
  epsilon = x$df.fitted$spx
  
  return(epsilon)
}
  

summary(GM_sp_Rvol22$g)
summary(GM_sp_Rvol22$tau)
summary(GM_sp_Rvol22$par)

GM_sp_Rvol22$par


#simulate garchmidas function

x = GM_sp_Rvol22
df_spx = x$df.fitted %>% select(c("date","spx"))
df_epsilon = df_spx %>% mutate(epsilon = spx - x$par[[1]]) %>% select(-c("spx"))

