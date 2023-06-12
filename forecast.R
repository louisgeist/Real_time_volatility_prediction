### ----- Forecast with a GARCH-MIDAS ------
# what follows relies on the "mfGARCH" package

# Simulation of a GARCH-MIDAS. No hypothesis on the innovation's law : bootstrap.
simulate_garchmidas <- function(x, h){
  # x is a mfGARCH object obtained by fit_mfgarch
  # h is the horizon of simulation

  # Build of the boostrap set for the innovations
  df_spx = x$df.fitted %>% select(c("date","spx"))
  df_epsilon = df_spx %>% mutate(epsilon = spx - x$par[[1]]) %>% select(-c("spx"))
  
  df_resi_g_tau = x$df.fitted %>% select(c("date","g","tau","residuals")) %>% drop_na()
  
  next_g_func <- function(alpha,beta,gamma,epsilon,tau,g){
    return((1-alpha-gamma/2-beta) + (alpha + gamma * as.numeric(epsilon < 0))*(epsilon**2)/tau + beta*g)
  }
  
  alpha = x$par["alpha"][[1]]
  beta = x$par["beta"][[1]]
  gamma = x$par["gamma"][[1]]
  residuals_bootstrap_set = x$df.fitted$residuals  %>% as.list() %>% na.remove() # bootstrap set
  
  # list of items to be calculated | at the end of the loop, stored in a dataframe
  list_residuals = sample(x = residuals_bootstrap_set, size = h)
  list_tau = double(h) # can be completed without entering the loop | WARNING - WATCH OUT FOR NON DAILY LONG TERM COMPENENT
  
  list_g = double(h)
  list_epsilon = double(h)
  
  for(i in 1:h){
    list_tau[i] = x$tau.forecast # tau forecasts are taken constant, but tau is a predictable process. tau_{t+1} is stored in tau.forecast
    
    # previous : i-1
    # next (that we want to predict): i
    
    if(i == 1){
      previous_g = df_resi_g_tau$g[c(length(df_resi_g_tau$tau))]
      previous_epsilon = df_epsilon$epsilon[c(length(df_epsilon$epsilon))]
    }
    else{
      previous_g = list_g[i-1]
      previous_epsilon = list_epsilon[i-1]
    }
    
    list_g[i] = next_g_func(alpha, beta, gamma, epsilon = previous_epsilon, tau = list_tau[i], g = previous_g)
    list_epsilon[i] = list_residuals[i][[1]] * sqrt(list_g[i] * list_tau[i]) # WARNING : check the indexes
    
  }
  df_forecast = as.data.frame( cbind(list_epsilon, list_g, list_tau, list_residuals)) %>% rename(c("epsilon" = "list_epsilon",
                                                                                               "g" = "list_g", 
                                                                                               "tau" = "list_tau", 
                                                                                               "residuals" = "list_residuals"))
  return(df_forecast)
}

# test of the function
h = 40

res = simulate_garchmidas(GM_sp_vrp,h)

plot(1:h, res$epsilon, type= "l")
plot(1:h, res$residuals, type= "l")
plot(1:h, res$g, type= "l")

### lubridate tests
# my_date = dmy("10-06-2023")
# wday(my_date)

### Smoothness of tau ?
# plot = ggplot(data = df_resi_g_tau)+geom_line((aes(x = date, y = tau)))
# plot
