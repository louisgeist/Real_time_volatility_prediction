### ----- Forecast with a GARCH-MIDAS ------
# what follows relies on the "mfGARCH" package
library(timeDate)

next_g_func <- function(alpha,beta,gamma,epsilon,tau,g){
  return((1-alpha-gamma/2-beta) + (alpha + gamma * as.numeric(epsilon < 0))*(epsilon**2)/tau + beta*g)
}

seq_quotation_date = function(initial_date,h){ #returns h+1 next quoted days after the initial date
  # initial_date : date
  # h : integer, for the horizon
  
  list_days = seq(initial_date, initial_date + 2*h+5, by = "day")
  
  unique_years = year(list_days) %>% as.data.frame() %>% rename(c("year" = "."))%>% distinct(year)
  holiday = c()
  for(y in unique_years$year){
    holiday = c(holiday, ymd(holidayNYSE(year = y)))
  }
  
  quoted_days = c()
  for(date in list_days){
    if((wday(as_date(date)) %in% seq(2,6))& (1 -(date %in% holiday)) ){
      quoted_days[c(length(quoted_days)+ 1)] = date
      
    }
  }
  
  quoted_days = quoted_days %>% as_date()
  return(quoted_days[c(seq(1:(h+1)))])
}

# Simulation of a GARCH-MIDAS. No hypothesis on the innovation's law : bootstrap
simulate_garchmidas <- function(x, h){
  # x is a mfGARCH object obtained by fit_mfgarch
  # h is the horizon of simulation

  # Build of the boostrap set for the innovations
  df_spx = x$df.fitted %>% select(c("date","spx"))
  df_epsilon = df_spx %>% mutate(epsilon = spx - x$par[[1]]) %>% select(-c("spx"))
  
  df_resi_g_tau = x$df.fitted %>% select(c("date","g","tau","residuals")) %>% drop_na()
  
  alpha = x$par["alpha"][[1]]
  beta = x$par["beta"][[1]]
  gamma = x$par["gamma"][[1]]
  residuals_bootstrap_set = x$df.fitted$residuals  %>% as.list() %>% na.remove() # bootstrap set
  
  # simulation of the RESIDUALS
  list_residuals = sample(x = residuals_bootstrap_set, size = h)
  
  
  # computation of TAU
  list_tau = rep(x$tau.forecast, times = h)
  
  # (this section of the code is written for long-term component that are avaible before the end of period : if tau_t is avaible before the end of the period t)
  # (but I think that this never occurs)
  # list_tau = double(h)
  # low.freq = names(x$df.fitted)[[3]]
  # 
  # quoted_days = seq_quotation_date(x$df.fitted$date[c(length(x$df.fitted$date))], h)[-c(1)]
  # current_tau = x$tau[[length(x$tau)]]
  # 
  # if(low.freq == "year_month"){
  #   current_lowfreq = x$df.fitted$year_month[[length(x$df.fitted$year_month)]]
  #   for(i in 1:h){
  #     if(floor_date(quoted_days[[i]], unit ="month") == current_lowfreq){
  #       list_tau[[i]] = current_tau
  #     }
  #     else{
  #       list_tau[[i]] = x$tau.forecast
  #     }
  #   }
  # }
  # else{
  #   if(low.freq == "year_week"){
  #     current_lowfreq = x$df.fitted$year_week[[length(x$df.fitted$year_week)]]
  #     for(i in 1:h){
  #       if(floor_date(quoted_days[[i]], unit ="week", week_start = x$week_start) == current_lowfreq){
  #         list_tau[[i]] = current_tau
  #       }
  #       else{
  #         list_tau[[i]] = x$tau.forecast
  #       }
  #     }
  #     
  #   }
  #   else{# means that it is a daily long term component
  #     list_tau = rep(x$tau.forecast, times = h)
  #   }
  # }
  
  # computation of EPSILON & G
  list_g = double(h)
  list_epsilon = double(h)
  
  for(i in 1:h){ 
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
  

  # date = c()
  # for(i in 0:h){
  #   th = as.character(quoted_days[i])
  #   print(th)
  #   date[i] = th
  # }
    
  df_forecast = as.data.frame( cbind( list_epsilon, list_g, list_tau, list_residuals)) %>% rename(c("epsilon" = "list_epsilon", #date, 
                                                                                               "g" = "list_g", 
                                                                                               "tau" = "list_tau", 
                                                                                               "residuals" = "list_residuals"))
  return(df_forecast)
}

# it is faster to use "optimal_forecast" and the results seem to be equivalent
bootstrap_forecast <- function(x,h){
  n = 1000
  res = 0
  
  for(i in 1:n){
    # res = res + simulate_garchmidas(x,h)$epsilon[[h]]**2 / simulate_garchmidas(x,h)$residuals[[h]]**2
    res = res + simulate_garchmidas(x,h)$tau[[h]] * simulate_garchmidas(x,h)$g[[h]]
  }
  
  return(res/n)
}

optimal_forecast <- function(x,h){
  # x is a mfGARCH object obtained by fit_mfgarch
  # h is the horizon of forecast
  
  alpha = x$par["alpha"][[1]]
  beta = x$par["beta"][[1]]
  gamma = x$par["gamma"][[1]]
  
  last_g = x$g[c(length(x$g))]
  last_epsilon = x$df.fitted$spx[[length(x$df.fitted$spx)]]
  last_tau = x$tau[[length(x$tau)]]
  
  next_g = next_g_func(alpha, beta, gamma, last_epsilon, last_tau, last_g) # g_1,t+1|t
  
  opt_forecast = x$tau.forecast*(1 + (alpha + gamma/2 + beta)**(h-1) * (next_g - 1))

  return(opt_forecast)
}

# test of the functions
h = 20

res = simulate_garchmidas(GM_dhoust, h)
res

plot(1:h, res$epsilon, type= "l")
plot(1:h, res$residuals, type= "l")
plot(1:h, res$g, type= "l")

simulate_garchmidas(GM_nfci, 20)

optimal_forecast(GM_nfci,100)

### Smoothness of tau ?
# plot = ggplot(data = df_resi_g_tau)+geom_line((aes(x = date, y = tau)))
# plot


