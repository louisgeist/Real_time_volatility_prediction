library(ggplot2)

sigma = 1

qlike = function(x){
  return(log(x)+sigma**2/x-1)
}

forecast = seq(0.25, 10,length.out = 10001)

loss = qlike(forecast)

plot(forecast,loss,type="l")
