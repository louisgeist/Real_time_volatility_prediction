sigma = 2

qlike = function(x){
  return(log(x**2)+sigma**2/x**2)
}

x_tab = seq(1,10,length.out = 1001)

y_tab = qlike(x_tab)

plot(x_tab,y_tab,type="l")
