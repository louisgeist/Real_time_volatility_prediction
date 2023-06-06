library(forecast)
library(tseries)
library(rugarch)
library(zoo)
library(fUnitRoots)

data.raw <- get.hist.quote(instrument = "^GSPC", start=as.Date("1971-01-01"), end=as.Date("2023-12-31"), quote="Close")

plot(data.raw)

data.ret = 100*diff(log(data.raw))
plot(data.ret)

###----- Import of explanatory variables ------
# this part needs to be rewritten to import properly the ^vix.csv 
library(openxlsx)
library(dplyr)
library(lubridate)

vix.raw = read.xlsx("./vix.xlsx")
vix.raw = select(vix.raw, "X2","X3")
vix.raw = rename(vix.raw,c("Date"="X2","Price"="X3"))
vix.raw = slice(vix.raw,4:1961)


as_datetime(45502)

plot(vix.raw)


###----- fit of normal GARCH ---------

spec_garch = ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1, 1)), mean.model = list(include.mean=FALSE), distribution.model = "norm")
model = ugarchfit(spec_garch, data.ret)
model

plot(model@fit[["sigma"]], type="l", ylab = "Sigma")

p_max = 5
q_max = 5

marix_res = matrix(NA, nrow = p_max, ncol = q_max)
rownames(marix_res) <- paste0("p=", 1:p_max)
colnames(marix_res) <- paste0("p=", 1:q_max)

aic_matrix = matrix_res
bic_matrix = matrix_res

for(p in 1:p_max){
  for(q in 1:q_max){
    spec_garch = ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(p, q)), mean.model = list(include.mean=FALSE), distribution.model = "norm")
    model = ugarchfit(spec_garch, data.ret)
  }
}

mean(data.ret$Close[1000:])
