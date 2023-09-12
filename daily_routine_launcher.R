library(lubridate)
source("./forecast.R")

origin_date = today() - days(1)
t = seq_quotation_date(origin_date,0)

if(t == today()-days(1)){
  main_index = "spx"
  source("./daily_routine_v2.R")
  
  main_index = "ndx"
  source("./daily_routine_v2.R")
}
  
