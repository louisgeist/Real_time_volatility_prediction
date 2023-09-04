library(lubridate)
library(tidyr)
source("./forecast.R")

# ---- Parameters ----
main_index = "spx"

initial_date = ymd("2023-05-01")
last_date = today() - days(1)

# ---- Script -----
n = (seq_quotation_date(initial_date = initial_date, h = 100) <= last_date) %>% sum()

list_origin_date = seq_quotation_date(initial_date = initial_date, h = n-1)

for(date in list_origin_date){
  print(as_date(date))
  origin_date = as_date(date)
  source("./daily_routine_v2.R")
}
