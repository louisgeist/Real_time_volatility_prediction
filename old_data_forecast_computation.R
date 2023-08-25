source("./forecast.R")

initial_date = ymd("2023-05-01")
last_date = today()

n = (seq_quotation_date(initial_date = initial_date, h = 100) <= last_date) %>% sum()

list_origin_date = seq_quotation_date(initial_date = initial_date, h = n-1)

for(date in list_origin_date){
  print(date)
  origin_date = date
  source("./daily_routine_v2.R")
}
