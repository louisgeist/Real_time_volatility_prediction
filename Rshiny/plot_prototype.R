library(ggplot2)
library(tidyverse)
library(plotly)
library(RColorBrewer)

# ----- Data loading -------
df_training_data = read.csv(file = "../data_plot/2023-06-26_training_data.csv") %>% mutate(date = ymd(date))
df_forecasts = read.csv(file = "../data_plot/2023-06-26_forecasts.csv") %>% mutate(date = ymd(date))

#----- plot ----- 
df_filtered = df_forecasts
models = c("GM_dhoust","GM_nai","GM_ip")

main_plot = plot_ly(df_filtered, x = ~date)

color_palette = brewer.pal(3, "Set1")

for(i in seq_along(models)){
  new_model = input$models
  
  main_plot = main_plot %>% add_lines(y = as.formula(paste0("~", new_model)), name = new_model, line = list(color = color_palette[i]))
}
