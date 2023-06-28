#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(RColorBrewer)

# ----- Data loading -------





#-------- server logic --------
function(input, output, session) {
    
      
    output$plot <- renderPlotly({
      
      # choose of the proper dataframe
      
      location_training_data = paste0("../data_plot/",input$origin_date,"_training_data.csv")
      location_forecasts = paste0("../data_plot/",input$origin_date,"_forecasts.csv")
      
      df_training_data = read.csv(file = location_training_data) %>% mutate(date = ymd(date))
      df_forecasts = read.csv(file = location_forecasts) %>% mutate(date = ymd(date))

      
      # adjustement according to the forecast horizon
      df_filtered <- df_forecasts[1:input$horizon, ]
      
      # plot
      main_plot = plot_ly(df_filtered, x = ~date)
      
      color_palette = brewer.pal(length(input$models), "Set1")
      
      for(i in seq_along(input$models)){
        new_model = input$models
        
        main_plot = main_plot %>% add_lines(y = as.formula(paste0("~", new_model)), name = new_model, line = list(color = color_palette[i]))
      }
      
      main_plot = main_plot %>%
        layout(title = "Graphique des prévisions",
               xaxis = list(title = "Date"),
               yaxis = list(title = "Volatility"))
      
      main_plot
    })
  

    output$table <- renderDataTable({
      faithful
    })
}

