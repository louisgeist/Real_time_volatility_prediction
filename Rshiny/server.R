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


#-------- server logic --------
function(input, output, session) {
  
  ### To show all prediction models at this date
  # But it makes a reset of selected models each time we change the "origin_date"
  # observeEvent(input$origin_date, {
  #    
  #    #location_forecasts = paste0("../data_plot/",input$origin_date,"_forecasts.csv")
  #    #df = read.csv(file = location_forecasts) %>% mutate(date = ymd(date))
  #    
  #    available_models = names(df_forecasts())
  #    
  #    updateCheckboxGroupInput(session, "models", choices = available_models)
  #  })
  
  
  ### reactive dataframes 
  df_training_data = reactive({
    location_training_data = paste0("../data_plot/",input$origin_date,"_training_data.csv")
    
    df = read.csv(file = location_training_data) %>% mutate(date = ymd(date))
    return(df)
  })
  
  df_forecasts = reactive({
    location_forecasts = paste0("../data_plot/",input$origin_date,"_forecasts.csv")
    
    df = read.csv(file = location_forecasts) %>% mutate(date = ymd(date))
    
    return(df)
  })
  
  
  ### reactive plots
  output$plot <- renderPlotly({

    
    # adjustement according to the forecast horizon
    df_filtered <- df_forecasts()[1:input$horizon, ]
    
    # plot
    main_plot = plot_ly(df_filtered, x = ~date)
    
    color_palette = brewer.pal(length(input$models), "Set1")
    
    for(i in seq_along(input$models)){
      new_model = input$models[[i]]
      
      main_plot = main_plot %>% add_lines(y = as.formula(paste0("~", new_model)), name = new_model, line = list(color = color_palette[i]))
    }
    
    main_plot = main_plot %>%
      layout(title = "Graph",
             xaxis = list(title = "Date"),
             yaxis = list(title = "Volatility"))
    
    main_plot
  })


  output$plot_training_data <- renderPlotly({
    
    p = plot_ly(df_training_data(), x = ~date, y = ~get(input$explanatory_variable), type = 'scatter', mode = 'lines')
    
    p <- p %>% layout(title = "Explanatory variable (should be enough 'smooth')",
                      xaxis = list(title = "Date"),
                      yaxis = list(title = input$explanatory_variable))
    
    p
    
  })
}

