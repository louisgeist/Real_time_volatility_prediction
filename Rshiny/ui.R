#
# user interface for real time volatility forecast
#

library(shiny)
library(plotly)

# Define UI for application that draws a histogram
fluidPage(
  titlePanel("Real time volatility forecast"),
  h1("Optimal volatility forecast with GARCH-MIDAS models"),
  
  ##### SIDE BAR LAYOUT
  sidebarLayout(
    sidebarPanel(
    
      dateInput(
        "origin_date",
        "Last day of data for forecasts :",
        value = Sys.Date(),
        min = "2023-06-26",
        max = Sys.Date(),
        format = "yyyy-mm-dd",
      ),
      
      
      # sliderInput : button to slide
      # numericInput : number to type
      sliderInput(
        "horizon",
        "Forecast horizon (in days):",
        value = 30,
        min = 1,
        max = 90
      ),
      
      checkboxGroupInput("models", "Choose the displayed models :", 
                         choices = c("GM_dhoust", "GM_ip", "GM_nai"),
                         selected = c("GM_dhoust", "GM_ip", "GM_nai"))
      

    ),
    
    ##### MAIN PANEL
    mainPanel(plotlyOutput("plot"))
  )
)
