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
      wellPanel(
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
          max = 80
        ),
        
        checkboxGroupInput("models", "Choose the displayed models :", 
                           choices = c("GM_Rvol22","GM_vix","GM_vrp","GM_nfci","GM_dhoust", "GM_ip", "GM_nai","GM_vix_dhoust","GM_vix_nai","GM_vix_nfci","GM_vix_ip"),
                           selected = c("GM_dhoust", "GM_vix", "GM_vix_dhoust"))
        ),
      wellPanel(
      
      
      selectInput("explanatory_variable",
                  "Explanatory variable :",
                  choices = c("Rvol22","vix","vrp","nfci","dhoust","ip","nai"),
                  selected = "dhoust")
      )
    ),
    
    ##### MAIN PANEL
    mainPanel(
      plotlyOutput("plot"), 
      plotlyOutput("plot_training_data")
      )
  )
)
