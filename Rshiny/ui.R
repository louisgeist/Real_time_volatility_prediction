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
          "Date of data download :",
          value = Sys.Date(),
          min = "2023-05-01",
          max = Sys.Date(),
          format = "yyyy-mm-dd",
          daysofweekdisabled = c(0,6),
          weekstart = 1
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
                           choices = c("GM_Rvol22","GM_vix","GM_vrp","GM_nfci","GM_dhoust", "GM_ip", "GM_nai","GM_vix_dhoust","GM_vix_nai","GM_vix_nfci","GM_vix_ip","GARCH11"),
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
      plotlyOutput("plot_training_data"),
      plotlyOutput("spx_plot")
      )
  ),
  
  h1("Models evaluation"),
    
  p("The following parameters were used:",
    tags$ul(
      tags$li("Main index : ", uiOutput("params")),
      tags$li("Parameter 2: Value 2"),
      tags$li("Parameter 3: Value 3")
    )
  ),
  
  fluidRow(
    column(width = 6, h1("QLIKE mean error"), tableOutput("error_array")),
    column(width = 6, h1("Minimum mean error"), tableOutput("min_array"))
  )

)
