#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#
library(shiny)
library(shinythemes)
library(tidyverse)
library(lubridate)


end <- ymd("2022-08-12")
start <- ymd("2015-05-19")

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  
  titlePanel("Stream VS Air Temperature Data"),
  
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      selectInput("gage", "Select a stream location",
                  choices = c(unique(dataset$Location)), 
                  selected = dataset$Location[1]),
      dateRangeInput("dates",
                     "Run analysis for these dates",
                     start = start, end = end,
                     min = start, max = end)
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      tabsetPanel(type = "tabs", 
                  tabPanel("Introduction", 
                           h4("Welcome to our app...")),
                  tabPanel("Data Visualization"), 
                  tabPanel("Map View"), 
                  tabPanel("Filtered Data")
      )
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$distPlot <- renderPlot({
    # generate bins based on input$bins from ui.R
    x    <- faithful[, 2]
    bins <- seq(min(x), max(x), length.out = input$bins + 1)
    
    # draw the histogram with the specified number of bins
    hist(x, breaks = bins, col = 'darkgray', border = 'white',
         xlab = 'Waiting time to next eruption (in mins)',
         main = 'Histogram of waiting times')
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
