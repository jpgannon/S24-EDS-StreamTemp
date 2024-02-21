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
  
  
  # Sidebar with a drop down menu for location and a selectable calendar for 
  # dates
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
                           h4("This app's function is to display stream and air 
                              temperature data for different analyses. The data 
                              is from the Hubbard Brook Experimental Forest. The
                              hourly temperature data is from nine different 
                              stream locations and two different air locations. ")),
                  tabPanel("Data Visualization", plotOutput("distPlot")),
                  tabPanel("Data Analysis", dataTableOutput("calc")),
                  tabPanel("Map View"),
                  tabPanel("Filtered Data",
                           dataTableOutput("data"))
      )
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$distPlot <- renderPlot({
    # generate bins based on input$bins from ui.R)
    filtered_data <- dataset %>%
      filter(Location == input$gage,
             TIMESTAMP >= input$dates[1],
             TIMESTAMP <= input$dates[2]
             
      )
    ggplot(filtered_data, aes(x = TIMESTAMP, y = StreamTemp, color = "StreamTemp")) +
      geom_line() +
      geom_line(aes(y = AirTemp, color = "AirTemp", alpha = 0.5)) +
      labs(x = "TimeStamp", y = "Temperature (Cº)") +
      scale_color_manual(values = c("StreamTemp" = "blue", "AirTemp" = "red")) +
      theme_minimal()
  })
  output$data <- renderDataTable(dataset, options = list(orderClasses = TRUE,
                                                         searchDelay = 500))
}


# Run the application
shinyApp(ui = ui, server = server)