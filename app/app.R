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
library(sf)
library(tidyverse)
library(readxl)
library(plotly)
library(leaflet)
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
                  tabPanel("Data Visualization",plotlyOutput("distPlot")), 
                  tabPanel("Map View",
                      leafletOutput("Map")),
                  tabPanel("Filtered Data", 
                           dataTableOutput("data"))
      )
    )
  )
)



# Define server logic required to draw a histogram
server <- function(input, output) {
  output$distPlot <- renderPlotly({
    
    # generate bins based on input$bins from ui.R)
    
    filtered_data <- dataset %>%
      filter(Location == input$gage,
             TIMESTAMP >= input$dates[1],
             TIMESTAMP <= input$dates[2])
    plot1 <- filtered_data %>%
      plot_ly(x = ~ TIMESTAMP)
    plot1 <- plot1 %>%
      add_trace(
        y = ~ StreamTemp,
        name = 'Stream Temp (ºC)',
        mode = 'lines',
        line = list(size = 4)
      )
    plot1 <- plot1 %>%
      add_trace(
        y = ~ AirTemp,
        name = 'Air Temp(ºC)',
        mode = 'lines',
        opacity = 0.7,
        line = list(width = 2)
      )
    plot_1 <- plot1 %>%
      layout(yaxis = list(title = "Temperature (Cº)"))
  })
  output$data <-
    renderDataTable(dataset, options = list(orderClasses = TRUE,
                                            searchDelay = 500))
  

# Map View Tab Astetics
  #Import Lat Long Data 
  watershed <- st_read("hbef_weirs.shp")
  watersheds <- read_excel("watersheds.xlsx", sheet = 1)
  
output$Map <- renderLeaflet({
  leaflet() %>%
    addProviderTiles("OpenTopoMap", options = providerTileOptions(noWrap = TRUE)) %>%
    addMarkers(data = watersheds, 
               lat = ~LAT, 
               lng = ~LONG,) %>%
    setView(lat = mean(watersheds$LAT), 
            lng = mean(watersheds$LONG),
            zoom = 13.45)
}) 

#Select sites by clicking on them
observeEvent(watersheds$Map_marker_click, {
site <- watersheds$Map_marker_click
siteID <- site$Location
updateSelectInput(session, "gage", selected = siteID)
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
