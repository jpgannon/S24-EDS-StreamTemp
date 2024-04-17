# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/

library(rsconnect)
library(shiny)
library(shinythemes)
library(sf)
library(tidyverse)
library(readxl)
library(lubridate)
library(plotly)
library(leaflet)
library(bslib)
library(shinycssloaders)

end <- ymd("2022-09-12")
start <- ymd("2015-05-19")

options(shiny.maxRequestSize = 100 * 1024^2)

#source("Filtering.R")
dataset <- read_csv("combinedData.csv")
bindedRows <- read_csv("bindedRows.csv")

# Define UI for application that draws a histogram
ui <- fluidPage(
  theme = shinytheme("yeti"),
  # Application title
  titlePanel("Stream VS Air Temperature Data"),
  
  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      selectizeInput(
        "gage",
        "Select stream locations",
        choices = unique(bindedRows$STA),
        selected = bindedRows$STA[1],
        multiple = TRUE
      ),
      dateRangeInput(
        "dates",
        "Run analysis for these dates",
        start = start,
        end = end,
        min = start,
        max = Sys.Date()
      ),
    ),
    
    #Show a plot of the generated distribution
    mainPanel(
      tabsetPanel(
        type = "tabs",
        tabPanel(
          "Introduction",
          h4("Welcome to our app..."),
          p("This app's function is to display stream and air temperature data for different analyses.
                 The data is from the Hubbard Brook Experimental Forest. The hourly temperature data is from nine different stream locations and two different air locations."
          ),
          # p("A brief description of the research project this app is built for: Diel Paired Air-Stream Temperature Metrics Reveal Seasonal Patterns in Hydrologic Connectivity"
          # ),
          # p("A brief description of each tab:"),
          p("Raw Time Series: This tab consists of one time series line graph. The graph visualizes the air temperature of STA_1 and STA_23 and stream temperature of 9 different locations. Since this is a time series graph as well, you can also manipulate the timeline to your own liking. This is very useful to compare and contrast the temperature from different locations. It can also visualize the effect of air temperature on stream temp on a timely basis. 
"),
          p("Tabular Data: This tab has the three metrics (Signal Mean, Signal Amplitude, Signal Phase Offset) in a dataset format. Signal Mean is calculated by the mean stream temperature divided by the mean air temperature. Signal Amplitude is calculated by the mean of the stream temperature divided by the air temperature. Signal Phase Offset is calculated by the mean of the stream temperature minus the air temperature."
          ),
          p("Compare Metrics - Graphs: This tab displays the same information as the Tabular Data but in a graphical format per metric. For the Signal Mean graph the temperatures have been converted to Kelvin before dividing to eliminate negative air temperature values. This graph also has been filtered to only display values < 1.05 to remove anomalous values. For all three graphs a rolling mean of 7 days has been used."
          ),
          p("Compare Metrics - Box Plots:"),
          p("Map View: This interactive tab displays the point location of each stream temperature sensor and its corresponding watershed boundary within Hubbard Brook."
          ),
          p("Filtered Data: This tab displays all of the data we have utilized in all the other tabs."
          ),
          p("Upload Data: This tab allows the user to download a copy of our cleaned data file, and add their own data. The user can then upload the new data file, and it will automatically add the new data to the calculations.")
        ),
        tabPanel("Raw Time Series", plotlyOutput("distPlot") %>% withSpinner(color = "blue"), p("Dedicated Locations STA_1: Stream_w1 - Stream_w6, STA_23: Stream_w7 - Stream_w9")),
        tabPanel(
          "Tabular Data",
          dataTableOutput("signalAnalysis"),
          downloadButton("downloadData", "Download Data")
        ),
        tabPanel(
          "Compare Metrics - Graphs",
          plotlyOutput("signalMeanPlot") %>% withSpinner(color = "blue"),
          plotlyOutput("signalAmplitudePlot") %>% withSpinner(color = "blue"),
          plotlyOutput("signalPhaseOffsetPlot") %>% withSpinner(color = "blue"),
          dataTableOutput("compareMetricsTable")
        ),
        # tabPanel(
        #   "Compare Metrics - Boxplots",
        #   plotlyOutput("compareMetricsBoxplots") %>% withSpinner(color = "blue")
        # ),
        tabPanel("Map View",
                 leafletOutput("Map") %>% withSpinner(color = "blue")),
        tabPanel(
          "Filtered Data",
          dataTableOutput("data"),
          downloadButton("downloadFiltredData", "Download Data")),
        tabPanel("Upload Data", 
                 h4("Click the button below to download the data file.
                        Add your data to the bottom this sheet and then reupload as a CSV
                        using the upload button below.(Follow the format that is on the sheet)"),
                 downloadButton("downloadTimeTemplate","Download Data File for Time Series"),
                 downloadButton("downloadMetricsTemplate", "Download Data File for Calculating Metrics"),
                 fileInput("UserData", "Upload updated metric data by clicking browse below"),
                 fileInput("UserTimeData", "Upload updated time data by clicking browse below"),
                 accept = c("text/csv",
                            "text/comma-separated-values/text/plain",
                            ".csv"))
      )
    )
  )
)


# Define server logic required to draw a histogram
server <- function(input, output) {
  
  getMetricData <- reactive({
    if (is.null(input$UserData)) {
      return(dataset)
   } else {
      uploaded_data <- read_csv(input$UserData$datapath)
      return(uploaded_data)
    }
  })
  
  getTimeData <- reactive({
    if (is.null(input$UserTimeData)) {
      return(bindedRows)
    } else {
      updated_data <- read_csv(input$UserTimeData$datapath)
      return(updated_data)
    }
  })
  output$downloadMetricsTemplate <- downloadHandler(
    filename = function() {
      "inputData.csv"
    },
    content = function(file) {
      write_csv(getMetricData(), file)
    }
  )
  
  output$downloadTimeTemplate <- downloadHandler(
    filename = function() {
      "inputTimeData.csv"
    },
    content = function(file) {
      write_csv(getTimeData(), file)
    }
  )
  
  output$distPlot <-renderPlotly({
    
    #Output for raw time series tab
    #Generate bins based on input$bins from ui.R
    filtered_bind_data <- getTimeData() %>%
      filter(STA == input$gage,
             DateTime >= input$dates[1],
             DateTime <= input$dates[2])
    
    #Generates interactive line graph
    plot1 <- filtered_bind_data %>%
      plot_ly(x = ~ DateTime)
    
    air_temp_location <- c("STA_1", "STA_23")
    
    for (location in input$gage) {
      location_data <- filtered_bind_data %>%
        filter(STA == location)
      
      if(location %in% air_temp_location) {
        plot1 <- plot1 %>%
          add_trace(
            data = location_data,
            x = ~DateTime,
            y = ~Temp,
            name = paste('Air Temp (ºC) -', location),
            type = 'scatter',
            mode = 'lines',
            line = list(width = 1.5),
            opacity = 0.6
          )
      } else {
        
        plot1 <- plot1 %>%
          add_trace(
            data = location_data,
            x = ~DateTime,
            y = ~Temp,
            #name = paste('Air Temp (ºC) -', input$gage[1]),
            name = paste('Stream Temp (ºC) -', location),
            mode = 'lines',
            line = list(dash = 'dot', width = 1.5)
          )
      }
    }
    
    plot_1 <- plot1 %>%
      layout(title = "Temperature Line Graph",
             yaxis = list(title = "Temperature (Cº)"),
             xaxis = list(title = "Time Stamp (yearly)"))
    
  })
  #Reactive value for daily data
  daily_data <- reactive({
    filtered_data <- getMetricData() %>%
      filter(Location == input$gage,
             TIMESTAMP >= input$dates[1],
             TIMESTAMP <= input$dates[2]) %>%
      mutate(Date = as.Date(TIMESTAMP))
    
    daily_data <- filtered_data %>%
      group_by(Location, Date) %>%
      summarize(
        Signal_Mean = mean(StreamTemp) / mean(AirTemp),
        Signal_Amplitude = mean(StreamTemp / AirTemp),
        Signal_Phase_Offset = mean(StreamTemp - AirTemp)
      )
    
    return(daily_data)
  })
  
  #Output for tabular data tab
  output$signalAnalysis <- renderDataTable({
    daily_data()
  })
  
  #Download button for tabular data tab
  output$downloadData <- downloadHandler(
    filename = function() {
      "TabularData.csv"
    },
    content = function(file) {
      write.csv(daily_data(), file, row.names = FALSE)
    }
  )
  
  
  #Outputs for compare metrics - graphs tab
  
  #Signal Mean Plot
  output$signalMeanPlot <- renderPlotly({
    filtered_data <- getMetricData() %>%
      filter(Location %in% input$gage,
             TIMESTAMP >= input$dates[1],
             TIMESTAMP <= input$dates[2]) %>%
      mutate(Date = as.Date(TIMESTAMP))
    
    plot <- plot_ly()
    
    for (loc in input$gage) {
      daily_data <- filtered_data %>%
        filter(Location == loc) %>%
        group_by(Date) %>%
        summarize(Signal_Mean = mean((StreamTemp) + 273.15) / mean((AirTemp) + 273.15)) %>% 
        filter(Signal_Mean < 1.05)
      
      
      plot <- plot %>%
        add_trace(
          data = daily_data,
          x = ~ Date,
          y = ~ Signal_Mean,
          type = 'scatter',
          mode = 'lines',
          name = loc
        )
    }
    
    plot %>%
      layout(
        title = "Signal Mean",
        margin = list(
          l = 75,
          r = 75,
          b = 75,
          t = 75
        ),
        xaxis = list(title = "Date"),
        yaxis = list(title = "Signal Mean (K)")
      )
  })
  
  #Signal Amplitude Plot
  output$signalAmplitudePlot <- renderPlotly({
    filtered_data <- getMetricData() %>%
      filter(Location %in% input$gage,
             TIMESTAMP >= input$dates[1],
             TIMESTAMP <= input$dates[2]) %>%
      mutate(Date = as.Date(TIMESTAMP))
    
    plot <- plot_ly()
    
    for (loc in input$gage) {
      daily_data <- filtered_data %>%
        filter(Location == loc) %>%
        group_by(Date) %>%
        summarize(Signal_Amplitude = mean(StreamTemp / AirTemp))
      
      plot <- plot %>%
        add_trace(
          data = daily_data,
          x = ~ Date,
          y = ~ Signal_Amplitude,
          type = 'scatter',
          mode = 'lines',
          name = loc
        )
    }
    
    plot %>%
      layout(
        title = "Signal Amplitude",
        margin = list(
          l = 75,
          r = 75,
          b = 75,
          t = 75
        ),
        xaxis = list(title = "Date"),
        yaxis = list(title = "Signal Amplitude")
      )
  })
  
  #Signal Phase Offset Plot
  output$signalPhaseOffsetPlot <- renderPlotly({
    filtered_data <- getMetricData() %>%
      filter(Location %in% input$gage,
             TIMESTAMP >= input$dates[1],
             TIMESTAMP <= input$dates[2]) %>%
      mutate(Date = as.Date(TIMESTAMP))
    
    plot <- plot_ly()
    
    for (loc in input$gage) {
      daily_data <- filtered_data %>%
        filter(Location == loc) %>%
        group_by(Date) %>%
        summarize(Signal_Phase_Offset = mean(StreamTemp - AirTemp))
      
      plot <- plot %>%
        add_trace(
          data = daily_data,
          x = ~ Date,
          y = ~ Signal_Phase_Offset,
          type = 'scatter',
          mode = 'lines',
          name = loc
        )
    }
    
    plot %>%
      layout(
        title = "Signal Phase Offset",
        margin = list(
          l = 75,
          r = 75,
          b = 75,
          t = 75
        ),
        xaxis = list(title = "Date"),
        yaxis = list(title = "Signal Phase Offset")
      )
  })
  
  #Output for the compare metrics - box plots tab
  
  
  #Output for map view tab
  watershed <- st_read("hbef_weirs.shp")
  watersheds <- read_excel("watersheds.xlsx", sheet = 1)
  watershed_boundary <- st_read("hbef_wsheds.shp") %>% 
    filter(WS %in% c('WS1','WS2','WS3','WS4','WS5','WS6','WS7','WS8','WS9')) %>%
    st_transform(4326)
  
  output$Map <- renderLeaflet({
    leaflet() %>%
      addProviderTiles("OpenTopoMap", options = providerTileOptions(noWrap = TRUE)) %>%
      addPolygons(data = watershed_boundary) %>%
      addCircleMarkers(data = watersheds, 
                       lat = ~LAT, 
                       lng = ~LONG,
                       radius = 5,
                       color = "yellow",
                       opacity = 0.8,
                       fill = T,
                       fillColor = 'red',
                       fillOpacity = 0.7,
                       label = c('\nW1 
                            \nSensor Elevation: 498 meters,
                            \nAspect:SSE,
                            \nWatershed Area: 14.10 hectares',
                                 '\nW2 
                            \nSensor Elevation: 505 meters,
                            \nAspect:SSE,
                             \nWatershed Area: 17.93 hectares',
                                 '\nW3 
                            \nSensor Elevation: 540 meters,
                            \nAspect:SE,
                            \nWatershed Area: 40.26 hectares',
                                 '\nW4 
                            \nSensor Elevation: 460 meters,
                            \nAspect:SE,
                            \nWatershed Area: 38.84 hectares',
                                 '\nW5 
                            \nSensor Elevation: 505 meters,
                            \nAspect:SE,
                             \nWatershed Area: 20.79 hectares',
                                 '\nW6 
                            \nSensor Elevation: 560 meters,
                            \nAspect:SE,
                            \nWatershed Area: 14.97 hectares',
                                 '\nW7
                            \nSensor Elevation: 628 meters,
                            \nAspect:N,
                             \nWatershed Area: 76.63 hectares',
                                 '\nW8 
                            \nSensor Elevation: 613 meters,
                            \nAspect:NNW,
                             \nWatershed Area: 61.07 hectares',
                                 '\nW9 
                            \nSensor Elevation: 692 meters,
                            \nAspect:NNE,
                            \nWatershed Area: 70.29 hectares')) %>%
      addLabelOnlyMarkers(data = watersheds, 
                          lng = ~LONG, 
                          lat = ~LAT, 
                          label = 
                            c('W1', 
                              'W2', 
                              'W3', 
                              'W4', 
                              'W5', 
                              'W6', 
                              'W7', 
                              'W8', 
                              'W9'),
                          labelOptions = labelOptions(noHide = TRUE, textOnly = TRUE)) %>%
      
      setView(lat = mean(watersheds$LAT), 
              lng = mean(watersheds$LONG),
              zoom = 13.25)
  })
  
  #Select sites by clicking on them
  observeEvent(watersheds$Map_marker_click, {
    site <- watersheds$Map_marker_click
    siteID <- site$Location
    updateSelectInput(session, "gage", selected = siteID)
  })
  
  
  #Output for filtered data tab
  datasett <- reactive({
    filtered_data <- getMetricData() %>%
      filter(Location == input$gage,
             TIMESTAMP >= input$dates[1],
             TIMESTAMP <= input$dates[2]) %>%
      mutate(Date = as.Date(TIMESTAMP))
    
    datasett <- filtered_data %>%
      group_by(Location, Date)
    
    return(datasett)
  })
  
  output$data <- renderDataTable({
    datasett()
  })
  
  #Download button for filtered data tab
  output$downloadFilteredData <- downloadHandler(
    filename = function() {
      "FilteredData.csv"
    },
    content = function(file) {
      write.csv(datasett(), file, row.names = FALSE)
    }
  )
  
}

#Upload data tab



# Run the application
shinyApp(ui = ui, server = server)


