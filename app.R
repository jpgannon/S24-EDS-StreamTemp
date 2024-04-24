
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

source("Filtering.R")
dataset <- read_csv("combinedData.csv")
bindedRows <- read_csv("bindedRows.csv")

# Define UI for application
ui <- fluidPage(
  theme = shinytheme("superhero"),
  # Application title
  titlePanel("Stream VS Air Temperature Data"),
  
  # Sidebar with a slider input for number of bins  sidebarLayout(
  sidebarPanel(
    selectizeInput(
      "gage",
      "Select Stations",
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
    p(strong("Disclaimer", style = "font-size: 18px;"),br(),
      span("To conduct analysis please use:", style = "font-size: 16px;"), 
      br(),
      "STA_1 with Streamtemp_W1 - Streamtemp_W6",
      br(),
      "STA_23 with Streamtemp_W7 - Streamtemp_W9",
    )
  ),
  
  #Define tabs and add descriptions
  mainPanel(
    tabsetPanel(
      type = "tabs",
      tabPanel(
        "Introduction",
        h4("Welcome to our app!"),
        p("Determining stream-hillslope connectivity provides an important context for understanding watershed biogeochemical fluxes and aquatic biologic resilience, especially for cold water fishes. In addition to other hillslope hydrologic methods, diel-paired stream and air temperature signals can inform spatial-temporal patterns of stream-hillslope connectivity. This new method utilizes temperature data, which is widely available and accessible, to calculate three simple daily metrics: amplitude ratio, phase lag and mean ratio. At the most basic level, the method relies on determining how the daily coupling of stream temperature and air temperature change over the year to inform the relative strength of groundwater-contributed waters (poorly coupled) to run-off and through-flow dominated (better coupled) times of the year. Notably, as the stream temperature is a non-conservative tracer, this method allows for the isolation of hydrologic dynamics at the km scale rather than an accumulative effect from upgradient processes." ),
        p("Created by: Robert Powell, Patrick Walsh, Nil Barua, Haley Crowder in collaboration with Dr. Hare")
      ),
      tabPanel("Raw Time Series", plotlyOutput("distPlot") %>% withSpinner(color = "blue"),
               p("Raw Time Series: This tab consists of one time series line graph. The graph visualizes the air temperature of STA_1 and STA_23 and stream temperature of 9 different locations. Since this is a time series graph as well, you can also manipulate the timeline to your own liking. This is very useful to compare and contrast the temperature from different locations. It can also visualize the effect of air temperature on stream temp on a timely basis.")
      ),
      tabPanel(
        "Tabular Data",
        p("Tabular Data: This tab has the three metrics (Signal Mean, Signal Amplitude, Signal Phase Offset) in a dataset format. Signal Mean is calculated by the mean stream temperature divided by the mean air temperature. Signal Amplitude is calculated by the mean of the stream temperature divided by the air temperature. Signal Phase Offset is calculated by the mean of the stream temperature minus the air temperature."
        ),
        dataTableOutput("signalAnalysis"),
        downloadButton("downloadData", "Download Data")
        
      ),
      tabPanel(
        "Compare Metrics - Graphs",
        p("Compare Metrics - Graphs: This tab displays the same information as the Tabular Data but in a graphical format per metric. For the Signal Mean graph the temperatures have been converted to Kelvin before dividing to eliminate negative air temperature values. This graph also has been filtered to only display values < 1.05 to remove anomalous values."
        ),
        plotlyOutput("signalMeanPlot") %>% withSpinner(color = "blue"),
        plotlyOutput("signalAmplitudePlot") %>% withSpinner(color = "blue"),
        plotlyOutput("signalPhaseOffsetPlot") %>% withSpinner(color = "blue"),
        dataTableOutput("compareMetricsTable")
      ),
      tabPanel(
        "Compare Metrics - Boxplots",
        p("Compare Metrics - Box Plots: This tab displays the same information as the tabular data and graphs tab but in boxplot format. This helps to analyze trends."),
        plotlyOutput("signalMeanBoxPlot") %>% withSpinner(color = "blue"),
        plotlyOutput("signalAmplitudeBoxPlot") %>% withSpinner(color = "blue"),
        plotlyOutput("signalPhaseOffsetBoxPlot") %>% withSpinner(color = "blue")
      ),
      tabPanel("Map View",
               leafletOutput("Map") %>% withSpinner(color = "blue"),
               p("Map View: This interactive tab displays the point location of each stream temperature sensor and its corresponding watershed boundary within Hubbard Brook."
               )),
      tabPanel(
        "Filtered Data",
        p("Filtered Data: This tab displays all of the data we have utilized in all the other tabs."
        ),
        dataTableOutput("data"),
        downloadButton("downloadFiltredData", "Download Data")),
      tabPanel("Upload Data", 
               p("Click the button below to download the data file.
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


# Define server logic
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
  
  output$note_text <- renderText({
    paste("[ STA (1 & 23) = Air Temp Station ]", "[ Streamtemp_W (1 - 9) = Water Temp Station]", sep = "\n")
  })
  
  
  output$distPlot <-renderPlotly({
    
    
    #Output for raw time series tab
    filtered_bind_data <- getTimeData() %>%
      filter(STA %in% input$gage,
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
            line = list(width = 1.5, color = 'orange'),
            opacity = 0.6
          )
      } else {
        
        plot1 <- plot1 %>%
          add_trace(
            data = location_data,
            x = ~DateTime,
            y = ~Temp,
            name = paste('Stream Temp (ºC) -', location),
            mode = 'lines',
            line = list(dash = 'dot', width = 1.5)
          )
      }
    }
    
    plot_1 <- plot1 %>%
      layout(title = "Temperature Line Graph",
             margin = list(
               l = 75,
               r = 75,
               b = 75,
               t = 75
             ),
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
    
    # Signal Mean in Kelvin
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
  # Signal Mean Box Plot
  output$signalMeanBoxPlot <- renderPlotly({
    filtered_data <- dataset %>%
      filter(Location %in% input$gage,
             TIMESTAMP >= input$dates[1],
             TIMESTAMP <= input$dates[2]) %>%
      mutate(Date = as.Date(TIMESTAMP))
    
    # Calculate Signal Mean in Kelvin and filter
    metrics_data <- filtered_data %>%
      group_by(Location, Date) %>%
      summarize(Signal_Mean = mean((StreamTemp) + 273.15) / mean((AirTemp) + 273.15),
                .groups = 'drop') %>%
      filter(Signal_Mean < 1.05)
    
    # Plot setup
    plot <- plot_ly()
    
    # Track locations to manage legend entries
    shown_legend <- c()
    
    for (loc in unique(metrics_data$Location)) {
      loc_data <- filter(metrics_data, Location == loc)
      plot <- add_trace(plot,
                        data = loc_data,
                        y = ~Signal_Mean,
                        x = ~Location,
                        type = 'box',
                        name = loc,
                        boxpoints = 'all',
                        showlegend = !loc %in% shown_legend)
      if (!loc %in% shown_legend) {
        shown_legend <- c(shown_legend, loc)  # Update shown locations
      }
    }
    
    plot %>%
      layout(title = "Signal Mean Box Plot",
             margin = list(l = 75, r = 75, b = 75, t = 75),
             yaxis = list(title = "Signal Mean (Kelvin)"),
             xaxis = list(title = "Location"))
  })
  
  # Signal Amplitude Box Plot
  output$signalAmplitudeBoxPlot <- renderPlotly({
    filtered_data <- dataset %>%
      filter(Location %in% input$gage,
             TIMESTAMP >= input$dates[1],
             TIMESTAMP <= input$dates[2]) %>%
      mutate(Date = as.Date(TIMESTAMP))
    
    metrics_data <- filtered_data %>%
      group_by(Location, Date) %>%
      summarize(Signal_Amplitude = mean(StreamTemp / AirTemp),
                .groups = 'drop') 
    
    # Plot setup
    plot <- plot_ly()
    
    # Track locations to manage legend entries
    shown_legend <- c()
    
    for (loc in unique(metrics_data$Location)) {
      loc_data <- filter(metrics_data, Location == loc)
      plot <- add_trace(plot,
                        data = loc_data,
                        y = ~Signal_Amplitude,
                        x = ~Location,
                        type = 'box',
                        name = loc,
                        boxpoints = 'all',
                        showlegend = !loc %in% shown_legend)
      if (!loc %in% shown_legend) {
        shown_legend <- c(shown_legend, loc)  # Update shown locations
      }
    }
    
    plot %>%
      layout(title = "Signal Amplitude Box Plot",
             margin = list(l = 75, r = 75, b = 75, t = 75),
             yaxis = list(title = "Signal Amplitude"),
             xaxis = list(title = "Location"))
  })
  
  # Signal Phase Offset Box Plot
  output$signalPhaseOffsetBoxPlot <- renderPlotly({
    filtered_data <- dataset %>%
      filter(Location %in% input$gage,
             TIMESTAMP >= input$dates[1],
             TIMESTAMP <= input$dates[2]) %>%
      mutate(Date = as.Date(TIMESTAMP))
    
    metrics_data <- filtered_data %>%
      group_by(Location, Date) %>%
      summarize(Signal_Phase_Offset = mean(StreamTemp - AirTemp),
                .groups = 'drop')
    
    # Plot setup
    plot <- plot_ly()
    
    # Track locations to manage legend entries
    shown_legend <- c()
    
    for (loc in unique(metrics_data$Location)) {
      loc_data <- filter(metrics_data, Location == loc)
      plot <- add_trace(plot,
                        data = loc_data,
                        y = ~Signal_Phase_Offset,
                        x = ~Location,
                        type = 'box',
                        name = loc,
                        boxpoints = 'all',
                        showlegend = !loc %in% shown_legend)
      if (!loc %in% shown_legend) {
        shown_legend <- c(shown_legend, loc)  # Update shown locations
      }
    }
    
    plot %>%
      layout(title = "Signal Phase Offset Box Plot",
             margin = list(l = 75, r = 75, b = 75, t = 75),
             yaxis = list(title = "Signal Phase Offset"),
             xaxis = list(title = "Location"))
  })
  
  
  
  #Output for map view tab
  #Downloading data 
  watershed <- st_read("hbef_weirs.shp")
  watersheds <- read_excel("watersheds.xlsx", sheet = 1)
  AirStation <- read_excel("AirStationLocation.xlsx",sheet = 1)
  watershed_boundary <- st_read("hbef_wsheds.shp") %>% 
    filter(WS %in% c('WS1','WS2','WS3','WS4','WS5','WS6','WS7','WS8','WS9')) %>%
    st_transform(4326)
  
  #Main Map Info
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
                       fillColor = 'blue',
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
      addCircleMarkers(data = AirStation, 
                       lat = ~LAT, 
                       lng = ~LONG,
                       radius = 5,
                       color = "yellow",
                       opacity = 0.8,
                       fill = T,
                       fillColor = 'orange',
                       fillOpacity = 0.7,
                       label = c(     '\nA1
                            \nSensor Elevation: 488 meters
                            \nAspect: S',
                                      '\nA23
                            \nSensor Elevation 683 meters:
                            \nAspect: NNE')) %>%
      addLabelOnlyMarkers(data = AirStation, 
                          lng = ~LONG, 
                          lat = ~LAT, 
                          label = 
                            c('A1',
                              'A23'),
                          labelOptions = labelOptions(noHide = T,textOnly = T))%>%
      
      
      setView(lat = mean(watersheds$LAT), 
              lng = mean(watersheds$LONG),
              zoom = 13.25)
  })
  
  #Click on Map Marker
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

# Run the application
shinyApp(ui = ui, server = server)



