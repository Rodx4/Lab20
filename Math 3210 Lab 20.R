library(shiny)
library(leaflet)
library(dplyr)
library(sf)
library(tidyverse)

# Define UI
ui <- fluidPage(
  titlePanel("Geographic Map Generator"),
  
  sidebarLayout(
    sidebarPanel(
      fileInput("file1", "Upload Data", accept = c(".csv", ".xlsx")),
      selectInput("locationType", "Select Geographic Level", 
                  choices = c("County", "State", "Country")),
      actionButton("generate_map", "Generate Map")
    ),
    
    mainPanel(
      leafletOutput("map")
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  
  data <- reactiveVal(NULL)  # to store uploaded data
  mapData <- reactiveVal(NULL)  # to store map data
  
  observeEvent(input$file1, {
    req(input$file1)
    ext <- tools::file_ext(input$file1$name)
    
    if (ext == "csv") {
      data(read.csv(input$file1$datapath))
    } else if (ext == "xlsx") {
      data(readxl::read_excel(input$file1$datapath))
    }
  })
  
  observeEvent(input$generate_map, {
    req(data())
    locType <- input$locationType
    
    # Check if the data contains the necessary columns
    location_col <- switch(locType,
                           "County" = "County",    # Adjust as per your dataset
                           "State" = "State",      # Adjust as per your dataset
                           "Country" = "Country"   # Adjust as per your dataset
    )
    
    if (location_col %in% colnames(data())) {
      
      # Clean data, filter relevant geographic columns
      geo_data <- data() %>%
        select(!!sym(location_col)) %>%
        distinct()
      
      # Check if the selected geographic data exists
      if (locType == "County") {
        # Load a shapefile for counties (modify as per your location)
        mapData(read_sf("path_to_shapefile_counties.shp"))
      } else if (locType == "State") {
        mapData(read_sf("path_to_shapefile_states.shp"))
      } else if (locType == "Country") {
        mapData(read_sf("path_to_shapefile_countries.shp"))
      }
      
      # Create leaflet map
      output$map <- renderLeaflet({
        leaflet(mapData()) %>%
          addProviderTiles(providers$CartoDB.Positron) %>%
          addPolygons()
      })
      
    } else {
      showModal(modalDialog(
        title = "Error",
        paste("Geographic data for", locType, "not found in your dataset."),
        easyClose = TRUE
      ))
    }
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
