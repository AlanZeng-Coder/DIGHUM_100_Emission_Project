# Title: Emission Visualization
# Description: Interactive visualization and analysis of Emission through country
# Author: Zien Zeng
# Date: 2024/6/4

# =======================================================
# Packages (you can use other packages if you want)
# =======================================================
library(shiny)
library(tidyverse)
library(lubridate)
library(sf)
library(leaflet)
library(plotly)
library(DT)
library(shinyjs)
library(rnaturalearth)  # for world map data

# =======================================================
# Import data
# =======================================================
emissions_data <- read.csv("emissions.csv")

# ===============================================
# Define "ui" for application
# ===============================================
ui <- fluidPage(
  useShinyjs(),  # Initialize shinyjs
  # Application title
  titlePanel("Emission Visualization"),
  
  # -------------------------------------------------------
  # Input widgets 
  # -------------------------------------------------------
  sidebarLayout(
    sidebarPanel(
      
      conditionalPanel(
        condition = "input.tabselected==1",
        h4("Year Analysis"),
        sliderInput("yearRangeMap", "Select Year Range:", 
                    min = min(emissions_data$Year), max = max(emissions_data$Year),
                    value = c(min(emissions_data$Year), min(emissions_data$Year) + 10))
      ),
    ),
    mainPanel(
      tabsetPanel(
        tabPanel(title = "Map",
                 value = 1,
                 leafletOutput("map", height = 1000)),
        id = "tabselected"
      )
    )
  )
)
# ===============================================
# Define server logic
# ===============================================
server <- function(input, output, session) {
  
  # Load world map data
  world <- ne_countries(scale = "medium", returnclass = "sf")
  
  # Fill missing years with zero emissions
  complete_data_map_or_table <- emissions_data %>%
    complete(Country, Year = full_seq(Year, 1), fill = list(
      Emissions.Type.CO2 = 0, Emissions.Type.N2O = 0, Emissions.Type.CH4 = 0, 
      Emissions.Sector.Power_Industry = 0, Emissions.Sector.Buildings = 0, 
      Emissions.Sector.Transport = 0, Emissions.Sector.Other_Industry = 0, 
      Emissions.Sector.Other_sectors = 0))
  
  # Ensure Year is of type character for consistent binding
  complete_data_map_or_table$Year <- as.character(complete_data_map_or_table$Year)
  
  # Render map with emission data
  output$map <- renderLeaflet({
    filtered_data_map <- complete_data_map_or_table %>%
      filter(Year >= input$yearRangeMap[1] & Year <= input$yearRangeMap[2]) %>%
      group_by(Country) %>%
      summarise(
        total_CO2 = sum(Emissions.Type.CO2, na.rm = TRUE),
        total_N2O = sum(Emissions.Type.N2O, na.rm = TRUE),
        total_CH4 = sum(Emissions.Type.CH4, na.rm = TRUE),
        total_emissions = sum(Emissions.Type.CO2, Emissions.Type.N2O, Emissions.Type.CH4, na.rm = TRUE)
      )
    
    # Merge with world map data
    world <- left_join(world, filtered_data_map, by = c("name" = "Country"))
    
    leaflet(world) %>%
      addProviderTiles("CartoDB.Positron") %>%
      addPolygons(
        fillColor = ~colorBin("YlOrRd", total_emissions, bins = 7)(total_emissions),
        weight = 1,
        opacity = 1,
        color = "black",
        dashArray = "3",
        fillOpacity = 0.7,
        highlightOptions = highlightOptions(
          weight = 5,
          color = "#666",
          dashArray = "",
          fillOpacity = 0.7,
          bringToFront = TRUE
        ),
        label = ~paste(
          "Country: ", name,
          "CO2 ", total_CO2,
          "N2O: ", total_N2O,
          "CH4: ", total_CH4,
          "Total Emissions: ", total_emissions
        ),
        labelOptions = labelOptions(
          style = list("font-weight" = "normal", padding = "3px 8px"),
          textsize = "15px",
          direction = "auto"
        )
      ) %>%
      addLegend(pal = colorBin("YlOrRd", NULL, bins = 7), values = ~total_emissions, opacity = 0.7, title = "Total Emissions", position = "bottomright")
  })
}

# ===============================================
# Run the application
# ===============================================
shinyApp(ui = ui, server = server)
