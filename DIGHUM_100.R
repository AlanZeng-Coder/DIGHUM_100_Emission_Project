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
        condition = "input.tabselected==2",
        h4("Table in selected year"),
        sliderInput("yearRange", "Select Year Range:", 
                    min = min(emissions_data$Year), max = max(emissions_data$Year),
                    value = c(min(emissions_data$Year), min(emissions_data$Year) + 10)),
        checkboxGroupInput("categories", "Select Emission Categories:",
                           choices = list("CO2" = "Emissions.Type.CO2", 
                                          "N2O" = "Emissions.Type.N2O", 
                                          "CH4" = "Emissions.Type.CH4"),
                           selected = c("Emissions.Type.CO2", "Emissions.Type.N2O", "Emissions.Type.CH4")),
        checkboxGroupInput("sectors", "Select Emission Sectors:",
                           choices = list("Power Industry" = "Emissions.Sector.Power_Industry", 
                                          "Buildings" = "Emissions.Sector.Buildings", 
                                          "Transport" = "Emissions.Sector.Transport",
                                          "Other Industry" = "Emissions.Sector.Other_Industry",
                                          "Other Sectors" = "Emissions.Sector.Other_sectors"),
                           selected = c("Emissions.Sector.Power_Industry", "Emissions.Sector.Buildings", "Emissions.Sector.Transport", "Emissions.Sector.Other_Industry", "Emissions.Sector.Other_sectors")),
        checkboxGroupInput("ratios", "Select Ratios:",
                           choices = list("Per GDP" = "Ratio.Per_GDP", 
                                          "Per Capita" = "Ratio.Per_Capita"),
                           selected = c("Ratio.Per_GDP", "Ratio.Per_Capita")),
        checkboxInput("showTotal", "Show Total Columns", value = TRUE),
        selectizeInput("countries", "Select Countries:",
                       choices = unique(emissions_data$Country),
                       selected = NULL,
                       multiple = TRUE, options = list(maxOptions = 10))
      ),
      conditionalPanel(
        condition = "input.tabselected==1",
        h4("Year Analysis"),
        sliderInput("yearRangeMap", "Select Year Range:", 
                    min = min(emissions_data$Year), max = max(emissions_data$Year),
                    value = c(min(emissions_data$Year), min(emissions_data$Year) + 10)),
        selectInput("highlightCountry", "Select a Country to Highlight:",
                    choices = unique(emissions_data$Country),
                    selected = NULL)
      ),
      conditionalPanel(
        condition = "input.tabselected==3",
        h4("Summary Plot"),
        selectInput("analysisType", "Select Analysis Type:",
                    choices = list("Max in all years" = "max_years", "Country in all years" = "country_years"),
                    selected = "max_years"),
        selectInput("emissionType", "Select Emission Type:",
                    choices = list("CO2" = "Emissions.Type.CO2", 
                                   "N2O" = "Emissions.Type.N2O", 
                                   "CH4" = "Emissions.Type.CH4",
                                   "Total Sector" = "Sector_Total",
                                   "Total Type" = "Type_Total",
                                   "Per GDP" = "Ratio.Per_GDP",
                                   "Per Capita" = "Ratio.Per_Capita",
                                   "Power Industry" = "Emissions.Sector.Power_Industry",
                                   "Buildings" = "Emissions.Sector.Buildings",
                                   "Transport" = "Emissions.Sector.Transport",
                                   "Other Industry" = "Emissions.Sector.Other_Industry",
                                   "Other Sectors" = "Emissions.Sector.Other_sectors"),
                    selected = "Emissions.Type.CO2"),
        conditionalPanel(
          condition = "input.analysisType == 'country_years'",
          selectInput("country", "Select Country:",
                      choices = unique(emissions_data$Country),
                      selected = unique(emissions_data$Country)[1])
        )
      ),
      conditionalPanel(
        condition = "input.tabselected==4",
        h4("Compare Countries"),
        selectInput("country1", "Select First Country:",
                    choices = unique(emissions_data$Country),
                    selected = unique(emissions_data$Country)[1]),
        selectInput("country2", "Select Second Country:",
                    choices = unique(emissions_data$Country),
                    selected = unique(emissions_data$Country)[2]),
        selectInput("compareEmissionType", "Select Emission Type:",
                    choices = list("CO2" = "Emissions.Type.CO2", 
                                   "N2O" = "Emissions.Type.N2O", 
                                   "CH4" = "Emissions.Type.CH4",
                                   "Total Sector" = "Sector_Total",
                                   "Total Type" = "Type_Total",
                                   "Per GDP" = "Ratio.Per_GDP",
                                   "Per Capita" = "Ratio.Per_Capita",
                                   "Power Industry" = "Emissions.Sector.Power_Industry",
                                   "Buildings" = "Emissions.Sector.Buildings",
                                   "Transport" = "Emissions.Sector.Transport",
                                   "Other Industry" = "Emissions.Sector.Other_Industry",
                                   "Other Sectors" = "Emissions.Sector.Other_sectors"),
                    selected = "Emissions.Type.CO2")
      )
    ),
    mainPanel(
      tabsetPanel(
        type = "tabs",
        tabPanel(title = "Table",
                 value = 2,
                 DT::DTOutput(outputId = "table")),
        tabPanel(title = "Map",
                 value = 1,
                 leafletOutput("map", height = 1000)),
        tabPanel(title = "Summary",
                 value = 3,
                 plotlyOutput(outputId = "summaryPlot")),
        tabPanel(title = "Compare",
                 value = 4,
                 plotlyOutput(outputId = "comparePlot")),
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
  
  observeEvent(input$highlightCountry, {
    country <- input$highlightCountry
    if (!is.null(country)) {
      country_data <- complete_data_map_or_table %>%
        filter(Country == country) %>%
        group_by(Country) %>%
        summarise(
          total_CO2 = sum(Emissions.Type.CO2, na.rm = TRUE),
          total_N2O = sum(Emissions.Type.N2O, na.rm = TRUE),
          total_CH4 = sum(Emissions.Type.CH4, na.rm = TRUE),
          total_emissions = sum(Emissions.Type.CO2, Emissions.Type.N2O, Emissions.Type.CH4, na.rm = TRUE)
        )
      
      country_geom <- world %>% filter(name == country)
      
      if (nrow(country_geom) > 0) {
        leafletProxy("map") %>%
          clearGroup("highlight") %>%
          addPolygons(
            data = country_geom, 
            color = "blue", 
            weight = 3, 
            group = "highlight",
            label = ~paste(
              "Country: ", name,
              "Total CO2: ", country_data$total_CO2,
              "Total N2O: ", country_data$total_N2O,
              "Total CH4: ", country_data$total_CH4,
              "Total Emissions: ", country_data$total_emissions
            ),
            labelOptions = labelOptions(
              style = list("font-weight" = "normal", padding = "3px 8px"),
              textsize = "15px",
              direction = "auto"
            )
          )
      }
    }
  })
  
  
  # Table of emissions in selected year range and selected categories
  output$table <- DT::renderDT({
    if (is.null(input$countries) || length(input$countries) == 0) {
      return(data.frame(message = "No countries selected"))
    }
    
    filtered_data <- complete_data_map_or_table %>%
      filter(Country %in% input$countries, Year >= input$yearRange[1] & Year <= input$yearRange[2])
    
    if (length(input$categories) == 0 && length(input$sectors) == 0 && length(input$ratios) == 0) {
      return(data.frame(message = "No categories, sectors, or ratios selected"))
    }
    
    selected_columns <- c("Country", "Year", input$categories, input$sectors)
    table_data <- filtered_data %>%
      select(all_of(selected_columns))
    
    if (input$showTotal) {
      if (length(input$categories) > 0) {
        table_data <- table_data %>%
          rowwise() %>%
          mutate(Type_Total = sum(c_across(all_of(input$categories)), na.rm = TRUE)) %>%
          ungroup()
      }
      if (length(input$sectors) > 0) {
        table_data <- table_data %>%
          rowwise() %>%
          mutate(Sector_Total = sum(c_across(all_of(input$sectors)), na.rm = TRUE)) %>%
          ungroup()
      }
    }
    
    if (length(input$ratios) > 0) {
      table_data <- table_data %>%
        bind_cols(filtered_data %>% select(all_of(input$ratios)))
    }
    
    datatable(table_data, options = list(pageLength = 25))
  })
  
  # Fill missing years with zero emissions and calculate totals
  complete_data_summary_compare <- emissions_data %>%
    complete(Country, Year = full_seq(Year, 1), fill = list(
      Emissions.Type.CO2 = 0, Emissions.Type.N2O = 0, Emissions.Type.CH4 = 0, 
      Emissions.Sector.Power_Industry = 0, Emissions.Sector.Buildings = 0, 
      Emissions.Sector.Transport = 0, Emissions.Sector.Other_Industry = 0, 
      Emissions.Sector.Other_sectors = 0, Ratio.Per_GDP = 0, Ratio.Per_Capita = 0)) %>%
    mutate(Sector_Total = Emissions.Sector.Power_Industry + Emissions.Sector.Buildings + Emissions.Sector.Transport + Emissions.Sector.Other_Industry + Emissions.Sector.Other_sectors,
           Type_Total = Emissions.Type.CO2 + Emissions.Type.N2O + Emissions.Type.CH4)
  
  # Summary plot rendering logic
  output$summaryPlot <- renderPlotly({
    if (input$analysisType == "max_years") {
      filtered_data <- complete_data_summary_compare %>%
        group_by(Year, Country) %>%
        summarise(Total = sum(get(input$emissionType), na.rm = TRUE)) %>%
        ungroup()
      
      max_data <- filtered_data %>%
        group_by(Year) %>%
        filter(Total == max(Total))
      
      plot <- ggplot(max_data, aes(x = Year, y = Total, fill = Country)) +
        geom_bar(stat = "identity") +
        geom_text(aes(label = Country), vjust = -0.5, hjust = 0.5) +
        labs(title = "Top Emitting Country by Year", x = "Year", y = "Total Emissions") +
        theme_minimal() +
        theme(legend.position = "none")
      
    } else {
      filtered_data <- complete_data_summary_compare %>%
        filter(Country == input$country) %>%
        group_by(Year) %>%
        summarise(Total = sum(get(input$emissionType), na.rm = TRUE)) %>%
        ungroup()
      
      plot <- ggplot(filtered_data, aes(x = Year, y = Total)) +
        geom_line() +
        geom_point() +
        labs(title = paste("Emissions in", input$country), x = "Year", y = "Total Emissions") +
        theme_minimal()
    }
    
    ggplotly(plot)
  })
  
  # Comparison plot rendering logic
  output$comparePlot <- renderPlotly({
    data_country1 <- complete_data_summary_compare %>%
      filter(Country == input$country1) %>%
      group_by(Year) %>%
      summarise(Total = sum(get(input$compareEmissionType), na.rm = TRUE)) %>%
      ungroup() %>%
      mutate(Country = input$country1)
    
    data_country2 <- complete_data_summary_compare %>%
      filter(Country == input$country2) %>%
      group_by(Year) %>%
      summarise(Total = sum(get(input$compareEmissionType), na.rm = TRUE)) %>%
      ungroup() %>%
      mutate(Country = input$country2)
    
    comparison_data <- bind_rows(data_country1, data_country2)
    
    plot <- ggplot(comparison_data, aes(x = Year, y = Total, color = Country)) +
      geom_line() +
      geom_point() +
      labs(title = paste("Comparison of", input$compareEmissionType, "between", input$country1, "and", input$country2), x = "Year", y = "Total Emissions") +
      theme_minimal()
    
    ggplotly(plot)
  })
}

# ===============================================
# Run the application
# ===============================================
shinyApp(ui = ui, server = server)
