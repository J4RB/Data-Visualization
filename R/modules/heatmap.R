library(shiny)
library(leaflet)
library(dplyr)
library(sf)
library(ggplot2)

heatmap_ui <- function(id) {
  ns <- NS(id)
  tabPanel(
    "Heatmap",
    sidebarLayout(
      sidebarPanel(
        selectInput(
          ns("region_choice"),
          "Select Region",
          choices = c("All", unique(data$region)),
          selected = "All"
        ),
        selectInput(
          ns("metric_choice"),
          "Select Metric for Heatmap",
          choices = c(
            "Average Price per m²" = "sqm_price",
            "Average Purchase Price" = "purchase_price",
            "Average Year Built" = "year_build"
          ),
          selected = "sqm_price"
        )
      ),
      mainPanel(
        plotOutput(ns("heat_map"), height = '600px')
        )
    )
  )
}

heatmap_server <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    
    filtered_data <- reactive({
     if (input$region_choice == "All"){
       data
     } else {
       data %>% filter(region == input$region_choice)
     } 
    })
    
    output$heat_map <- renderPlot({
      df <- filtered_data() %>%
        group_by(house_type, no_rooms) %>%
        summarise(value = mean(.data[[input$metric_choice]], na.rm = TRUE),
                  .groups = "drop")
      
      metric_label <- switch(
        input$metric_choice,
        "sqm_price" = "Average Price per m² (DKK)",
        "purchase_price" = "Average Purchase Price (DKK)",
        "year_build" = "Average Year Built"
      )
      
      ggplot(df, aes(x = house_type, y = factor(no_rooms), fill = value)) +
        geom_tile(color = "white") +
        scale_fill_viridis_c(option = "mako", name = metric_label) +
        labs(
          title = paste(metric_label, "by House Type and Rooms —", input$region_choice),
          x = "House Type",
          y = "Number of Rooms"
        ) +
        theme_minimal(base_size = 14) +
        theme(
          axis.text.x = element_text(angle = 45, hjust = 1),
          plot.title = element_text(face = "bold", hjust = 0.5)
        )
    })
  })
}