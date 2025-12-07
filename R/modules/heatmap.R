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
        pickerInput(
          ns("region"), 
          "Select region", 
          choices = c(unique(data$region)), 
          selected = c(unique(data$region)), 
          multiple = TRUE
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
        ),
        sliderInput(
          ns("year_range"),
          "Select Year Range",
          min = min(data$year, na.rm = TRUE),
          max = max(data$year, na.rm = TRUE),
          value = c(min(data$year, na.rm = TRUE), max(data$year, na.rm = TRUE)),
          step = 1,
          sep = ""
        ),
        checkboxInput(
          ns("colorblind"),
          "Colorblind-friendly colors",
          value = FALSE
        )
      ),
      mainPanel(
        withSpinner(
          plotlyOutput(ns("heat_map"), height = '600px'),
          type = 4,
          color = '#444'
          )
        )
    )
  )
}

heatmap_server <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    
    filtered_data <- reactive({
      data %>% 
        filter(region %in% input$region) %>% 
        filter(
          year >= input$year_range[1],
          year <= input$year_range[2]
        )
    })
    
    output$heat_map <- plotly::renderPlotly({
      df <- filtered_data() %>%
        group_by(house_type, no_rooms) %>%
        summarise(
          value = mean(.data[[input$metric_choice]], na.rm = TRUE),
                  .groups = "drop"
          )
      
      metric_label <- switch(
        input$metric_choice,
        "sqm_price" = "Average Price per m² (DKK)",
        "purchase_price" = "Average Purchase Price (DKK)",
        "year_build" = "Average Year Built"
      )
      
      region_label <- if (length(input$region) == length(unique(data$region))) {
        "All Regions"
      } else {
        paste(input$region, collapse = ", ")
      }
      
      df$hover_text <- paste0(
        "<b>House Type:</b> ", df$house_type, "<br>",
        "<b>Rooms:</b> ", df$no_rooms, "<br>",
        "<b>", metric_label, ":</b> ", round(df$value, 2)
      )
      
      if (input$colorblind) {
        fill_scale <- scale_fill_viridis_c(
          option = "plasma",
          direction = -1,
          name = metric_label
        )
      } else {
        fill_scale <- scale_fill_viridis_c(
          option = "mako",
          direction = -1,
          name = metric_label
        )
      }
      
      p <- ggplot(df, aes(
        x = house_type, 
        y = factor(no_rooms), 
        fill = value,
        text = hover_text
        )) +
        geom_tile(color = "white") +
        fill_scale +
        labs(
          title = paste(metric_label, "by House Type and Rooms —", region_label),
          x = "House Type",
          y = "Number of Rooms"
        ) +
        theme_minimal(base_size = 14) +
        theme(
          axis.text.x = element_text(angle = 45, hjust = 1),
          plot.title = element_text(face = "bold", hjust = 0.5)
        )
      plotly::ggplotly(p, tooltip = "text") %>%
        plotly::style(hoverinfo = "text")
    })
  })
}