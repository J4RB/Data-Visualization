library(shiny)
library(leaflet)
library(dplyr)
library(sf)
library(ggplot2)

negotiation_plot_ui <- function(id){
  ns <- NS(id)
  tabPanel(
    "Negotiations Plot",
    sidebarLayout(
      sidebarPanel(
        pickerInput(
          ns("region"), 
          "Select region", 
          choices = c(unique(data$region)), 
          selected = c(unique(data$region)), 
          multiple = TRUE
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
        sliderInput(
          ns("squre_meters"),
          "Select square meter range",
          min = min(data$sqm, na.rm = TRUE),
          max = max(data$sqm, na.rm = TRUE),
          value = c(min(data$sqm, na.rm = TRUE), max(data$sqm, na.rm = TRUE)),
          step = 1
        )
      ),
      mainPanel(
        plotOutput(ns("negotiation_plot"))
      )
    )
  )
}

negotiation_plot_server <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    
    # ----------- FILTERED DATA -----------
    filtered_data <- reactive({
      data %>%
        filter(region %in% input$region) %>%
        filter(year >= input$year_range[1],
               year <= input$year_range[2]) %>%
        filter(sqm >= input$squre_meters[1],
               sqm <= input$squre_meters[2])
    })
    
    # ----------- PLOT OUTPUT -----------
    output$negotiation_plot <- renderPlot({
      
      df <- filtered_data()
      
      if (nrow(df) == 0) {
        return(NULL)
      }
      
      # Aggregate per year & region: mean, min, max % change
      df_summary <- df %>%
        group_by(year, region) %>%
        summarise(
          mean_change = mean(X._change_between_offer_and_purchase, na.rm = TRUE),
          min_change = min(X._change_between_offer_and_purchase, na.rm = TRUE),
          max_change = max(X._change_between_offer_and_purchase, na.rm = TRUE)
        )
      
      ggplot(df_summary, aes(x = year, color = region)) +
        geom_ribbon(aes(ymin = min_change, ymax = max_change, fill = region), alpha = 0.2, color = NA) +
        geom_line(aes(y = mean_change), size = 1) +
        geom_point(aes(y = mean_change), size = 2, alpha = 0.7) +
        labs(
          title = "Average % Change with Min-Max Range Over Years",
          x = "Year",
          y = "% Change Between Offer and Purchase",
          color = "Region",
          fill = "Region"
        ) +
        theme_minimal(base_size = 14) +
        theme(
          plot.title = element_text(face = "bold", hjust = 0.5)
        )
  })
})
}