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
        HTML('
              <p>Here you can use these options to filter the data presented in the heatmap</p>
        '),
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
        HTML('
          <div>
          <p>This graph is investigating the following question</p>
          <ul>
            <li>Does the negotiation play a crucial part when purchasing a house in Denmark?</li>
          </ul>
          </div>
        '),
        withSpinner(
          plotlyOutput(ns("negotiation_plot")),
          type = 4,
          color = "#444"
        )
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
    output$negotiation_plot <- plotly::renderPlotly({
      
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
          max_change = max(X._change_between_offer_and_purchase, na.rm = TRUE),
          .groups = 'drop'
        )
      
      p <- ggplot(df_summary, aes(x = year, color = region)) +
        geom_ribbon(
          aes(
            ymin = min_change, 
            ymax = max_change, 
            fill = region,
            label = paste(
              "Region: ", region, "<br>",
              "Year: ", year, "<br>",
              "Mean Change: ", round(mean_change, 2), "<br>",
              "Min Change: ", round(min_change, 2), "<br>",
              "Max Change: ", round(max_change, 2)
            )
          ),
          alpha = 0.2, color = NA
        ) +
        geom_line(aes(y = mean_change)) +
        geom_point(
          aes(
            y = mean_change,
            label = paste(
              "Region: ", region, "<br>",
              "Year: ", year, "<br>",
              "Mean change: ", round(mean_change, 2)
            )
          ),
          size = 2
        ) +
        theme_minimal()
      
      ggplotly(p, tooltip = "label")
  })
})
}