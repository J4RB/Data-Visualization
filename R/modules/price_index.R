library(shiny)
library(ggplot2)
library(plotly)
library(viridis)
library(DT)
library(readr)
library(lubridate)
library(ggtext)
library(dplyr)

# UI
price_index_ui <- function(id) {
  ns <- NS(id)
  tabPanel(
    "Price Index",
    sidebarLayout(
      sidebarPanel(
        selectInput(ns("quarter_tab2"), "Select base queater", choices = unique(data$quarter), selected = "2020Q1"),
        selectInput(ns("house_type_tab2"), "select house type", choices = c("All", unique(data$house_type)), selected = "All"),
        selectInput(ns("region_tab2"), "select a region", choices = c("All", unique(data$region)),  selected = "All"),
        selectInput(ns("no_rooms_tab2"), "select a no of rooms", choices = c("All", unique(data$no_rooms)), selected = "All")
      ),
      mainPanel(
        plotlyOutput(ns("simple_line_chart_overall")),
        plotlyOutput(ns("simple_line_chart"))
      )
    )
  )
}

# Server
price_index_server <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    
    
    output$simple_line_chart_overall <- renderPlotly({
      
      hpi_data <- data %>%
        { if (input$house_type_tab2 == "All") . else filter(., house_type == input$house_type_tab2) } %>%
        { if (input$region_tab2 == "All") . else filter(., region == input$region_tab2) } %>%
        { if (input$no_rooms_tab2 == "All") . else filter(., no_rooms == input$no_rooms_tab2) } %>%
        group_by(quarter) %>%
        summarise(avg_price = mean(purchase_price, na.rm = TRUE),
                  total_houses = n(),
        ) %>%
        #group_by(region) %>%
        mutate (
          #base_price = first(avg_price),
          base_price = avg_price[quarter == input$quarter_tab2][1],
          hpi = (avg_price / base_price) * 100
        )%>%
        mutate(quarter = factor(quarter, levels = unique(quarter), ordered = TRUE))
      
      p <- hpi_data %>%
        ggplot( aes(
          x = quarter,
          y = hpi,
          group = 1,
          text = paste(
            "<br>Year:", quarter,
            "<br>HPI:", round(hpi, 1),
            "<br>Total Houses:", total_houses
          )
        )) +
        labs(
          title = "House Price Index",
          x = "Quarter",
          y = paste0("House Price Index (Base Year =", input$quarter_tab2, ")"),
          color = "Region"
        ) +
        geom_line(size = .75) +
        geom_point(size = .1) + 
        scale_x_discrete(
          breaks = levels(hpi_data$quarter)[seq(1, length(levels(hpi_data$quarter)), by = 4)]
        ) +  # show every 4th quarter
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
      ggplotly(p, tooltip = "text")
      
    })
    
    output$simple_line_chart <- renderPlotly({
      
      hpi_data <- data %>%
        { if (input$house_type_tab2 == "All") . else filter(., house_type == input$house_type_tab2) } %>%
        { if (input$region_tab2 == "All") . else filter(., region == input$region_tab2) } %>%
        { if (input$no_rooms_tab2 == "All") . else filter(., no_rooms == input$no_rooms_tab2) } %>%
        group_by(region, quarter) %>%
        summarise(avg_price = mean(purchase_price, na.rm = TRUE)) %>%
        group_by(region) %>%
        mutate (
          #base_price = first(avg_price),
          base_price = avg_price[quarter == input$quarter_tab2][1],
          hpi = (avg_price / base_price) * 100
        ) %>%
        arrange(quarter)
      
      hpi_data$quarter <- factor(hpi_data$quarter, levels = unique(hpi_data$quarter), ordered = TRUE)
      
      quarters_levels <- levels(hpi_data$quarter)
      n_quarters <- length(quarters_levels)
      x_breaks <- quarters_levels[seq(1, n_quarters, by = 4)]
      
      p <- hpi_data %>%
        ggplot( aes(
          x = quarter,
          y = hpi,
          color = region,
          group = region,,
          text = paste(
            "Region:", region,
            "<br>Year:", quarter,
            "<br>HPI:", round(hpi, 1)
          )
        )) +
        labs(
          title = "House Price Index by Region",
          x = "Quarter",
          y = paste0("House Price Index (Base Year =", input$quarter_tab2, ")"),
          color = "Region"
        ) +
        geom_line(size = 1) +
        geom_point(size = 2) + 
        scale_x_discrete(breaks = x_breaks) +  # show every 4th quarter
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
      ggplotly(p, tooltip = "text")
      
    })
    
    
    
  })
}
