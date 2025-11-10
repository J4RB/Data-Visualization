library(shiny)
library(ggplot2)
library(plotly)
library(viridis)
library(DT)
library(readr)
library(lubridate)
library(ggtext)
library(dplyr)
library(shinycssloaders)

# UI
price_index_ui <- function(id) {
  ns <- NS(id)
  tabPanel(
    "Price Index",
    sidebarLayout(
      sidebarPanel(
        selectInput(ns("quarter"), "Select base queater", choices = unique(data$quarter), selected = "2020Q1"),
        selectInput(ns("house_type"), "Select house type", choices = c("All", unique(data$house_type)), selected = "All"),
        selectInput(ns("region"), "Select a region", choices = c("All", unique(data$region)),  selected = "All"),
        selectInput(ns("no_rooms"), "select a no of rooms", choices = c("All", unique(data$no_rooms)), selected = "All"),
        sliderInput(ns("year_build"), "Constraction year", min = min(data$year_build, na.rm = TRUE), max = max(data$year_build, na.rm = TRUE), value = range(data$year_build, na.rm = TRUE), step = 1),
        sliderInput(ns("year_range"), "Purchase year", min = min(data$year, na.rm = TRUE), max = max(data$year, na.rm = TRUE), value = range(data$year, na.rm = TRUE), step = 1),
        selectInput(ns("graph_type"), "Show graph type", choices = c("Line Chart", "Bar Chart"), selected = "Line Chart")
      ),
      mainPanel(
        withSpinner(plotlyOutput(ns("simple_line_chart_overall"))),
        br(),br(),
        withSpinner(plotlyOutput(ns("simple_line_chart")))
      )
    )
  )
}

# Server
price_index_server <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    
    output$simple_line_chart_overall <- renderPlotly({
      
      hpi_data <- data %>%
        { if (input$house_type == "All") . else filter(., house_type == input$house_type) } %>%
        { if (input$no_rooms == "All") . else filter(., no_rooms == input$no_rooms) } %>%
        filter(year_build >= input$year_build[1] & year_build <= input$year_build[2]) %>%
        filter(year >= input$year_range[1] & year <= input$year_range[2]) %>%
        group_by(quarter) %>%
        summarise(avg_price = mean(purchase_price, na.rm = TRUE),
                  total_houses = n(),
                  .groups = "drop"
        ) %>%
        mutate (
          base_price = avg_price[quarter == input$quarter][1],
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
          title = paste0("Overall House Price Index (Base quarter = <b>", input$quarter, "</b>)"),
          x = "Quarter",
          y = paste0("House Price Index")
        ) +
        theme_minimal(base_size = 12) +  
        scale_x_discrete(
          breaks = levels(hpi_data$quarter)[seq(1, length(levels(hpi_data$quarter)), by = 4)]
        ) +  # show every 4th quarter
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
      
      if (input$graph_type == "Line Chart") {
        p <- p + geom_line(size = 0.75, alpha = 0.9, color = bc) + 
          geom_point(size = 1, alpha = 0.8, color = bc)
      } else if (input$graph_type == "Bar Chart") {
        p <- p + geom_col(fill = bc) 
      }
      
      ggplotly(p, tooltip = "text")
      
    })
    
    output$simple_line_chart <- renderPlotly({
      
      hpi_data <- data %>%
        { if (input$house_type == "All") . else filter(., house_type == input$house_type) } %>%
        { if (input$region == "All") . else filter(., region == input$region) } %>%
        { if (input$no_rooms == "All") . else filter(., no_rooms == input$no_rooms) } %>%
        filter(year_build >= input$year_build[1] & year_build <= input$year_build[2]) %>%
        filter(year >= input$year_range[1] & year <= input$year_range[2]) %>%
        group_by(region, quarter) %>%
        summarise(avg_price = mean(purchase_price, na.rm = TRUE),  total_houses = n(), .groups = "drop") %>%
        group_by(region) %>%
        mutate (
          base_price = avg_price[quarter == input$quarter][1],
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
            "<br>HPI:", round(hpi, 1),
            "<br>Total Houses:", total_houses
          )
        )) +
        labs(
          title = paste0("House Price Index by Region (Base quarter = <b>", input$quarter, "</b>)"),
          x = "Quarter",
          y = paste0("House Price Index"),
          color = "Region"
        ) +
        theme_minimal(base_size = 12) +   
        scale_color_viridis_d(option = "D") +
        scale_x_discrete(breaks = x_breaks) +  # show every 4th quarter
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
      
      if (input$graph_type == "Line Chart") {
        p <- p + geom_line(size = 0.75, alpha = 0.9) + 
          geom_point(size = 1, alpha = 0.8)
      } else if (input$graph_type == "Bar Chart") {
        p <- p + geom_col(aes(fill = region), position = position_dodge(width = 0.8)) 
      }
      
      ggplotly(p, tooltip = "text")
      
    })
    
    
    
  })
}
