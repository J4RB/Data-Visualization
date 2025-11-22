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
        fluidRow(
          column(
            width = 6, 
            selectInput(ns("base_quarter"), "Select base queater", choices = unique(data$quarter), selected = "2020Q1"),
          ),
          column(
            width = 6, 
            selectInput(ns("base_year"), "Select base year", choices = unique(data$year), selected = "2020"),
          )
        ),
        selectInput(ns("graph_base_type"), "Generate graph by", choices = c("Quater", "Year"), selected = "Quater"),
        
        sliderInput(ns("year_range"), "Purchase year", min = min(data$year, na.rm = TRUE), max = max(data$year, na.rm = TRUE), step = 1, value = c(2000, max(data$year, na.rm = TRUE))),
        pickerInput(ns("house_type"),"Select house type", choices = c(unique(data$house_type)), selected = c(unique(data$house_type)), multiple = TRUE),
        pickerInput(ns("region"), "Select region", choices = c(unique(data$region)), selected = c(unique(data$region)), multiple = TRUE),
        pickerInput(ns("no_rooms"),"Select no rooms", choices = sort(unique(data$no_rooms)), selected = sort(unique(data$no_rooms)), multiple = TRUE),
        checkboxInput(ns("color_blind"), label = "Color-blind friendly colors", value = FALSE),
        selectInput(ns("graph_type"), "Show graph type", choices = c("Line Chart", "Bar Chart"), selected = "Line Chart")
      ),
      mainPanel(
        withSpinner(plotlyOutput(ns("simple_line_chart_overall"))),
        hr(),
        withSpinner(plotlyOutput(ns("simple_line_chart")))
      )
    )
  )
}

# Server
price_index_server <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    
    output$simple_line_chart_overall <- renderPlotly({
      
      base_quarter <- input$base_quarter
      base_year <- input$base_year
      year_1 <- input$year_range[1]
      year_2 <- input$year_range[2]
      filter_house_type <- input$house_type
      filter_region <- input$region
      filter_no_rooms <- input$no_rooms
      color_blind <- input$color_blind
      graph_base_type <- input$graph_base_type
      graph_type <- input$graph_type
      
      
      base_type_value <- base_quarter
      group_by_field <- "quarter"
      if(graph_base_type == "Year"){
        group_by_field <- "year"
        base_type_value <- base_year
      }
      
      hpi_data <- data %>%
        filter(year >= input$year_range[1] & year <= input$year_range[2]) %>%
        {if (!is.null(filter_house_type) && length(filter_house_type) > 0) filter(., house_type %in% filter_house_type) else .} %>%
        {if (!is.null(filter_region) && length(filter_region) > 0) filter(., region %in% filter_region) else .} %>%
        {if (!is.null(filter_no_rooms) && length(filter_no_rooms) > 0) filter(., no_rooms %in% filter_no_rooms) else .}
        
        # Check if the selected baseline exists
        available_baselines <- unique(hpi_data[[group_by_field]])
        if (!(base_type_value %in% available_baselines)) {
          # If not, pick the first available value as baseline
          old_base <- base_type_value
          base_type_value <- available_baselines[1]
          showNotification(
            paste0("Selected base ", graph_base_type, " '", old_base, 
                   "' not found in filtered data. Using '", base_type_value, "' as baseline instead."),
            type = "warning",
            duration = 5  # seconds
          )
        }
      
      hpi_data <- hpi_data %>%
        group_by(.data[[group_by_field]] ) %>%
        summarise(avg_price = mean(purchase_price, na.rm = TRUE),
                  total_houses = n(),
                  .groups = "drop"
        ) %>%
        mutate (
          base_price = avg_price[.data[[group_by_field]]  == base_type_value][1],
          hpi = (avg_price / base_price) * 100
        )%>%
        mutate(my_value = factor(.data[[group_by_field]] , levels = unique(.data[[group_by_field]]), ordered = TRUE))
      
      base_data <- hpi_data %>% filter(my_value == base_type_value)
      
      p <- hpi_data %>%
        ggplot( aes(
          x = my_value,
          y = hpi,
          group = 1,
          text = paste(
            "<br>Year:", my_value,
            "<br>HPI:", round(hpi, 1),
            "<br>Total Houses:", total_houses
          )
        )) +
        labs(
          title = paste0("Tracking the Danish Housing Market (",group_by_field,"ly): Housing Price Index from Year <b>",year_1,"</b> to <b>",year_2
          ,"</b> (Base ", group_by_field ," = <b>", base_type_value, "</b>)"),
          x = group_by_field,
          y = paste0("House Price Index")
        ) +
        theme_minimal(base_size = 12) +  
        scale_x_discrete(
          breaks = levels(hpi_data$my_value)[seq(1, length(levels(hpi_data$my_value)), by = 4)]
        ) +  # show every 4th quarter
        theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "none",
              plot.title = element_text(size = plot_title))
      
      if (graph_type == "Line Chart") {
        p <- p + geom_line(aes(color = "HPI"), size = 0.75, alpha = 0.9) + 
          geom_point(aes(color = "HPI"), size = 1, alpha = 0.8)
        
        if (isTRUE(color_blind)) {
          p <- p + scale_color_viridis_d(option = "E")
        } else {
          p <- p + scale_color_brewer(palette = color_palette)
        }
        
      } else if (graph_type == "Bar Chart") {
        p <- p + geom_col(aes(fill = "HPI")) + geom_line(aes(group = 1, color = "HPI"), size = 0.8) +  # Line connecting bars
          geom_point(aes(fill = "HPI"), size = 1.5)         # Points on top of line
        
        if (isTRUE(color_blind)) {
          p <- p + scale_fill_viridis_d(option = "E")
        } else {
          p <- p + scale_fill_brewer(palette = color_palette)
        }
      }
      
      p <- p +
        geom_point(
          data = base_data,
          aes(x = my_value, y = hpi),
          color = "red",
          size = 3
        ) +
        geom_text(
          data = base_data,
          aes(x = my_value, y = hpi, label = paste0("Base = ", round(hpi,1))),
          vjust = -1,
          color = "red",
          fontface = "bold"
        )
      
      
      suppressWarnings(ggplotly(p, tooltip = "text"))
      
    })
    
    output$simple_line_chart <- renderPlotly({
      
      base_quarter <- input$base_quarter
      base_year <- input$base_year
      year_1 <- input$year_range[1]
      year_2 <- input$year_range[2]
      filter_house_type <- input$house_type
      filter_region <- input$region
      filter_no_rooms <- input$no_rooms
      color_blind <- input$color_blind
      graph_base_type <- input$graph_base_type
      graph_type <- input$graph_type
      
      
      base_type_value <- base_quarter
      group_by_field <- "quarter"
      if(graph_base_type == "Year"){
        group_by_field <- "year"
        base_type_value <- base_year
      }
      
      hpi_data <- data %>%
        filter(year >= input$year_range[1] & year <= input$year_range[2]) %>%
        {if (!is.null(filter_house_type) && length(filter_house_type) > 0) filter(., house_type %in% filter_house_type) else .} %>%
        {if (!is.null(filter_region) && length(filter_region) > 0) filter(., region %in% filter_region) else .} %>%
        {if (!is.null(filter_no_rooms) && length(filter_no_rooms) > 0) filter(., no_rooms %in% filter_no_rooms) else .}
        
        
        # Check if the selected baseline exists
        available_baselines <- unique(hpi_data[[group_by_field]])
        if (!(base_type_value %in% available_baselines)) {
          # If not, pick the first available value as baseline
          old_base <- base_type_value
          base_type_value <- available_baselines[1]
          showNotification(
            paste0("Selected base ", graph_base_type, " '", old_base, 
                   "' not found in filtered data. Using '", base_type_value, "' as baseline instead."),
            type = "warning",
            duration = 5  # seconds
          )
        }
        
      hpi_data <- hpi_data %>%
        group_by(region, .data[[group_by_field]]) %>%
        summarise(avg_price = mean(purchase_price, na.rm = TRUE),  total_houses = n(), .groups = "drop") %>%
        group_by(region) %>%
        mutate (
          base_price = avg_price[.data[[group_by_field]] == base_type_value][1],
          hpi = (avg_price / base_price) * 100
        ) %>%
        arrange(.data[[group_by_field]])
      
      hpi_data$my_value <- factor(hpi_data[[group_by_field]], levels = unique(hpi_data[[group_by_field]]), ordered = TRUE)
      
      base_data <- hpi_data %>%
        filter(.data[[group_by_field]] == base_type_value)
      
      my_value_levels <- levels(hpi_data$my_value)
      n_my_values <- length(my_value_levels)
      x_breaks <- my_value_levels[seq(1, n_my_values, by = 4)]
      
      p <- hpi_data %>%
        ggplot( aes(
          x = my_value,
          y = hpi,
          color = region,
          group = region,,
          text = paste(
            "Region:", region,
            "<br>Year:", my_value,
            "<br>HPI:", round(hpi, 1),
            "<br>Total Houses:", total_houses
          )
        )) +
        labs(
          title = paste0("Tracking the Danish Housing Market (",group_by_field,"ly) by Region: Housing Price Index from Year <b>",year_1,"</b> to <b>",year_2
                         ,"</b> (Base ", group_by_field ," = <b>", base_type_value, "</b>)"),
          x = group_by_field,
          y = paste0("House Price Index"),
          color = "Region"
        ) +
        theme_minimal(base_size = 12) +   
        scale_x_discrete(breaks = x_breaks) +  # show every 4th quarter
        theme(axis.text.x = element_text(angle = 45, hjust = 1),
              plot.title = element_text(size = plot_title))
      
      if (input$graph_type == "Line Chart") {
        p <- p + geom_line(size = 0.75, alpha = 0.9) + 
          geom_point(size = 1, alpha = 0.8)
        
        if (isTRUE(color_blind)) {
          p <- p + scale_color_viridis_d(option = "E")
        } else {
          p <- p + scale_color_brewer(palette = color_palette)
        }
        
      } else if (input$graph_type == "Bar Chart") {
        p <- p + geom_col(aes(fill = region), position = position_dodge(width = 0.8)) + geom_line(size = 0.8) +  # Line connecting bars
          geom_point(size = 1.5)
        
        if (isTRUE(color_blind)) {
          p <- p + scale_fill_viridis_d(option = "E")
        } else {
          p <- p + scale_fill_brewer(palette = color_palette)
        }
      }
      
      p <- p +
        geom_point(
          data = base_data,
          aes(x = my_value, y = hpi, color = region),
          size = 3,
          shape = 21,
          fill = "red"   # optional: make the point standout
        ) +
        geom_text(
          data = base_data,
          aes(x = my_value, y = hpi, label = paste0("Base = ", round(hpi, 1))),
          vjust = -1,
          color = "red",
          fontface = "bold",
          show.legend = FALSE
        )
      
      suppressWarnings(
        suppressMessages((ggplotly(p, tooltip = "text"))
        )
      )
      
    })
    
    
    
  })
}
