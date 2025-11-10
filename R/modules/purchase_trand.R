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
library(scales)

# UI
purchase_ui <- function(id) {
  ns <- NS(id)
  tabPanel(
    "Purchase Trand",
    sidebarLayout(
      sidebarPanel(
        selectInput(ns("year"), "Select a year", choices = unique(data$year), selected = "2020"),
        selectInput(ns("house_type"),"Select house type", choices = c("All", unique(data$house_type)), selected = "All"),
        selectInput(ns("region"), "Select a region", choices = c("All", unique(data$region)), selected = "All"),
        selectInput(ns("plot_by"), "Plot by", choices = plot_choices, selected = "sqm_price"),
        checkboxInput(ns("color_blind"), label = "Enable color-blind friendly colors", value = FALSE),
        selectInput(ns("bar_chart_type"), "Plot bar chart", choices = c("Position Dodge", "Stacked"), selected = "Stacked"),
        sliderInput(ns("year_range"), "Purchase year", min = min(data$year, na.rm = TRUE), max = max(data$year, na.rm = TRUE), step = 1, value = c(2000, max(data$year, na.rm = TRUE))),
        selectInput(ns("bar_plot_by"), "Plot by", choices = bar_plot_choices, selected = "total_value"),
      ),
      mainPanel(
        withSpinner(plotlyOutput(ns("scart_point"))),
        br(),br(),
        withSpinner(plotlyOutput(ns("scart_point_by_room"))),
        br(),br(),
        withSpinner(plotlyOutput(ns("year_house_bar")))
      )
    )
  )
}

# Server
purchase_server <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    
    
    output$scart_point <- renderPlotly({
      
      filter_data <- data %>%
        filter(year == input$year) %>%
        { if (input$house_type == "All") . else filter(., house_type == input$house_type) } %>%
        { if (input$region == "All") . else filter(., region == input$region) }
      mean_y <- mean(filter_data[[input$plot_by]], na.rm = TRUE)
      
      plot_label <- names(plot_choices)[plot_choices == input$plot_by]
      #y_formatter <- label_number(scale_cut = cut_short_scale(), accuracy = 0.1)
      
      p <- ggplot(filter_data, aes(x = date, y = .data[[input$plot_by]], color = house_type
                   , text =  paste(
                     "Date:", date,
                     "<br>", plot_label,":", .data[[input$plot_by]],
                     "<br>House Type:", house_type,
                     "<br>Room No:", no_rooms,
                     "<br>Region:", region
                   )
        ))+
        scale_y_continuous(
          labels = label_number(scale_cut = cut_short_scale())
        )+
        geom_point(alpha = 0.6, size = 3) +
        geom_jitter(width = 0.1, height = 0.1, alpha = 0.4) +
        labs(
          title = paste0("Perchase record of <b>", input$year, "</b> year for <b>", input$house_type, "</b> house type"),
          x = "Date",
          y = plot_label,
          color = "House Type"
        )+
        geom_hline(yintercept = mean_y, color = "red", linetype = "dashed") +
        annotate(
          "text",
          x = mean(filter_data$date),
          y = mean_y,
          label = paste("Mean =", y_formatter(mean_y)),
          color = bc,
          vjust = -1,
          size = 4
        )+
        theme_minimal(base_size = 12) + 
        theme(
          plot.title = element_markdown(size = 14)
        )
      
      if (isTRUE(input$color_blind)) {
        p <- p + scale_color_viridis_d(option = "E", begin = 0.1, end = 0.9)
      } else {
        p <- p + scale_color_brewer(palette = "Set2")
      }
      
      ggplotly(p, tooltip = "text")
    })
    
    output$scart_point_by_room <- renderPlotly({
      
      filter_data <- data %>%
        filter(year == input$year) %>%
        { if (input$house_type == "All") . else filter(., house_type == input$house_type) } %>%
        { if (input$region == "All") . else filter(., region == input$region) }
      mean_y <- mean(filter_data[[input$plot_by]], na.rm = TRUE)
      
      plot_label <- names(plot_choices)[plot_choices == input$plot_by]
      #y_formatter <- label_number(scale_cut = cut_short_scale(), accuracy = 0.1)
      
      p <- ggplot(filter_data, aes(x = date, y = .data[[input$plot_by]], color = house_type
                                   , text =  paste(
                                     "Date:", date,
                                     "<br>", plot_label,":", .data[[input$plot_by]],
                                     "<br>House Type:", house_type,
                                     "<br>Room No:", no_rooms,
                                     "<br>Region:", region
                                   )
      ))+
        facet_wrap(~ no_rooms) +
        scale_y_continuous(
          labels = label_number(scale_cut = cut_short_scale())
        )+
        geom_point(alpha = 0.6, size = 3) +
        geom_jitter(width = 0.1, height = 0.1, alpha = 0.4) +
        labs(
          title = paste0("Perchase record of <b>", input$year, "</b> year for <b>", input$house_type, "</b> house type"),
          x = "Date",
          y = plot_label,
          color = "House Type"
        )+
        geom_hline(yintercept = mean_y, color = "red", linetype = "dashed") +
        annotate(
          "text",
          x = mean(filter_data$date),
          y = mean_y,
          label = paste("Mean =", y_formatter(mean_y)),
          color = bc,
          vjust = -1,
          size = 4
        )+
        theme_minimal(base_size = 12) + 
        theme(
          plot.title = element_markdown(size = 14),
          axis.text.x = element_text(angle = 45, hjust = 1)
        )
      
      if (isTRUE(input$color_blind)) {
        p <- p + scale_color_viridis_d(option = "E", begin = 0.1, end = 0.9)
      } else {
        p <- p + scale_color_brewer(palette = "Set2")
      }
      
      ggplotly(p, tooltip = "text")
    })
    
    
    output$year_house_bar <- renderPlotly({
      plot_label <- names(bar_plot_choices)[bar_plot_choices == input$bar_plot_by]
      
      bar_data <- data %>%
      { if (input$region == "All") . else filter(., region == input$region) } %>%
      filter(year >= input$year_range[1] & year <= input$year_range[2]) %>%
        group_by(year, house_type) %>%
        summarise(
          total_value = sum(purchase_price, na.rm = TRUE),
          count = n(),
          .groups = "drop"
        )
      
      p <- ggplot(bar_data, aes(
        x = factor(year),
        y = !!sym(input$bar_plot_by),
        fill = house_type,
        text = paste(
          "Year:", year,
          "<br>House Type:", house_type,
          
          if (input$bar_plot_by == "total_value") {
            paste("<br>Total Purchase:", y_formatter(total_value))
          } else {
            paste("<br>sales:", y_formatter(count))
          }
        )
      ))  +
      labs(
        x = "Year",
        y = paste0("Total ",  plot_label),
        fill = "House Type",
        title = paste0("<b>", plot_label, "</b> by Year and House Type")
      ) +
      theme_minimal(base_size = 14) +
      scale_y_continuous(labels = label_number(scale_cut = cut_short_scale(), accuracy = 0.1)) +
      theme(
        plot.title = element_text(size = 16)
        , axis.text.x = element_text(angle = 45, hjust = 1)
            )
      
      if (isTRUE(input$color_blind)) {
        p <- p + scale_fill_viridis_d(option = "E", begin = 0.1, end = 0.9)
      } else {
        p <- p + scale_fill_brewer(palette = "Set2")
      }
      
      if (input$bar_chart_type == "Position Dodge") {
        p <- p +  geom_bar(stat = "identity", position = position_dodge(width = 0.8))#+
          #geom_text(
           # aes(label = y_formatter(!!sym(input$bar_plot_by))),
           # position = position_dodge(width = 0.8), # match the bars
          #  vjust = -0.5, 
          #  hjust = -0.1, 
          #  size = 3
          #)
      } else {
        total_counts <- bar_data %>%
          group_by(year) %>%
          summarise(total = sum(!!sym(input$bar_plot_by)), .groups = "drop")
        p <- p +  geom_bar(stat = "identity")+
          geom_text(
            data = total_counts,
            aes(x = factor(year), y = total, label = y_formatter(total)),
            vjust = -2.5,
            hjust = -0.9, 
            size = 3,
            inherit.aes = FALSE
          )
      }
      
      ggplotly(p, tooltip = "text")
    })
    
    
    
    
    
  })
}
