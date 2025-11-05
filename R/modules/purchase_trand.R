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
purchase_ui <- function(id) {
  ns <- NS(id)
  tabPanel(
    "Purchase Trand",
    sidebarLayout(
      sidebarPanel(
        selectInput(ns("year"), "Select a year", choices = unique(data$year), selected = "2020"),
        selectInput(ns("house_type"),"select house type", choices = c("All", unique(data$house_type)), selected = "All"),
        selectInput(ns("region"), "select a region", choices = c("All", unique(data$region)), selected = "All"),
        selectInput(ns("plot_by"), "Plot by", choices = c("purchase price" = "purchase_price","sqm price"  = "sqm_price"),selected = "purchase_price")
      ),
      mainPanel(
        plotlyOutput(ns("scart_point")),
        plotlyOutput(ns("scart_point_by_room"))
      )
    )
  )
}

# Server
purchase_server <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    
    
    output$scart_point <- renderPlotly({
      
      p <- data %>%
        filter(year == input$year) %>%
        { if (input$house_type == "All") . else filter(., house_type == input$house_type) } %>%
        { if (input$region == "All") . else filter(., region == input$region) } %>%
        ggplot(aes(x = date, y = .data[[input$plot_by]], color = house_type
                   , text =  paste(
                     "Date:", date,
                     "<br>", input$plot_by,":", .data[[input$plot_by]],
                     "<br>House Type:", house_type,
                     "<br>Room No:", no_rooms,
                     "<br>Region:", region
                   )
        ))+
        #facet_wrap(~ no_rooms, scales = "free_y") +
        geom_point()+
        labs(
          title = paste0("Perchase record of <b>", input$year, "</b> year for <b>", input$house_type, "</b> house type"),
          x = "Date",
          y = input$plot_by,,
          color = "House Type",
          size = "No of Rooms"
        )+
        geom_hline(yintercept = mean(data[[input$plot_by]], na.rm = TRUE),
                   color = "red",
                   linetype = "dashed"
        )+
        theme(
          plot.title = element_markdown(size = 14)  # enables HTML <b> tags
        )
      ggplotly(p, tooltip = "text")
    })
    
    output$scart_point_by_room <- renderPlotly({
      
      
      p <- data %>%
        filter(year == input$year) %>%
        { if (input$house_type == "All") . else filter(., house_type == input$house_type) } %>%
        { if (input$region == "All") . else filter(., region == input$region) } %>%
        ggplot(aes(x = date, y = .data[[input$plot_by]], color = house_type
                   , text =  paste(
                     "Date:", date,
                     "<br>", input$plot_by,"Purchase Price:", .data[[input$plot_by]],
                     "<br>House Type:", house_type,
                     "<br>Room No:", no_rooms,
                     "<br>Region:", region
                   )
        ))+
        facet_wrap(~ no_rooms) +
        geom_point()+
        labs(
          title = paste0("Perchase record of <b>", input$year, "</b> year for <b>", input$house_type, "</b> house type"),
          x = "Date",
          y = input$plot_by,,
          color = "House Type",
          size = "No of Rooms"
        )+
        geom_hline(yintercept = mean(data[[input$plot_by]], na.rm = TRUE),
                   color = "red",
                   linetype = "dashed"
        )+
        theme(
          plot.title = element_markdown(size = 14)  # enables HTML <b> tags
        )
      ggplotly(p, tooltip = "text")
    })
    
    
    
  })
}
