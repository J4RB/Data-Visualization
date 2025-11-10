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
library(gganimate)
library(gifski)

# UI
animation_ui <- function(id) {
  ns <- NS(id)
  tabPanel(
    "Animation",
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
        sliderInput(ns("speed"), "Animation Speed (fps):", min = 5, max = 30, value = 10, step = 1)
      ),
      mainPanel(
        withSpinner(imageOutput(ns("overall_purchase")))
      )
    )
  )
}

# Server
animation_server <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    
    output$overall_purchase <- renderImage({
    
    })
    
  })
}
