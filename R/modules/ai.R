# ai.R
# Ridgeline density visualization module — cleaned version

library(shiny)
library(dplyr)
library(ggplot2)
library(ggridges)
library(viridis)
library(scales)

# ---- UI ----
ai_ui <- function(id) {
  ns <- NS(id)
  
  tabPanel(
    "AI: Price Density Trends",
    sidebarLayout(
      sidebarPanel(
        # ---- Metric dropdown ----
        selectInput(
          ns("metric"),
          "Metric:",
          choices = c(
            "Square meter price (DKK/m²)" = "sqm_price",
            "Purchase price (DKK)" = "purchase_price",
            "House size (m²)" = "sqm",
            "Number of rooms" = "no_rooms",
            "Year built" = "year_build"
          ),
          selected = "sqm_price"
        ),
        
        tags$hr(),
        
        fluidRow(
          column(
            6,
            checkboxGroupInput(
              ns("house_type"),
              "House type:",
              choices = c(
                "Villa" = "Villa",
                "Townhouse" = "Townhouse",
                "Apartment" = "Apartment",
                "Summerhouse" = "Summerhouse",
                "Farm" = "Farm"
              ),
              selected = c("Villa", "Townhouse", "Apartment", "Summerhouse", "Farm")
            )
          ),
          column(
            6,
            checkboxGroupInput(
              ns("sales_type"),
              "Sales type:",
              choices = c(
                "Regular Sale" = "regular_sale",
                "Family Sale"  = "family_sale",
                "Auction"      = "auction",
                "Other Sale"   = "other_sale"
              ),
              selected = c("regular_sale", "family_sale", "auction", "other_sale")
            )
          )
        ),
        
        tags$hr(),
        
        # ---- Year sold range slider ----
        uiOutput(ns("year_sold_slider_ui")),
        
        tags$hr(),
        
        # ---- Year build range slider ----
        uiOutput(ns("year_build_slider_ui")),
        
        tags$hr(),
        
        # ---- Sqm range slider ----
        uiOutput(ns("sqm_slider_ui")),
        
        tags$hr(),
        
        # ---- Room range slider ----
        uiOutput(ns("room_slider_ui")),
        
        tags$hr(),
        
        checkboxInput(
          ns("log_scale"),
          "Log-transform metric (recommended)",
          value = TRUE
        ),
        
        selectInput(
          ns("color_palette"),
          "Color palette:",
          choices = c(
            "Viridis" = "viridis",
            "Magma" = "magma",
            "Plasma" = "plasma",
            "Cividis" = "cividis"
          ),
          selected = "viridis"
        )
      ),
      
      mainPanel(
        div(
          style = "height: calc(100vh - 90px); position: relative;",
          plotOutput(ns("ridge_plot"), height = "100%")
        )
      )
    )
  )
}


# ---- SERVER ----
ai_server <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # ensure year exists
    data <- data %>%
      mutate(year = if ("year" %in% names(.)) year else as.numeric(format(as.Date(date), "%Y")))
    
    # ---- Sliders ----
    output$year_sold_slider_ui <- renderUI({
      req(data)
      years <- sort(unique(data$year))
      sliderInput(
        ns("year_sold_range"),
        "Year sold:",
        min = min(years, na.rm = TRUE),
        max = max(years, na.rm = TRUE),
        value = c(min(years, na.rm = TRUE), max(years, na.rm = TRUE)),
        step = 1,
        sep = "",
        dragRange = TRUE
      )
    })
    
    output$year_build_slider_ui <- renderUI({
      req(data)
      years <- sort(unique(data$year_build))
      sliderInput(
        ns("year_build_range"),
        "Year built:",
        min = min(years, na.rm = TRUE),
        max = max(years, na.rm = TRUE),
        value = c(min(years, na.rm = TRUE), max(years, na.rm = TRUE)),
        step = 1,
        sep = "",
        dragRange = TRUE
      )
    })
    
    output$sqm_slider_ui <- renderUI({
      req(data)
      sliderInput(
        ns("sqm_range"),
        "Square metres (sqm):",
        min = 0,
        max = ceiling(max(data$sqm, na.rm = TRUE)),
        value = c(0, ceiling(max(data$sqm, na.rm = TRUE))),
        step = 5,
        sep = "",
        dragRange = TRUE
      )
    })
    
    output$room_slider_ui <- renderUI({
      req(data)
      sliderInput(
        ns("room_range"),
        "Rooms:",
        min = 0,
        max = ceiling(max(data$no_rooms, na.rm = TRUE)),
        value = c(0, ceiling(max(data$no_rooms, na.rm = TRUE))),
        step = 1,
        sep = "",
        dragRange = TRUE
      )
    })
    
    
    # ---- Data filtering ----
    filtered <- reactive({
      df <- data
      
      if (!is.null(input$year_sold_range))
        df <- df %>% filter(year >= input$year_sold_range[1], year <= input$year_sold_range[2])
      
      if (!is.null(input$year_build_range))
        df <- df %>% filter(year_build >= input$year_build_range[1], year_build <= input$year_build_range[2])
      
      if (!is.null(input$sqm_range))
        df <- df %>% filter(sqm >= input$sqm_range[1], sqm <= input$sqm_range[2])
      
      if (!is.null(input$room_range))
        df <- df %>% filter(no_rooms >= input$room_range[1], no_rooms <= input$room_range[2])
      
      if (!is.null(input$house_type) && length(input$house_type) > 0)
        df <- df %>% filter(house_type %in% input$house_type)
      else
        df <- df %>% filter(FALSE)
      
      if (!is.null(input$sales_type) && length(input$sales_type) > 0)
        df <- df %>% filter(sales_type %in% input$sales_type)
      else
        df <- df %>% filter(FALSE)
      
      metric_col <- input$metric
      df <- df %>% filter(!is.na(.data[[metric_col]]))
      df <- df %>% mutate(metric_value = .data[[metric_col]])
      
      # sample (optional for speed)
      if (nrow(df) > 120000) set.seed(123)
      if (nrow(df) > 120000) df <- df %>% sample_n(120000)
      
      df
    })
    
    
    # ---- Ridgeline plot ----
    output$ridge_plot <- renderPlot({
      df <- filtered()
      req(df)
      
      if (nrow(df) == 0) {
        plot.new()
        title("No data matching the filters")
        return()
      }
      
      # Keep only years with enough data
      df_plot <- df %>%
        mutate(year = as.integer(year)) %>%
        group_by(year) %>%
        filter(n() >= 10) %>%
        ungroup()
      
      if (nrow(df_plot) == 0) {
        plot.new()
        title("Not enough data for chosen years (≥10 per year)")
        return()
      }
      
      # transform metric if needed
      if (input$log_scale) {
        df_plot <- df_plot %>% mutate(plot_metric = log1p(metric_value))
        x_lab <- paste0("log(", input$metric, ")")
      } else {
        df_plot <- df_plot %>% mutate(plot_metric = metric_value)
        x_lab <- input$metric
      }
      
      pal_vals <- switch(input$color_palette,
                         viridis = viridis(100, option = "C"),
                         magma = viridis(100, option = "A"),
                         plasma = viridis(100, option = "B"),
                         cividis = viridis(100, option = "D")
      )
      
      ggplot(df_plot, aes(
        x = plot_metric,
        y = factor(year),
        height = ..density..,
        group = year,
        fill = ..x..
      )) +
        geom_density_ridges_gradient(
          stat = "binline",
          bins = 60,
          scale = 1.6,
          rel_min_height = 0.01
        ) +
        scale_fill_gradientn(colors = pal_vals) +
        labs(
          title = "Distribution of Housing Metric by Year",
          x = x_lab,
          y = "Year"
        ) +
        theme_minimal(base_size = 13) +
        theme(
          axis.text.y = element_text(size = 9),
          plot.title = element_text(size = 16, face = "bold")
        )
    }, res = 96)
    
  })
}
