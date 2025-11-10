library(shiny)
library(leaflet)
library(dplyr)
library(sf)

# ---- UI ----
map_ui <- function(id) {
  ns <- NS(id)
  tabPanel(
    "Map",
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
        
        # ---- Aggregation level ----
        radioButtons(
          ns("agg_level"),
          "Aggregate by:",
          choices = c("ZIP code" = "zip", "Region" = "region"),
          selected = "zip"
        ),
        tags$hr(),
        
        fluidRow(
          column(6,
                 # ---- Filter: House type ----
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
          column(6,
                 # ---- Filter: Sales type ----
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
        
        # ---- Year build range slider ----
        uiOutput(ns("year_build_slider_ui")),
        
        # ---- Sqm range slider ----
        uiOutput(ns("sqm_slider_ui")),
        
        # ---- Room range slider ----
        uiOutput(ns("room_slider_ui")),
        
        # ---- Tip: Shown when filters are empty ----
        uiOutput(ns("filter_tip"))
      ),
      
      mainPanel(
        div(
          style = "height: calc(100vh - 90px);",  # subtract some space for padding/header
          leafletOutput(ns("map"), height = "100%")
        )
      )
    )
  )
}

# ---- SERVER ----
map_server <- function(id, data, dk_zip_sf, geojson_regions) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # ---- Ensure 'year' column exists ----
    data <- data %>%
      mutate(year = if ("year" %in% names(.)) year else as.numeric(format(as.Date(date), "%Y")))
    
    # ---- Map areas to geojson regions ----
    data <- data %>%
      mutate(
        geojson_region = case_when(
          area %in% c("Capital, Copenhagen", "North Zealand", "Bornholm") ~ "Region Hovedstaden",
          area == "North jutland" ~ "Region Nordjylland",
          area == "East & mid jutland" ~ "Region Midtjylland",
          area %in% c("South jutland", "Fyn & islands") ~ "Region Syddanmark",
          area == "Other islands" ~ "Region Sjælland",
          TRUE ~ NA_character_
        )
      )
    
    # ---- Prepare ZIP shapefile ----
    dk_zip_sf <- dk_zip_sf %>%
      st_zm(drop = TRUE, what = "ZM") %>%
      filter(!st_is_empty(geometry)) %>%
      mutate(POSTNR_TXT = as.character(POSTNR_TXT))
    
    # ---- Prepare regions geojson ----
    geojson_regions <- geojson_regions %>%
      st_make_valid() %>%
      filter(!st_is_empty(geometry))
    
    # ---- Year sold slider UI ----
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
    
    # ---- Year build slider UI ----
    output$year_build_slider_ui <- renderUI({
      req(data)
      years_build <- sort(unique(data$year_build))
      sliderInput(
        ns("year_build_range"),
        "Year build:",
        min = min(years_build, na.rm = TRUE),
        max = max(years_build, na.rm = TRUE),
        value = c(min(years_build, na.rm = TRUE), max(years_build, na.rm = TRUE)),
        step = 1,
        sep = "",
        dragRange = TRUE
      )
    })
    
    # ---- Sqm slider UI ----
    output$sqm_slider_ui <- renderUI({
      req(data)
      sqm_vals <- sort(unique(data$sqm))
      sliderInput(
        ns("sqm_range"),
        "Square metre:",
        min = 0,
        max = max(sqm_vals, na.rm = TRUE),
        value = c(0, max(sqm_vals, na.rm = TRUE)),
        step = 10,
        sep = "",
        dragRange = TRUE
      )
    })
    
    # ---- Room slider UI ----
    output$room_slider_ui <- renderUI({
      req(data)
      room_vals <- sort(unique(data$no_rooms))
      sliderInput(
        ns("room_range"),
        "Rooms:",
        min = 1,
        max = max(room_vals, na.rm = TRUE),
        value = c(1, max(room_vals, na.rm = TRUE)),
        step = 1,
        sep = "",
        dragRange = TRUE
      )
    })
    
    # ---- Reactive merged data ----
    merged <- reactive({
      req(data, input$metric)
      metric <- input$metric
      filtered <- data
      
      # ---- Filter by year sold ----
      if (!is.null(input$year_sold_range) && all(!is.na(input$year_sold_range))) {
        filtered <- filtered %>%
          filter(year >= input$year_sold_range[1], year <= input$year_sold_range[2])
      }
      
      # ---- Filter by year build ----
      if (!is.null(input$year_build_range) && all(!is.na(input$year_build_range))) {
        filtered <- filtered %>%
          filter(year_build >= input$year_build_range[1], year_build <= input$year_build_range[2])
      }
      
      # ---- Filter by square meters ----
      if (!is.null(input$sqm_range) && all(!is.na(input$sqm_range))) {
        filtered <- filtered %>%
          filter(sqm >= input$sqm_range[1], sqm <= input$sqm_range[2])
      }

      # ---- Filter by number of rooms ----
      if (!is.null(input$room_range) && all(!is.na(input$room_range))) {
        filtered <- filtered %>%
          filter(no_rooms >= input$room_range[1], no_rooms <= input$room_range[2])
      }
            
      # ---- Filter by house type ----
      if (!is.null(input$house_type) && length(input$house_type) > 0) {
        filtered <- filtered %>% filter(house_type %in% input$house_type)
      } else {
        filtered <- filtered %>% filter(FALSE)
      }
      
      # ---- Filter by sales type ----
      if (!is.null(input$sales_type) && length(input$sales_type) > 0) {
        filtered <- filtered %>% filter(sales_type %in% input$sales_type)
      } else {
        filtered <- filtered %>% filter(FALSE)
      }
      
      # ---- Aggregate based on selected level ----
        # ---- zip aggregation ----
      if (input$agg_level == "zip") {
        if (nrow(filtered) == 0) {
          df_zip <- tibble(zip_code = character(), avg_value = numeric(), n = integer())
        } else {
          df_zip <- filtered %>%
            mutate(zip_code = as.character(zip_code)) %>%
            group_by(zip_code) %>%
            summarise(avg_value = mean(.data[[metric]], na.rm = TRUE), n = n(), .groups = "drop")
        }
        dk_zip_sf %>%
          left_join(df_zip, by = c("POSTNR_TXT" = "zip_code")) %>%
          mutate(
            display_value = ifelse(is.na(avg_value), "No data", round(avg_value, 2)),
            n = ifelse(is.na(n), 0, n),
            color_value = ifelse(is.na(avg_value), NA, avg_value)
          )
      } else {
        # ---- region aggregation ----
        if (nrow(filtered) == 0) {
          df_region <- tibble(geojson_region = character(), avg_value = numeric(), n = integer())
        } else {
          df_region <- filtered %>%
            group_by(geojson_region) %>%
            summarise(avg_value = mean(.data[[metric]], na.rm = TRUE), n = n(), .groups = "drop")
        }
        geojson_regions %>%
          left_join(df_region, by = c("REGIONNAVN" = "geojson_region")) %>%
          mutate(
            display_value = ifelse(is.na(avg_value), "No data", round(avg_value, 2)),
            n = ifelse(is.na(n), 0, n),
            color_value = ifelse(is.na(avg_value), NA, avg_value)
          )
      }
    })
    
    # ---- Initial render of the map ----
    output$map <- renderLeaflet({
      leaflet() %>%
        addTiles() %>%
        setView(lng = 12.0, lat = 56.5, zoom = 7)
    })
    
    # ---- Update polygons & legend ----
    update_map <- function(merged_data, metric_label, agg_level) {
      merged_data <- merged_data %>%
        st_make_valid() %>%
        st_zm(drop = TRUE, what = "ZM") %>%
        filter(!st_is_empty(geometry))
      
      merged_data$color_value <- suppressWarnings(as.numeric(merged_data$color_value))
      merged_data$color_value[is.infinite(merged_data$color_value)] <- NA
      finite_vals <- merged_data$color_value[is.finite(merged_data$color_value)]
      
      pal <- colorNumeric("YlOrRd", domain = finite_vals, na.color = "transparent")
      
      merged_data <- merged_data %>%
        mutate(popup_content = if (agg_level == "zip") {
          paste0("<strong>ZIP: </strong>", POSTNR_TXT,
                 "<br><strong>Average ", metric_label, ": </strong>", display_value,
                 "<br><strong>Sales records: </strong>", n)
        } else {
          paste0("<strong>Region: </strong>", REGIONNAVN,
                 "<br><strong>Average ", metric_label, ": </strong>", display_value,
                 "<br><strong>Sales records: </strong>", n)
        })
      
      leafletProxy("map") %>%
        clearShapes() %>%
        clearControls() %>%
        addPolygons(
          data = merged_data,
          fillColor = ~pal(color_value),
          color = "#333333",
          weight = 1,
          fillOpacity = 0.7,
          highlightOptions = highlightOptions(weight = 2, color = "black", bringToFront = TRUE),
          popup = ~popup_content
        ) %>%
        addLegend(
          "bottomright",
          pal = pal,
          values = finite_vals,
          title = paste("Average", metric_label),
          opacity = 1
        )
    }
    
    # ---- React to filter changes ----
    observeEvent(
      {
        list(input$metric, input$house_type, input$sales_type, input$year_sold_range,
             input$year_build_range, input$sqm_range, input$room_range, input$agg_level)
      },
      {
        req(merged())
        update_map(merged(), input$metric, input$agg_level)
      }
    )
  })
}
