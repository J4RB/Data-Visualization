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
          "Select metric to display:",
          choices = c(
            "Square meter price (DKK/m²)" = "sqm_price",
            "Purchase price (DKK)" = "purchase_price",
            "House size (sqm)" = "sqm",
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
        
        # ---- Year range slider ----
        uiOutput(ns("year_slider_ui")),
        tags$hr(),
        
        # ---- Filter: House type ----
        checkboxGroupInput(
          ns("house_type"),
          "Filter by house type:",
          choices = c(
            "Villa" = "Villa",
            "Townhouse" = "Townhouse",
            "Apartment" = "Apartment",
            "Summerhouse" = "Summerhouse",
            "Farm" = "Farm"
          ),
          selected = c("Villa", "Townhouse", "Apartment", "Summerhouse", "Farm")
        ),
        
        # ---- Filter: Sales type ----
        checkboxGroupInput(
          ns("sales_type"),
          "Filter by sales type:",
          choices = c(
            "Regular Sale" = "regular_sale",
            "Family Sale"  = "family_sale",
            "Auction"      = "auction",
            "Other Sale"   = "other_sale"
          ),
          selected = c("regular_sale", "family_sale", "auction", "other_sale")
        ),
        
        # ---- Tip: Shown when filters are empty ----
        uiOutput(ns("filter_tip"))
      ),
      
      mainPanel(
        leaflet::leafletOutput(ns("map"), height = "900px")
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
    
    # ---- Year slider UI ----
    output$year_slider_ui <- renderUI({
      req(data)
      years <- sort(unique(data$year))
      sliderInput(
        ns("year_range"),
        "Select year range:",
        min = min(years, na.rm = TRUE),
        max = max(years, na.rm = TRUE),
        value = c(min(years, na.rm = TRUE), max(years, na.rm = TRUE)),
        step = 1,
        sep = "",
        dragRange = TRUE
      )
    })
    
    # ---- Dynamic tip when filters are empty ----
    output$filter_tip <- renderUI({
      if ((is.null(input$house_type) || length(input$house_type) == 0) ||
          (is.null(input$sales_type) || length(input$sales_type) == 0)) {
        div(
          style = "color: #b30000; font-weight: bold; margin-top: 10px;",
          "Tip: Select at least one house type and one sales type to display data."
        )
      } else {
        NULL
      }
    })
    
    # ---- Reactive merged data ----
    merged <- reactive({
      req(data, input$metric)
      metric <- input$metric
      filtered <- data
      
      # ---- Filter by year ----
      if (!is.null(input$year_range) && all(!is.na(input$year_range))) {
        filtered <- filtered %>%
          filter(year >= input$year_range[1], year <= input$year_range[2])
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
      if (input$agg_level == "zip") {
        if (nrow(filtered) == 0) {
          df <- tibble(zip_code = character(), avg_value = numeric(), n = integer())
        } else {
          df <- filtered %>%
            mutate(zip_code = as.character(zip_code)) %>%
            group_by(zip_code) %>%
            summarise(avg_value = mean(.data[[metric]], na.rm = TRUE), n = n(), .groups = "drop")
        }
        dk_zip_sf %>%
          left_join(df, by = c("POSTNR_TXT" = "zip_code")) %>%
          mutate(
            display_value = ifelse(is.na(avg_value), "No data", round(avg_value, 2)),
            n = ifelse(is.na(n), 0, n),
            color_value = ifelse(is.na(avg_value), NA, avg_value)
          )
      } else {
        # ---- region aggregation ----
        if (nrow(filtered) == 0) {
          df <- tibble(geojson_region = character(), avg_value = numeric(), n = integer())
        } else {
          df <- filtered %>%
            group_by(geojson_region) %>%
            summarise(avg_value = mean(.data[[metric]], na.rm = TRUE), n = n(), .groups = "drop")
        }
        geojson_regions %>%
          left_join(df, by = c("REGIONNAVN" = "geojson_region")) %>%
          mutate(
            display_value = ifelse(is.na(avg_value), "No data", round(avg_value, 2)),
            n = ifelse(is.na(n), 0, n),
            color_value = ifelse(is.na(avg_value), NA, avg_value)
          )
      }
    })
    
    # ---- Helper function to render map ----
    render_map <- function(merged_data, metric_label, agg_level) {
      merged_data <- merged_data %>%
        st_make_valid() %>%
        st_zm(drop = TRUE, what = "ZM") %>%
        filter(!st_is_empty(geometry))
      
      merged_data$color_value <- suppressWarnings(as.numeric(merged_data$color_value))
      merged_data$color_value[is.infinite(merged_data$color_value)] <- NA
      finite_vals <- merged_data$color_value[is.finite(merged_data$color_value)]
      
      leaf <- leaflet(merged_data) %>%
        addTiles() %>%
        setView(lng = 10.0, lat = 56.0, zoom = 6)
      
      if (length(finite_vals) < 2) {
        leaf <- leaf %>%
          addPolygons(
            color = "#333333",
            weight = 1,
            fillOpacity = 0,
            highlightOptions = highlightOptions(weight = 2, color = "black", bringToFront = TRUE),
            popup = ~paste0("<strong>", ifelse(agg_level == "zip", "ZIP: ", "Region: "), "</strong>", 
                            ifelse(agg_level == "zip", POSTNR_TXT, REGIONNAVN),
                            "<br><em>No data</em>")
          )
      } else {
        pal <- colorNumeric("YlOrRd", domain = finite_vals, na.color = "transparent")
        leaf <- leaf %>%
          addPolygons(
            fillColor = ~pal(color_value),
            color = "#333333",
            weight = 1,
            fillOpacity = 0.7,
            highlightOptions = highlightOptions(weight = 2, color = "black", bringToFront = TRUE),
            popup = ~paste0(
              "<strong>", ifelse(agg_level == "zip", "ZIP: ", "Region: "), "</strong>", 
              ifelse(agg_level == "zip", POSTNR_TXT, REGIONNAVN),
              "<br><strong>Average ", metric_label, ": </strong>", display_value,
              "<br><strong>Sales records: </strong>", n
            )
          ) %>%
          addLegend(
            position = "bottomright",
            pal = pal,
            values = finite_vals,
            title = paste("Average", metric_label),
            opacity = 1
          )
      }
      
      leaf
    }
    
    # ---- Render map ----
    output$map <- renderLeaflet({
      merged_data <- merged()
      render_map(merged_data, input$metric, input$agg_level)
    })
    
    # ---- React to filters ----
    observeEvent(
      {
        list(input$metric, input$house_type, input$sales_type, input$year_range, input$agg_level)
      },
      {
        merged_data <- merged()
        output$map <- renderLeaflet({
          render_map(merged_data, input$metric, input$agg_level)
        })
      }
    )
  })
}
