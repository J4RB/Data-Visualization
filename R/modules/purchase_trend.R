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
library(shinyWidgets)

# UI
purchase_ui <- function(id) {
  ns <- NS(id)
  tabPanel(
    "Purchase Trend",
    sidebarLayout(
      sidebarPanel(
        HTML("<p style='font-weight:bold; font-style:italic;text-decoration:underline'>Filter for Histogram</p>"),
        fluidRow(
          column(
            width = 6, 
            sliderInput(ns("year_range_histogram"), "Purchase year", min = min(data$year, na.rm = TRUE), max = max(data$year, na.rm = TRUE), step = 1, value = c(2000, max(data$year, na.rm = TRUE))),
          ),
          column(
            width = 6, 
            sliderInput(ns("bin_range_histogram"), "Number of bins", min = 5, max = 50, step = 1, value = 15),
          )
        ),
        fluidRow(
          column(
            width = 6, 
            pickerInput(ns("house_type_histogram"),"Select house type", choices = c(unique(data$house_type)), selected = c(unique(data$house_type)), multiple = TRUE),
          ),
          column(
            width = 6, 
            pickerInput(ns("no_rooms_histogram"),"Select no rooms", choices = sort(unique(data$no_rooms)), selected = sort(unique(data$no_rooms)), multiple = TRUE),
          )
        ),
        fluidRow(
          column(
            width = 6, 
            pickerInput(ns("region_histogram"), "Select region", choices = c(unique(data$region)), selected = c(unique(data$region)), multiple = TRUE),
          ),
          column(
            width = 6, 
            selectInput(ns("plot_by_histogram"), "Plot by", choices = plot_choices, selected = "sqm_price"),
          )
        ),
        fluidRow(
          column(
            width = 6, 
            checkboxInput(ns("color_blind_histogram"), label = "Color-blind friendly colors", value = FALSE),
          ),
          column(
            width = 6, 
            actionButton(ns("render_histogram"), "Hide Histogram"),
          )
        ),
        hr(),
        
        HTML("<p style='font-weight:bold; font-style:italic;text-decoration:underline'>Filter for Box Plot</p>"),
        fluidRow(
          column(
            width = 6, 
            numericInput(ns("max_row_box"), "Max rows to use for plots [0 means all]", value = 500, min = 100, max = 100000, step = 100),
          ),
          column(
            width = 6, 
            sliderInput(ns("year_range_box"), "Purchase year", min = min(data$year, na.rm = TRUE), max = max(data$year, na.rm = TRUE), step = 1, value = c(2020, max(data$year, na.rm = TRUE))),
          )
        ),
        fluidRow(
          column(
            width = 6, 
            pickerInput(ns("house_type_box"),"Select house type", choices = c(unique(data$house_type)), selected = c(unique(data$house_type)), multiple = TRUE),
          ),
          column(
            width = 6, 
            pickerInput(ns("region_box"), "Select region", choices = c(unique(data$region)), selected = c(unique(data$region)), multiple = TRUE),
          )
        ),
        fluidRow(
          column(
            width = 6, 
            pickerInput(ns("no_rooms_box"),"Select no rooms", choices = sort(unique(data$no_rooms)), selected = sort(unique(data$no_rooms)), multiple = TRUE),
          ),
          column(
            width = 6, 
            selectInput(ns("plot_by_box"), "Plot by", choices = plot_choices, selected = "sqm_price"),
          )
        ),
        fluidRow(
          column(
            width = 6, 
            actionButton(ns("render_box"), "Render Box Plot"),
          )
        ),
        hr(),
        
        HTML("<p style='font-weight:bold; font-style:italic;text-decoration:underline'>Filter for Bar Plot</p>"),
        fluidRow(
          column(
            width = 6, 
            sliderInput(ns("year_range_bar"), "Purchase year", min = min(data$year, na.rm = TRUE), max = max(data$year, na.rm = TRUE), step = 1, value = c(2020, max(data$year, na.rm = TRUE))),
          ),
          column(
            width = 6, 
            pickerInput(ns("house_type_bar"),"Select house type", choices = c(unique(data$house_type)), selected = c(unique(data$house_type)), multiple = TRUE),
          )
        ),
        fluidRow(
          column(
            width = 6, 
            pickerInput(ns("region_bar"), "Select region", choices = c(unique(data$region)), selected = c(unique(data$region)), multiple = TRUE),
          ),
          column(
            width = 6, 
            pickerInput(ns("no_rooms_bar"),"Select no rooms", choices = sort(unique(data$no_rooms)), selected = sort(unique(data$no_rooms)), multiple = TRUE),
          )
        ),
        fluidRow(
          column(
            width = 6, 
            selectInput(ns("plot_by_bar"), "Plot by", choices = bar_plot_choices, selected = "total_value"),
          ),
          column(
            width = 6, 
            selectInput(ns("chart_type_bar"), "Plot bar chart", choices = c("Position Dodge", "Stacked"), selected = "Stacked"),
          )
        ),
        fluidRow(
          column(
            width = 6, 
            checkboxInput(ns("color_blind_bar"), label = "Color-blind friendly colors", value = FALSE),
          ),
          column(
            width = 6, 
            actionButton(ns("render_bar"), "Render Bar Plot"),
          )
        ),
        hr(),
        
        HTML("<p style='font-weight:bold; font-style:italic;text-decoration:underline'>Filter for Scart Plot</p>"),
        fluidRow(
          column(
            width = 6, 
            numericInput(ns("max_row_scart"), "Max rows to use for plots [0 means all]", value = 500, min = 100, max = 100000, step = 100),
          ),
          column(
            width = 6, 
            sliderInput(ns("year_range_scart"), "Purchase year", min = min(data$year, na.rm = TRUE), max = max(data$year, na.rm = TRUE), step = 1, value = c(2022, max(data$year, na.rm = TRUE))),
          )
        ),
        fluidRow(
          column(
            width = 6, 
            pickerInput(ns("house_type_scart"),"Select house type", choices = c(unique(data$house_type)), selected = c(unique(data$house_type)), multiple = TRUE),
          ),
          column(
            width = 6, 
            pickerInput(ns("region_scart"), "Select region", choices = c(unique(data$region)), selected = c(unique(data$region)), multiple = TRUE),
          )
        ),
        fluidRow(
          column(
            width = 6, 
            pickerInput(ns("no_rooms_scart"),"Select no rooms", choices = sort(unique(data$no_rooms)), selected = sort(unique(data$no_rooms)), multiple = TRUE),
          ),
          column(
            width = 6, 
            selectInput(ns("plot_by_scart"), "Plot by", choices = plot_choices, selected = "sqm_price"),
          )
        ),
        fluidRow(
          column(
            width = 6, 
            selectInput(ns("chart_type_scart"), "Scart Plot", choices = c("Distribution", "Facet Wrap"), selected = "Distribution"),
          ),
          column(
            width = 6, 
            checkboxInput(ns("color_blind_scart"), label = "Color-blind friendly colors", value = FALSE),
          )
        ),
        fluidRow(
          column(
            width = 6, 
            actionButton(ns("render_scart"), "Render Scart Plot"),
          )
        ),
      ),
      
      mainPanel( 
        withSpinner(plotlyOutput(ns("histogram_point"))),
        hr(),
        withSpinner(plotlyOutput(ns("box_plot"))),
        hr(),
        withSpinner(plotlyOutput(ns("bar_plot"))),
        hr(),
        withSpinner(plotlyOutput(ns("scart_point"))),
      )
    )
  )
}

# Server
purchase_server <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    
    render_histogram_flag <- reactiveVal(1)
    render_box_flag <- reactiveVal(0)
    render_bar_flag <- reactiveVal(0)
    render_scart_flag <- reactiveVal(0)
    
    # Toggle button
    observeEvent(input$render_histogram, {
      if (render_histogram_flag() == 0) {
        render_histogram_flag(1)   # enable plot
        updateActionButton(session, "render_histogram", label = "Hide Histogram")
      } else {
        render_histogram_flag(0)   # disable plot
        updateActionButton(session, "render_histogram", label = "Show Histogram")
      }
    })
    
    # Toggle button
    observeEvent(input$render_box, {
      if (render_box_flag() == 0) {
        render_box_flag(1)   # enable plot
        updateActionButton(session, "render_box", label = "Hide Box Plot")
      } else {
        render_box_flag(0)   # disable plot
        updateActionButton(session, "render_box", label = "Show Box Plot")
      }
    })
    
    # Toggle button
    observeEvent(input$render_bar, {
      if (render_bar_flag() == 0) {
        render_bar_flag(1)   # enable plot
        updateActionButton(session, "render_bar", label = "Hide Bar Plot")
      } else {
        render_bar_flag(0)   # disable plot
        updateActionButton(session, "render_bar", label = "Show Bar Plot")
      }
    }) 
    
    # Toggle button
    observeEvent(input$render_scart, {
      if (render_scart_flag() == 0) {
        render_scart_flag(1)   # enable plot
        updateActionButton(session, "render_scart", label = "Hide Scart Plot")
      } else {
        render_scart_flag(0)   # disable plot
        updateActionButton(session, "render_scart", label = "Show Scart Plot")
      }
    })
    
    output$histogram_point <- renderPlotly({
      req(render_histogram_flag() == 1)
      
      year_1 <- input$year_range_histogram[1]
      year_2 <- input$year_range_histogram[2]
      filter_house_type <- input$house_type_histogram
      filter_region <- input$region_histogram
      filter_no_rooms <- input$no_rooms_histogram
      plot_by <- input$plot_by_histogram
      bins <- input$bin_range_histogram
      color_blind <- input$color_blind_histogram
      plot_label <- names(plot_choices)[plot_choices == plot_by]
      
      filter_data <- data %>%
        filter(year >= year_1 & year <= year_2) %>%
        {if (!is.null(filter_house_type) && length(filter_house_type) > 0) filter(., house_type %in% filter_house_type) else .} %>%
        {if (!is.null(filter_region) && length(filter_region) > 0) filter(., region %in% filter_region) else .} %>%
        {if (!is.null(filter_no_rooms) && length(filter_no_rooms) > 0) filter(., no_rooms %in% filter_no_rooms) else .}
      
      p <- ggplot(filter_data, aes(x = .data[[plot_by]]
                                   , fill = house_type
                                   , text = paste(house_type)
                                  )) +
        geom_histogram(
          alpha = 0.7, position = "identity", bins = bins
          ) +
        scale_x_continuous(labels = comma) +
        labs(
          title = paste("Distribution of Danish Housing ", plot_label, " Accross the Selected Year Range <b>",year_1,'</b> to <b>', year_2
                        , '</b>'),
          x = plot_label,
          y = "Count",
          fill = "House Type"
        ) +
        theme_minimal(base_size = 12) +
        theme(plot.title = element_text(size = plot_title))
      
        if (isTRUE(color_blind)) {
          p <- p + scale_fill_viridis_d(option = "E", begin = 0.1, end = 0.9)
        } else {
          p <- p + scale_fill_brewer(palette = color_palette)
        }
      
      ggplotly(p, tooltip = "text")
      
    })
    
    output$box_plot <- renderPlotly({
      #if (render_box_flag() == 0) return(NULL)
      req(render_box_flag() == 1)
      
      year_1 <- input$year_range_box[1]
      year_2 <- input$year_range_box[2]
      filter_house_type <- input$house_type_box
      filter_region <- input$region_box
      filter_no_rooms <- input$no_rooms_box
      plot_by <- input$plot_by_box
      max_rows <- input$max_row_box
      plot_label <- names(plot_choices)[plot_choices == plot_by]
      
      filter_data <- data %>%
        filter(year >= year_1 & year <= year_2) %>%
        {if (!is.null(filter_house_type) && length(filter_house_type) > 0) filter(., house_type %in% filter_house_type) else .} %>%
        {if (!is.null(filter_region) && length(filter_region) > 0) filter(., region %in% filter_region) else .} %>%
        {if (!is.null(filter_no_rooms) && length(filter_no_rooms) > 0) filter(., no_rooms %in% filter_no_rooms) else .}
      
      if (!is.null(max_rows) && max_rows > 0 && nrow(filter_data) > max_rows) {
        filter_data <- filter_data %>% sample_n(max_rows)
      }
      # Create box plot
      p <- ggplot(filter_data, aes(
        x = house_type,
        y = .data[[plot_by]],
        #fill = house_type,
        text = paste(
          "House Type:", house_type,
          "<br>", plot_label, ":", .data[[plot_by]],
          "<br>Region:", region
        )
      )) +
        geom_boxplot(alpha = 0.7, outlier.colour = "red", outlier.shape = 16) +
        scale_y_continuous(labels = comma) +
        labs(
          title = paste("Boxplot of Danish Housing ", plot_label, " by House Type for Year between <b>", year_1,'</b> to <b>', year_2,'</b>'),
          x = "House Type",
          y = plot_label,
          fill = "House Type"
        ) +
        theme_minimal(base_size = 12) +
        theme(plot.title = element_text(size = plot_title)) +
        guides(fill = "none")
      
      ggplotly(p, tooltip = "text")
    })
    
    output$bar_plot <- renderPlotly({
      req(render_bar_flag() == 1)
      year_1 <- input$year_range_bar[1]
      year_2 <- input$year_range_bar[2]
      filter_house_type <- input$house_type_bar
      filter_region <- input$region_bar
      filter_no_rooms <- input$no_rooms_bar
      plot_by <- input$plot_by_bar
      color_blind <- input$color_blind_bar
      chart_type <- input$chart_type_bar
      plot_label <- names(bar_plot_choices)[bar_plot_choices == input$plot_by_bar]
      
      filter_data <- data %>%
        filter(year >= year_1 & year <= year_2) %>%
        {if (!is.null(filter_house_type) && length(filter_house_type) > 0) filter(., house_type %in% filter_house_type) else .} %>%
        {if (!is.null(filter_region) && length(filter_region) > 0) filter(., region %in% filter_region) else .} %>%
        {if (!is.null(filter_no_rooms) && length(filter_no_rooms) > 0) filter(., no_rooms %in% filter_no_rooms) else .}
      
      bar_data <- filter_data %>%
        group_by(year, house_type) %>%
        summarise(
          total_value = sum(purchase_price, na.rm = TRUE),
          count = n(),
          .groups = "drop"
        )
      
      p <- ggplot(bar_data, aes(
        x = factor(year),
        y = !!sym(plot_by),
        fill = house_type,
        text = paste(
          "Year:", year,
          "<br>House Type:", house_type,
          
          if (plot_by == "total_value") {
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
          title = paste0(chart_type, " Barplot of Danish Housing ",  plot_label, " for Year between <b>", year_1,'</b> to <b>', year_2
                         ,'</b> for Different House Type')
        ) +
        theme_minimal(base_size = 14) +
        scale_y_continuous(labels = label_number(scale_cut = cut_short_scale(), accuracy = 0.1)) +
        theme(
          plot.title = element_text(size = plot_title)
          , axis.text.x = element_text(angle = 45, hjust = 1)
        )
      
      if (isTRUE(color_blind)) {
        p <- p + scale_fill_viridis_d(option = "E", begin = 0.1, end = 0.9)
      } else {
        p <- p + scale_fill_brewer(palette = color_palette)
      }
      
      if (chart_type == "Position Dodge") {
        p <- p +  geom_bar(stat = "identity", position = position_dodge(width = 0.8))
      } else {
        total_counts <- bar_data %>%
          group_by(year) %>%
          summarise(total = sum(!!sym(plot_by)), .groups = "drop")
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
    
    output$scart_point <- renderPlotly({
      req(render_scart_flag() == 1)
      
      year_1 <- input$year_range_scart[1]
      year_2 <- input$year_range_scart[2]
      filter_house_type <- input$house_type_scart
      filter_region <- input$region_scart 
      filter_no_rooms <- input$no_rooms_scart
      plot_by <- input$plot_by_scart 
      max_rows <- input$max_row_scart 
      color_blind <- input$color_blind_scart
      chart_type <- input$chart_type_scart
      plot_label <- names(plot_choices)[plot_choices == plot_by]
      
      filter_data <- data %>%
        filter(year >= year_1 & year <= year_2) %>%
        {if (!is.null(filter_house_type) && length(filter_house_type) > 0) filter(., house_type %in% filter_house_type) else .} %>%
        {if (!is.null(filter_region) && length(filter_region) > 0) filter(., region %in% filter_region) else .} %>%
        {if (!is.null(filter_no_rooms) && length(filter_no_rooms) > 0) filter(., no_rooms %in% filter_no_rooms) else .}
      
      if (!is.null(max_rows) && max_rows > 0 && nrow(filter_data) > max_rows) {
        filter_data <- filter_data %>% sample_n(max_rows)
      }
      
      mean_y <- mean(filter_data[[plot_by]], na.rm = TRUE)
      
      p <- ggplot(filter_data, aes(x = date, y = .data[[plot_by]], color = house_type))+
      #facet_wrap(~ no_rooms) +
      scale_y_continuous(
        labels = label_number(scale_cut = cut_short_scale())
      )+
      geom_point(alpha = 0.5
                 , size = .4
                 , aes(text =  paste(
                   "Date:", date,
                   "<br>", plot_label,":", .data[[plot_by]],
                   "<br>House Type:", house_type,
                   "<br>Room No:", no_rooms,
                   "<br>Region:", region
                 ))) +
      geom_jitter(width = 0.1, height = 0.1, alpha = 0.4) +
      labs(
        title = paste0("Scatterplot of Danish Housing ", plot_label," of Selected Year between <b>", year_1,'</b> to <b>', year_2
                       , "</b> for Selected House Type (<span style='font-size:", (plot_title - 2) ,"pt'>Mean: ", y_formatter(mean_y) ,"</span>)"),
        x = "Date",
        y = plot_label,
        color = "House Type"
      )+
      geom_smooth(method = "lm", se = TRUE, aes(group = house_type, color = house_type)) +
      geom_hline(yintercept = mean_y, color = "#FF6666", linetype = "dashed"
                 , size = 0.3, alpha = .5) +
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
        plot.title = element_markdown(size = plot_title),
        axis.text.x = element_text(angle = 45, hjust = 1)
      )+ 
        guides(color = guide_legend(override.aes = list(size = 3)))
      
      if (chart_type == 'Facet Wrap') {
        p <- p + facet_wrap(~ no_rooms) 
      }
      
      if (isTRUE(color_blind)) {
        p <- p + scale_color_viridis_d(option = "E", begin = 0.1, end = 0.9)
      } else {
        p <- p + scale_color_brewer(palette = color_palette)
      }
      
      ggplotly(p, tooltip = "text")%>%
        layout(legend = list(itemsizing = "constant"))
    })
  })
}
