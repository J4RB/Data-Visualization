library(shiny)
library(ggplot2)
library(plotly)
library(viridis)
library(ggtext)
library(dplyr)
library(scales)
library(gganimate)
library(gifski)
library(forcats)

# UI
animation_ui <- function(id) {
  ns <- NS(id)
  tabPanel(
    "Animation",
    sidebarLayout(
      sidebarPanel(
        checkboxInput(ns("color_blind"), label = "Enable color-blind friendly colors", value = FALSE),
        selectInput(ns("plot_by"), "Plot by", choices = bar_plot_choices, selected = "total_value"),
        sliderInput(ns("speed"), "Animation Speed (fps):", min = 5, max = 40, value = 10, step = 1)
      ),
      mainPanel(
        withSpinner(imageOutput(ns("overall_purchase"), width = "100%", height = "600px"))
      )
    )
  )
}

# Server
animation_server <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    
    output$overall_purchase <- renderImage({
      
      fps <- input$speed
      plot_by <- input$plot_by
      color_blind <- input$color_blind
      plot_label <- names(bar_plot_choices)[bar_plot_choices == input$plot_by]
    
      rank_data <- data %>%
        group_by(year, house_type) %>%
        summarise(total = ifelse(plot_by == "total_value", sum(purchase_price),n()),
                  .groups = "drop") %>%
        group_by(year) %>%
        arrange(desc(total)) %>%
        mutate(rank = row_number()) %>%
        ungroup() %>%
        mutate(house_type = factor(house_type, levels = unique(house_type)))
      
      anim <- rank_data %>%
        ggplot(aes(x = -rank, y = total, fill = house_type)) +
        geom_col(width = 0.7) +
        geom_text(aes(label = paste0(house_type, '[',y_formatter(total),']')), hjust = -0.1) +
        scale_x_continuous(expand = expansion(mult = c(0, 0.2))) +
        labs(title = paste0("Rank for Year: <b>{closest_state}</b> using <b><i>", plot_label, "</i></b>"), y = "Total Purchase Price") +
        coord_flip(clip = "off") +
        transition_states(year, transition_length = 4, state_length = 2, wrap = FALSE) +
        view_follow(fixed_x = TRUE, fixed_y = FALSE) +   # <-- Dynamic Y-axis
        #view_follow(fixed_y = TRUE, fixed_x = TRUE)+
        ease_aes('cubic-in-out') +
        theme_void() +
        theme(
          legend.position = "none",
          plot.margin = margin(0, 60, 0, 0),
          plot.title = element_markdown(size = 18),
        )
      
      if (isTRUE(color_blind)) {
        anim <- anim + scale_fill_viridis_d(option = "E")
      } else {
        anim <- anim + scale_fill_brewer(palette = "Set2")
      }
      
      
      anim_file <- tempfile(fileext = ".gif")
      
      animate(anim, nframes = 400, fps = fps, renderer = gifski_renderer(anim_file))
      
      
      # Return GIF
      list(src = anim_file, contentType = 'image/gif')
      
    }, deleteFile = TRUE)
    
  })
}
