# UI
visualization_ui <- function(id) {
  ns <- NS(id)
  tabPanel(
    "Visualization",
    textOutput(ns("msg"))
  )
}

# Server
visualization_server <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    output$msg <- renderText("Visualization tab!")
  })
}
