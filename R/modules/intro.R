# UI
intro_ui <- function(id) {
  ns <- NS(id)
  tabPanel(
    "Introduction",
    textOutput(ns("msg"))
  )
}

# Server
intro_server <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    output$msg <- renderText("Intro tab!")
  })
}
