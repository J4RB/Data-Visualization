library(shiny)
library(DT)

# UI
data_set_ui <- function(id) {
  ns <- NS(id)
  tabPanel(
    "Data Set",
    mainPanel(
      DTOutput(ns("data_table")),
      textOutput(ns("msg"))
    )
  )
}

# Server
data_set_server <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    output$data_table <- renderDT({
      datatable(
        data,
        options = list(
          pageLength = 25,     # show 25 rows per page
          scrollX = TRUE,      # enable horizontal scroll if many columns
          autoWidth = TRUE,
          scrollY = '70vh',
        )
      )
    })
    
    
    output$msg <- renderText("Intro tab!")
    
  })
}
