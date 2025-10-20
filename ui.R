library(shiny)

# Load UI modules
source("R/modules/intro.R")
source("R/modules/visualization.R")

ui <- navbarPage(
  title = "[Title]",
  
  intro_ui("intro"),
  visualization_ui("visualization")
)
