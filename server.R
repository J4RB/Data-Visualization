library(shiny)

# Load server modules
source("R/modules/intro.R")
source("R/modules/visualization.R")

server <- function(input, output, session) {
  # Import data
  data <- read.csv("Data/DKHousingPricesSample100k.csv")
  
  # Call each module’s server logic
  intro_server("intro", data)
  visualization_server("visualization", data)
}