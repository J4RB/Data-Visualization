# Load UI modules
source("R/modules/intro.R")
source("R/modules/visualization.R")
source("R/modules/map.R")
source("R/modules/heatmap.R")

ui <- navbarPage(
  title = "[Title]",
  
  intro_ui("intro"),
  visualization_ui("visualization"),
  map_ui("map"),
  heatmap_ui("heatmap_vis")
)
