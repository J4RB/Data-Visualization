# Load UI modules
source("R/modules/intro.R")
source("R/modules/purchase_trand.R")
source("R/modules/price_index.R")
source("R/modules/visualization.R")
source("R/modules/map.R")
source("R/modules/data_set.R")

ui <- navbarPage(
  title = "[Title]",
  
  intro_ui("intro"),
  purchase_ui("purchase"),
  price_index_ui("price_index"),
  visualization_ui("visualization"),
  map_ui("map"),
  data_set_ui("data_set")
)
