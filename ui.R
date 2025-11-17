# Load UI modules
source("R/modules/intro.R")
source("R/modules/purchase_trend.R")
source("R/modules/price_index.R")
source("R/modules/map.R")
source("R/modules/heatmap.R")
source("R/modules/data_set.R")
source("R/modules/animation.R")
source("R/modules/negotiation_plot.R")
source("R/modules/ai.R")

ui <- navbarPage(
  title = "Danish Residential Housing Prices",
  
  intro_ui("intro"),
  purchase_ui("purchase"),
  price_index_ui("price_index"),
  animation_ui("animation"),
  map_ui("map"),
  data_set_ui("data_set"),
  heatmap_ui("heatmap_vis"),
  negotiation_plot_ui("negotiation_plot_vis"),
  ai_ui("ai")
)
