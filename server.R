# Load server modules
source("R/modules/intro.R")
source("R/modules/purchase_trand.R")
source("R/modules/price_index.R")
source("R/modules/visualization.R")
source("R/modules/map.R")
source("R/modules/data_set.R")
source("R/modules/animation.R")
source("R/modules/heatmap.R")

server <- function(input, output, session) {
  # Call each module’s server logic
  intro_server("intro", data)
  purchase_server("purchase", data)
  price_index_server("price_index", data)
  visualization_server("visualization", data)
  map_server("map", data, dk_zip_sf, dk_region_sf)
  heatmap_server("heatmap_vis", data)
  data_set_server("data_set", data)
  animation_server("animation", data)
}