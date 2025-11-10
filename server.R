# Load server modules
source("R/modules/intro.R")
source("R/modules/visualization.R")
source("R/modules/map.R")
source("R/modules/heatmap.R")

server <- function(input, output, session) {
  # Call each module’s server logic
  intro_server("intro", data)
  visualization_server("visualization", data)
  map_server("map", data, dk_zip_sf, dk_region_sf)
  heatmap_server("heatmap_vis", data)
}