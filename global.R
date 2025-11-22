# global.R
library(shiny)
library(leaflet)
library(sf)
library(geojsonio)
library(dplyr)
library(lubridate)
library(scales)

# Load Danish postal code polygons
dk_zip_sf <- sf::st_read("Data/postnumre.geojson")
dk_zip_sf$POSTNR_TXT <- as.character(dk_zip_sf$POSTNR_TXT)

# Load Danish region polygons
dk_region_sf <- sf::st_read("Data/regioner.geojson")
dk_region_sf$REGIONNAVN <- as.character(dk_region_sf$REGIONNAVN)

# Load main housing data once
data <- read.csv("Data/DKHousingPricesSample100k.csv", stringsAsFactors = FALSE)

data$date <- as.Date(data$date)
data$year <- year(data$date)

# global.R
plot_choices <- c(
  "Purchase Price" = "purchase_price",
  "Prices per m²"      = "sqm_price"
)

bar_plot_choices <- c(
  "Purchase Price" = "total_value",
  "No of sales"      = "count"
)

bc <- "#377eb8"
y_formatter <- scales::label_number(scale_cut = scales::cut_short_scale(), accuracy = 0.1)

plot_title <- 11
color_palette <-"Dark2"