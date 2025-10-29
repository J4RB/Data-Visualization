# global.R
library(shiny)
library(leaflet)
library(sf)
library(geojsonio)
library(dplyr)

# Load Danish postal code polygons
dk_zip_sf <- sf::st_read("Data/postnumre.geojson")
dk_zip_sf$POSTNR_TXT <- as.character(dk_zip_sf$POSTNR_TXT)

# Load Danish region polygons
dk_region_sf <- sf::st_read("Data/regioner.geojson")
dk_region_sf$REGIONNAVN <- as.character(dk_region_sf$REGIONNAVN)

# Load main housing data once
data <- read.csv("Data/DKHousingPricesSample100k.csv", stringsAsFactors = FALSE)