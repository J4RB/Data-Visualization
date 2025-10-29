# global.R
library(shiny)
library(leaflet)
library(sf)
library(geojsonio)
library(dplyr)

# Load Danish postal code polygons (TopoJSON)
dk_zip_sf <- sf::st_read("Data/postnumre.geojson")
dk_zip_sf$POSTNR_TXT <- as.character(dk_zip_sf$POSTNR_TXT)

# Load main housing data once
data <- read.csv("Data/DKHousingPricesSample100k.csv", stringsAsFactors = FALSE)
# Ensure ZIP codes are character and 4-digit
data$zip_code <- sprintf("%04s", data$zip_code)