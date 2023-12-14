
# Load required packages
library(shiny)
library(tidyverse)
library(sf)
library(leaflet)
library(shinythemes)
library(shinycssloaders)
library(viridis)
library(rsconnect)

setwd('materials')

# Read data
ward_solar <- read.csv('ward_solar.csv')
zipcode_solar <- read.csv('zipcode_solar.csv')

ward_solar_time <- read.csv('ward_solar_time.csv')
zipcode_solar_time <- read.csv('zipcode_solar_time.csv')


cama_build_solar <- read.csv('solar_cama_building.csv')
build_solar <- read.csv('solar_building.csv')


cama_build_solar$`Capacity (MW)` = cama_build_solar$`Capacity..MW.`
cama_build_solar$`Facility Address` = cama_build_solar$`clean_address_solar`

ward_solar$perc_solar = ward_solar$perc_solar*100
zipcode_solar$perc_solar = zipcode_solar$perc_solar*100

# A function to round all numeric columns in a dataframe
round_df <- function(df) {
  df[] <- lapply(df, function(x) {
    if(is.numeric(x)) round(x, 2) else x
  })
  return(df)
}

# Apply the function to each dataframe
build_solar <- round_df(build_solar)
cama_build_solar <- round_df(cama_build_solar)
zipcode_solar <- round_df(zipcode_solar)
ward_solar <- round_df(ward_solar)

# Convert the WKT column back to geometry
# polygons for buildings, wards, zipcodes
ward_solar$geometry <- st_as_sfc(ward_solar$geom_ward, crs = 4326)
zipcode_solar$geometry <- st_as_sfc(zipcode_solar$geom_zip, crs = 4326)
cama_build_solar$geometry <- st_as_sfc(cama_build_solar$geom_build, crs = 4326)
build_solar$geometry <- st_as_sfc(build_solar$geom_build, crs = 4326)


# Create spatial dataframes
ward_sf <- st_as_sf(ward_solar)
zipcode_sf <- st_as_sf(zipcode_solar)
cama_build_solar_sf <- st_as_sf(cama_build_solar)
build_solar_sf <- st_as_sf(build_solar)


#plot and check
leaflet(zipcode_sf) %>% 
  addTiles() %>% 
  addPolygons()


#export rds
saveRDS(ward_sf, 'ward_solar.rds')
saveRDS(zipcode_sf, 'zipcode_solar.rds')
saveRDS(cama_build_solar_sf, 'build_solar_cama.rds')
saveRDS(build_solar_sf, 'build_solar.rds')
saveRDS(ward_solar_time, 'ward_solar_time.rds')
saveRDS(zipcode_solar_time, 'zipcode_solar_time.rds')

