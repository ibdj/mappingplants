#### load packages ####
install.packages("pacman")
pacman::p_load(sf,terra) 


##### sampling and testing ndwi, ndmi and twi #####


#### Read points from a GeoPackage ####
points <- st_read("/Users/ibdj/Library/CloudStorage/OneDrive-Aarhusuniversitet/MappingPlants/02 Modelling future changes/survey123_data_32622.gpkg", layer = "survey")

#### reading the raster ####

# Read your rasters
raster1 <- rast("path/to/raster1.tif")
raster2 <- rast("path/to/raster2.tif")

# Create a raster stack
raster_stack <- c(raster1, raster2)

# Read your point data
points <- vect("path/to/points.shp")

# Extract values
extracted_values <- extract(raster_stack, points)

# Combine the extracted values with the original point data
result <- cbind(points, extracted_values)

