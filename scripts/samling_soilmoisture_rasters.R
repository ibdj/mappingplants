#### load packages ####
install.packages("pacman")
pacman::p_load(sf,terra, tidyverse) 


##### sampling and testing ndwi, ndmi and twi #####


#### Read points from a GeoPackage ####
points <- st_read("/Users/ibdj/Library/CloudStorage/OneDrive-Aarhusuniversitet/MappingPlants/02 Modelling future changes/survey123_data_32622.gpkg", layer = "survey")

#### reading the raster ####

# Read your rasters
raster1 <- rast("data/ndmi_Sentinel2_20230724.tif")
raster2 <- rast("data/ndwi_Sentinel1_2024.tif")

# Crop to the intersection of both rasters
common_extent <- intersect(ext(raster1), ext(raster2))
raster1_cropped <- crop(raster1, common_extent)
raster2_cropped <- crop(raster2, common_extent)

# Create a raster stack
raster_stack <- c(raster1_cropped, raster2_cropped)

# Extract values
extracted_values <- extract(raster_stack, points)

colnames(extracted_values) <- c("ID", "ndmi", "ndwi")

# Combine the extracted values with the original point data
result <- cbind(points, extracted_values)

#### plot the rasters ####
plot(raster1_cropped, main="ndmi")
plot(raster2_cropped, main="ndwi")
#### basic plotting ####

ggplot(result)+
  geom_point(aes(x = soil_mean, y = ndwi))

ggplot(result)+
  geom_point(aes(x = soil_mean, y = ndmi))

#### correlation for ndwi ####

linear_model <- lm(ndwi ~ soil_mean , data = result)
poly_model_2 <- lm(ndwi ~ poly(soil_mean, degree = 2), data = result)
poly_model_3 <- lm(ndwi ~ poly(soil_mean, degree = 3), data = result)

anova(linear_model, poly_model_2, poly_model_3)
AIC(linear_model, poly_model_2, poly_model_3)

best_model <- lm(ndwi ~ poly(soil_mean, degree = 2), data = result)
par(mfrow = c(2,2))
plot(best_model)

r_squared_poly2 <- summary(best_model)$r.squared

# Create the plot
ggplot(result, aes(x = soil_mean, y = ndwi)) +
  geom_point(color = "blue") + # Scatter points
  stat_smooth(method = "lm", formula = y ~ poly(x, 2), se = FALSE, color = "red") + # Polynomial fit
  stat_smooth(method = "lm", se = FALSE, color = "red") + # linear fit
  labs(
    title = "Polynomial Fit with R-squared",
    x = "Mean soil moisture measured (%)",
    y = "NDWI Sentinel July 2024"
  ) +
  annotate("text", x = max(result$soil_mean), y = max(result$ndwi_sentinel), 
           label = paste0("Poly 2, R² = ", round(r_squared_poly2, 2)), hjust = 1, size = 5) +
  annotate("text", x = max(result$soil_mean), y = max(result$ndwi), 
           label = paste0("linear R² = ", round(r_squared_linear, 2)), hjust = 1, size = 5) +
  theme_minimal()

# plot for linear fit with ndwi

r_squared_linear <- summary(linear_model)$r.squared

# Create the plot
ggplot(result, aes(x = soil_mean, y = ndwi)) +
  geom_point(color = "blue") + # Scatter points
  stat_smooth(method = "lm", se = FALSE, color = "red") + # linear fit
  labs(
    title = "Linear fit",
    x = "Mean soil moisture measured (%)",
    y = "NDWI Sentinel July 2024"
  ) +
  annotate("text", x = max(result$soil_mean), y = max(result$ndwi), 
           label = paste0("linear R² = ", round(r_squared_linear, 2)), hjust = 1, size = 5) +
  theme_minimal()
