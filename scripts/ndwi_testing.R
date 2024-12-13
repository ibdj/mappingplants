pacman::p_load(leaps) 

#### importing data ####

samples_with_sentinelndwi <- read_csv("~/Library/CloudStorage/OneDrive-Aarhusuniversitet/MappingPlants/r_generel/samples_with_sentinelndwi.csv", col_types = cols(aspect = col_skip()))

ndwi <- samples_with_sentinelndwi |> 
  select(plot_name,soil_mean,ndwi_sentinel)

#### basic plot ####
ggplot(ndwi, aes(x = soil_mean, y = ndwi_sentinel))+
  geom_point()+
  geom_smooth(method = lm, se = FALSE)

#### seting up the data for shifting #####

# Shift ndwi_sentinel
min_ndwi <- min(ndwi$ndwi_sentinel)
ndwi$ndwi_sentinel_shifted <- ndwi$ndwi_sentinel - min_ndwi + 1e-6

# Fit logarithmic model with shifted data

# Calculate R-squared
r_squared_log <- summary(log_model)$r.squared

# Generate predictions and shift back
ndwi$log_pred_shifted <- predict(log_model, newdata = ndwi)
ndwi$log_pred <- ndwi$log_pred_shifted + min_ndwi - 1e-6

#### finding the best fitting model ####
linear_model <- lm(ndwi_sentinel ~ soil_mean , data = ndwi)
poly_model_2 <- lm(ndwi_sentinel ~ poly(soil_mean, degree = 2), data = ndwi)
poly_model_3 <- lm(ndwi_sentinel ~ poly(soil_mean, degree = 3), data = ndwi)

anova(linear_model, poly_model_2, poly_model_3)
AIC(linear_model, poly_model_2, poly_model_3)

best_model <- lm(ndwi_sentinel ~ poly(soil_mean, degree = 2), data = ndwi)

par(mfrow = c(2,2))
plot(best_model)

#### plotting the poly 2 order ####
r_squared_poly2 <- summary(poly_model_2)$r.squared

# Create the plot
ggplot(ndwi, aes(x = soil_mean, y = ndwi_sentinel)) +
  geom_point(color = "blue") + # Scatter points
  stat_smooth(method = "lm", formula = y ~ poly(x, 2), se = FALSE, color = "red") + # Polynomial fit
  labs(
    title = "Polynomial Fit with R-squared",
    x = "Mean soil moisture measured (%)",
    y = "NDWI Sentinel July 2024"
  ) +
  annotate("text", x = max(ndwi$soil_mean), y = max(ndwi$ndwi_sentinel), 
           label = paste0("R² = ", round(r_squared_poly2, 2)), hjust = 1, size = 5) +
  theme_minimal()

#### plotting with linear regression ####

r_squared_linear <- summary(linear_model)$r.squared

# Create the plot
ggplot(ndwi, aes(x = soil_mean, y = ndwi_sentinel)) +
  geom_point(color = "blue") + # Scatter points
  stat_smooth(method = "lm", se = FALSE, color = "red") + # Polynomial fit
  labs(
    title = "Linear fit",
    x = "Mean soil moisture measured (%)",
    y = "NDWI Sentinel July 2024"
  ) +
  annotate("text", x = max(ndwi$soil_mean), y = max(ndwi$ndwi_sentinel), 
           label = paste0("R² = ", round(r_squared_linear, 2)), hjust = 1, size = 5) +
  theme_minimal()



