pacman::p_load(leaps) 

#### importing data ###

samples_with_sentinelndwi <- read_csv("~/Library/CloudStorage/OneDrive-Aarhusuniversitet/MappingPlants/r_generel/samples_with_sentinelndwi.csv", col_types = cols(aspect = col_skip()))

ndwi <- samples_with_sentinelndwi |> 
  select(plot_name,soil_mean,ndwi_sentinel)

#### basic plot ####
ggplot(ndwi, aes(x = soil_mean, y = ndwi_sentinel))+
  geom_point()+
  geom_smooth(method = lm, se = FALSE)

#### finding the best fitting model ####
linear_model <- lm(ndwi_sentinel ~ soil_mean , data = ndwi)
poly_model_2 <- lm(ndwi_sentinel ~ poly(soil_mean, degree = 2), data = ndwi)
poly_model_3 <- lm(ndwi_sentinel ~ poly(soil_mean, degree = 3), data = ndwi)


anova(linear_model, poly_model_2, poly_model_3)
AIC(linear_model, poly_model_2, poly_model_3)

best_model <- lm(ndwi_sentinel ~ poly(soil_mean, degree = 2), data = ndwi)

par(mfrow = c(2,2))
plot(best_model)
