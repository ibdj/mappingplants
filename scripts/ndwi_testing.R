#### importing data ###

samples_with_sentinelndwi <- read_csv("~/Library/CloudStorage/OneDrive-Aarhusuniversitet/MappingPlants/r_generel/samples_with_sentinelndwi.csv", col_types = cols(aspect = col_skip()))

ndwi <- samples_with_sentinelndwi |> 
  select(plot_name,soil_mean,ndwi_sentinel)

#### basic plot ####
ggplot(ndwi, aes(x = soil_mean, y = ndwi_sentinel))+
  geom_point()+
  geom_smooth(method = lm, se = FALSE)

lm(ndwi_sentinel ~ soil_mean , data = ndwi)

lm(ndwi_sentinel ~ poly(soil_mean, degree = 2), data = ndwi)
