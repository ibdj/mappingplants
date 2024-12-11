#### importing data ###

samples_with_sentinelndwi <- read_csv("~/Library/CloudStorage/OneDrive-Aarhusuniversitet/MappingPlants/r_generel/samples_with_sentinelndwi.csv", col_types = cols(aspect = col_skip()))

ndwi <- samples_with_sentinelndwi |> 
  select(plot_name,soil_mean,ndwi_sentinel)

ggplot(ndwi, aes(x = soil_mean, y = ndwi_sentinel))+
  geom_point()

