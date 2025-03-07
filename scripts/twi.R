install.packages(c("pacman"))
pacman::p_load(raster,dynatop)

dtm <- raster("data/Kobbefjord_2015_DTM_50cm.tif")

slope <- terrain(dtm, opt = "slope", unit = "radians")
flow_dir <- terrain(dtm, opt = "flowdir")
flow_acc <- raster::area(flow_dir)

twi <- log((flow_acc + 1) / (tan(slope) + 0.01))

plot(twi)

twi_arcticdem <- raster(file.choose())

plot(twi_arcticdem)
plot(twi)
