install.packages(c("pacman"))
pacman::p_load(raster,dynatopmodel)

dtm <- raster("path/to/your/dtm.tif")