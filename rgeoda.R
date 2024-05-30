require(rgeoda);require(sf);require(terra)
data_dir  <- "D:/CIAT_DEFORESTATION/DATA/OneDrive_1_17-5-2024"

#z <- raster::raster("D:/DESCARGAS/Hansen_GFC-2023-v1.11_lossyear_10N_030E.tif")

# x1 <- rast("https://glad.umd.edu/Potapov/TCC_2010/treecover2010_10N_030E.tif")
# x2 <- rast("https://glad.umd.edu/Potapov/TCC_2010/treecover2010_10N_040E.tif")
# x3 <- rast("https://glad.umd.edu/Potapov/TCC_2010/treecover2010_00N_030E.tif")
# x4 <- rast("https://glad.umd.edu/Potapov/TCC_2010/treecover2010_00N_040E.tif")
base <- sf::st_read(paste0(data_dir,"/","ken_adm_iebc_20191031_shp/ken_admbnda_adm2_iebc_20191031.shp"))

x1 <- terra::rast("
https://storage.googleapis.com/earthenginepartners-hansen/GFC-2023-v1.11/Hansen_GFC-2023-v1.11_treecover2000_10N_030E.tif")
x2 <- terra::rast("
https://storage.googleapis.com/earthenginepartners-hansen/GFC-2023-v1.11/Hansen_GFC-2023-v1.11_treecover2000_10N_040E.tif")
x3 <- terra::rast("
https://storage.googleapis.com/earthenginepartners-hansen/GFC-2023-v1.11/Hansen_GFC-2023-v1.11_treecover2000_00N_030E.tif")
x4 <- terra::rast("
https://storage.googleapis.com/earthenginepartners-hansen/GFC-2023-v1.11/Hansen_GFC-2023-v1.11_treecover2000_00N_040E.tif")
s <- terra::sprc(x1, x2,x3,x4)
m <- terra::merge(s)
m2 <- terra::crop(m,base)
m3 <- terra::mask(m2,base)
terra::writeRaster(m3,"D:/CIAT_DEFORESTATION/DATA/NEW/rasters/tree_cover.tif")
#plot(m3)
#https://dieghernan.github.io/tidyterra/
#https://geodacenter.github.io/rgeoda/articles/rgeoda_tutorial.html#load-spatial-data
queen_w <- queen_weights(base)
summary(queen_w)
nbrs <- get_neighbors(queen_w, idx = 1)
cat("\nNeighbors of the 1-st observation are:", nbrs)
lag <- spatial_lag(queen_w, base['Shape_Area'])
lag
#https://www.paulamoraga.com/tutorial-terra/
plot(st_geometry(base),add=T,axes = TRUE)
#plot(z,add=T)
x_Ext <- terra::extract(m3, base, na.rm = TRUE, weights = TRUE)  
