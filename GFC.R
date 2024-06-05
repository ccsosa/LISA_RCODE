require(gfcanalysis);require(terra);require(sf)
base <- sf::st_read("D:/CIAT_DEFORESTATION/DATA/NEW/KEN_ILRI/gadm41_KEN_3.shp")
tiles <- calc_gfc_tiles(base)
plot(tiles)
#plot(test_poly, lt=2, add=TRUE)
out <- "D:/CIAT_DEFORESTATION/DATA/NEW/rasters/TILES_GFC"
xx <- gfcanalysis::download_tiles(tiles = tiles, 
                                  output_folder = out#,
                                  #images = "treecover2000"
                                  )

x <- gfcanalysis::extract_gfc(
  aoi=base,
  data_folder=out,
  filename="gfc_KEN_TK_extract.tif"
  #to_UTM = FALSE,
  #stack = "first"#,
  #dataset = "GFC-2022-v1.10",
#  ...
)



#gfc_data <- extract_gfc(base[which(base$GID_3==base$GID_3[[i]])], data_folder = out)

if(!file.exists("D:/CIAT_DEFORESTATION/DATA/NEW/rasters/treecover2000_KEN.tif")){
  
x <- list.files("D:/CIAT_DEFORESTATION/DATA/NEW/rasters/TILES_GFC")
x <- lapply(1:length(x),function(i){
  x <- terra::rast(paste0("D:/CIAT_DEFORESTATION/DATA/NEW/rasters/TILES_GFC","/",x[[i]]))
  return(x)

})

s <- terra::sprc(x)
m <- terra::merge(s)
m2 <- terra::crop(m,base)
m3a <- terra::mask(m2,base)

  terra::writeRaster(m3a,"D:/CIAT_DEFORESTATION/DATA/NEW/rasters/treecover2000_KEN.tif")    
} else {
  m3a <- terra::rast("D:/CIAT_DEFORESTATION/DATA/NEW/rasters/treecover2000_KEN.tif")
}

treecover2000 <- m3a
forest_threshold <- 30
forest2000 <- treecover2000 > forest_threshold

plot(forest2000)
ff <- terra::rast("D:/CIAT_DEFORESTATION/DATA/NEW/GFW/FOREST_EMMISIONS/10N_040E.tif")
ff
plot(ff)
#threshold_gfc(gfc = m3a, forest_threshold = 25)

# threshold_gfc(gfc, forest_threshold = 25, ...)