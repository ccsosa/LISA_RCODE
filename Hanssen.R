require(rgeoda);require(sf);require(terra);require(matrixStats)
data_dir  <- "D:/CIAT_DEFORESTATION/DATA/OneDrive_1_17-5-2024"
data_dir_COVER <- "D:/CIAT_DEFORESTATION/DATA/NEW"

#z <- raster::raster("D:/DESCARGAS/Hansen_GFC-2023-v1.11_lossyear_10N_030E.tif")

# x1 <- rast("https://glad.umd.edu/Potapov/TCC_2010/treecover2010_10N_030E.tif")
# x2 <- rast("https://glad.umd.edu/Potapov/TCC_2010/treecover2010_10N_040E.tif")
# x3 <- rast("https://glad.umd.edu/Potapov/TCC_2010/treecover2010_00N_030E.tif")
# x4 <- rast("https://glad.umd.edu/Potapov/TCC_2010/treecover2010_00N_040E.tif")

################################################################################
#TREE COVER
x_shp <- geodata::gadm(country = "KEN",level = 2,
                       path = "D:/CIAT_DEFORESTATION/DATA/NEW/GADM")
x_shp <- sf::st_as_sf(x_shp)
area <- (as.numeric(st_area(x_shp)))*(1e-6/1)
x_shp$area <- area
#adding adm1 and adm2 in one to join information later
x_shp$ID_NORM <- paste0(x_shp$NAME_1,"_",x_shp$NAME_2)
#save shp

base <- sf::st_read(paste0(data_dir,"/","ken_adm_iebc_20191031_shp/ken_admbnda_adm2_iebc_20191031.shp"))

if(!file.exists("D:/CIAT_DEFORESTATION/DATA/NEW/rasters/tree_cover.tif")){
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
} else {
  m3 <- terra::rast("D:/CIAT_DEFORESTATION/DATA/NEW/rasters/tree_cover.tif")
}
################################################################################
#FOREST LOSS
#lossyear_40N_080W.tif
#base <- sf::st_read(paste0(data_dir,"/","ken_adm_iebc_20191031_shp/ken_admbnda_adm2_iebc_20191031.shp"))

if(!file.exists("D:/CIAT_DEFORESTATION/DATA/NEW/rasters/lossyear.tif")){
  x1 <- terra::rast("
https://storage.googleapis.com/earthenginepartners-hansen/GFC-2023-v1.11/Hansen_GFC-2023-v1.11_lossyear_10N_030E.tif")
  x2 <- terra::rast("
https://storage.googleapis.com/earthenginepartners-hansen/GFC-2023-v1.11/Hansen_GFC-2023-v1.11_lossyear_10N_040E.tif")
  x3 <- terra::rast("
https://storage.googleapis.com/earthenginepartners-hansen/GFC-2023-v1.11/Hansen_GFC-2023-v1.11_lossyear_00N_030E.tif")
  x4 <- terra::rast("
https://storage.googleapis.com/earthenginepartners-hansen/GFC-2023-v1.11/Hansen_GFC-2023-v1.11_lossyear_00N_040E.tif")
  s <- terra::sprc(x1, x2,x3,x4)
  m <- terra::merge(s)
  m2 <- terra::crop(m,base)
  m3a <- terra::mask(m2,base)

  terra::writeRaster(m3,"D:/CIAT_DEFORESTATION/DATA/NEW/rasters/lossyear.tif")  
} else {
  m3a <- terra::rast("D:/CIAT_DEFORESTATION/DATA/NEW/rasters/lossyear.tif")
}

m4 <- m3a
# m4[which(m4[]<10)] <- NA
# m4[which(m4[]>21)] <- NA

raster_LU_List <- list()
pb <-
  utils::txtProgressBar(min = 0,
                        max = nrow(x_shp),
                        style = 3)

for(i in 1:nrow(x_shp)){
  #i <- 1
  x1 <- terra::crop(m4,x_shp[which(x_shp$ID_NORM==x_shp$ID_NORM[[i]]),],mask=T)
  x1 <- terra::mask(x1,x_shp[which(x_shp$ID_NORM==x_shp$ID_NORM[[i]]),])
  #calculating area by raster value
  x_Ext <- terra::expanse(x1,unit="km",transform=T,byValue=T)
  x_Ext$ID_NORM <- x_shp$ID_NORM[[i]]
  utils::setTxtProgressBar(pb, i)
  raster_LU_List[[i]] <- x_Ext
};rm(i)
close(pb)


raster_LU_List2 <- do.call(rbind,raster_LU_List)

write.csv(raster_LU_List2,paste0(data_dir_COVER,"/KENYA_HANSSN_VALUES_HUMDATA.csv"),row.names = F)
raster_LU_List3 <- raster_LU_List2[which(raster_LU_List2$value>10),]
raster_LU_List3 <- raster_LU_List3[which(raster_LU_List3$value<21),]


years <- unique(raster_LU_List3$value)
df_to_see <- as.data.frame(matrix(ncol=length(years)+1,nrow = nrow(x_shp)))
colnames(df_to_see) <- c("ID_NORM",years)
df_to_see[,1] <- x_shp$ID_NORM


for(i in 1:nrow(x_shp)){
  #print(paste0("i: ",i))
  x1 <- raster_LU_List3[which(raster_LU_List3$ID_NORM==x_shp$ID_NORM[[i]]),]
  for(j in 2:ncol(df_to_see)){
    #print(paste0("i: ",i," j: ",j))
    x_y_j <-x1$area[which(x1$value==years[[j-1]])]
      if(length(x_y_j)>0){
        df_to_see[i,j] <- x_y_j  
      } else {
        df_to_see[i,j] <-NA #0
      }
  };rm(j)
};rm(i)


df_to_see$mean <- rowMeans(df_to_see[,as.character(years)],na.rm = T)
df_to_see$sd <- matrixStats::rowSds(as.matrix(df_to_see[,as.character(years)]),na.rm = T)
df_to_see$median <- matrixStats::rowMedians(as.matrix(df_to_see[,as.character(years)]),na.rm = T)
write.csv(df_to_see,paste0(data_dir_COVER,"/KENYA_HANSSEN_VALUES_WIDE.csv"),row.names = F,na = "")
