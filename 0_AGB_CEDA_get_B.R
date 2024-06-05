#loading packages
require(sf);require(terra)
#adding folders
folder <- "D:/CIAT_DEFORESTATION/DATA/NEW/G/CEDA"
base <- sf::st_read("D:/CIAT_DEFORESTATION/DATA/NEW/KEN_ILRI/gadm41_KEN_0.shp")
#sf_use_s2(FALSE)

################################################################################
#AGB flags 
  #https://catalogue.ceda.ac.uk/uuid/af60720c1e404a9e9d2c145d2b2ead4e
if(!file.exists("D:/CIAT_DEFORESTATION/DATA/NEW/rasters/ILRI/AGB_2010_2020.tif")){
  x_lis <- list.files(folder,pattern = ".tif")

x_lis <- lapply(1:length(x_lis),function(i){
  x <- terra::rast(paste0(folder,"/",x_lis[[i]]))
  return(x)
})

s <- terra::sprc(x_lis)
m <- terra::merge(s)
m2 <- terra::crop(m,base)
m3 <- terra::mask(m2,base)
terra::writeRaster(m3,"D:/CIAT_DEFORESTATION/DATA/NEW/rasters/ILRI/AGB_2010_2020.tif")  
}
#https://climate.esa.int/media/documents/D4.3_CCI_PUG_V4.0_20230605.pdf
# """
# AGB=0 in both maps
# 1: AGB loss
# 2: Potential AGB loss
# 3: Improbable change
# 4: Potential AGB gain
# 5: AGB gain
# 
# """
#
################################################################################
#2020
#https://catalogue.ceda.ac.uk/uuid/af60720c1e404a9e9d2c145d2b2ead4e
if(!file.exists("D:/CIAT_DEFORESTATION/DATA/NEW/rasters/ILRI/AGB_2020.tif")){
x_lis <- list.files(paste0(folder,"/2020"),pattern = ".tif")

x_lis <- lapply(1:length(x_lis),function(i){
  x <- terra::rast(paste0(folder,"/2020/",x_lis[[i]]))
  return(x)
})

s <- terra::sprc(x_lis)
m <- terra::merge(s)
m2 <- terra::crop(m,base)
m3 <- terra::mask(m2,base)
terra::writeRaster(m3,"D:/CIAT_DEFORESTATION/DATA/NEW/rasters/ILRI/AGB_2020.tif")  
plot(m3)
}
###################################
#2010
if(!file.exists("D:/CIAT_DEFORESTATION/DATA/NEW/rasters/ILRI/AGB_2010.tif")){
x_lis <- list.files(paste0(folder,"/2010"),pattern = ".tif")

x_lis <- lapply(1:length(x_lis),function(i){
  x <- terra::rast(paste0(folder,"/2010/",x_lis[[i]]))
  return(x)
})

s <- terra::sprc(x_lis)
m <- terra::merge(s)
m2 <- terra::crop(m,base)
m3 <- terra::mask(m2,base)
terra::writeRaster(m3,"D:/CIAT_DEFORESTATION/DATA/NEW/rasters/ILRI/AGB_2010.tif")  
plot(m3)
}

################################################################################
# #dif dates
# A1 <- terra::rast("D:/CIAT_DEFORESTATION/DATA/NEW/rasters/AGB_2010.tif")  
# A2 <- terra::rast("D:/CIAT_DEFORESTATION/DATA/NEW/rasters/AGB_2020.tif")  
# 
# A_DIF <- ((A2-A1)/A1)*100
# A_DIF_1 <- A1-A2
# plot(A_DIF)
# plot(A_DIF_1)
# ################################################################################
# x_shp <- sf::st_read("D:/CIAT_DEFORESTATION/DATA/NEW/ken_admbnda_adm2_iebc_20191031.shp")
# x_shp <- sf::st_as_sf(x_shp)
# 
# #base <- sf::st_read("D:/CIAT_DEFORESTATION/DATA/NEW/ken_admbnda_adm0_iebc_20191031.shp")
# sf_use_s2(FALSE)
# #plot(st_geometry(base))
# area <- (as.numeric(st_area(x_shp)))*(1e-6/1)
# ################################################################################
# x_shp$ABG_2010_2020 <- NA
# pb <-
#   utils::txtProgressBar(min = 0,
#                         max = nrow(x_shp),
#                         style = 3)
# for(i in 1:nrow(x_shp)){
#   #calculating raster value  by county
#   x_Ext <- terra::extract(A_DIF_1,
#                           x_shp[which(x_shp$ADM2_PCODE==x_shp$ADM2_PCODE[[i]]),],
#                           na.rm = TRUE, weights = F,fun=mean,method="simple",ID=F)
#   #x_Ext <- terra::expanse(x1,unit="km",transform=T,byValue=F)
#   x_shp$ABG_2010_2020[[i]] <- as.numeric(x_Ext)
#   utils::setTxtProgressBar(pb, i)
# };rm(i)
# 
# close(pb)
