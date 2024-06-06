rm(list = ls());gc()
#load libraries
require(readxl);require(dplyr);require(geodata);
require(sf);require(matrixStats);require(parallel);
require(ggpmisc)
require(ggplot2);require(MetBrewer);require(corrplot);
require(factoextra);require(FactoMineR);require(xlsx);require(rgeoda);library(car)
#data_dir <- "D:/CIAT_DEFORESTATION/RESULTS"

data_dir_COVER <- "D:/CIAT_DEFORESTATION/DATA/NEW"

# # pop <- terra::rast(paste0(data_dir_COVER,"/GPW/","gpw_v4_population_count_rev11_2pt5_min.nc"))
# # plot(pop)
# pop_2010 <- terra::rast(paste0(data_dir_COVER,"/GPW/","gpw_v4_population_count_rev11_2010_2pt5_min.tif"))
# pop_2015 <- terra::rast(paste0(data_dir_COVER,"/GPW/","gpw_v4_population_count_rev11_2015_2pt5_min.tif"))
# pop_2020 <- terra::rast(paste0(data_dir_COVER,"/GPW/","gpw_v4_population_count_rev11_2020_2pt5_min.tif"))
# mean_pop <- mean(pop_2010,pop_2015,pop_2020)
# median_pop <- terra::app(c(pop_2010,pop_2015,pop_2020),fun=sd)
# mean_pop_sd <- terra::app(c(pop_2010,pop_2015,pop_2020),fun=median)
# terra::writeRaster(median_pop,paste0(data_dir_COVER,"/GPW/","median_pop_2010_2020.tif"))
# # plot(pop_2010)
# plot(mean_pop)
# plot(mean_pop_sd)
# plot(median_pop)
################################################################################
x_shp <- sf::st_read("D:/CIAT_DEFORESTATION/DATA/NEW/KEN_ILRI/gadm41_KEN_3.shp")
x_shp <- sf::st_as_sf(x_shp)
LUC_R <- terra::rast("D:/CIAT_DEFORESTATION/DATA/LUC_EMISSIONS/luc_emission_mean_1961_2020_setnull.tif")
#https://human-settlement.emergency.copernicus.eu/download.php?ds=pop
################################################################################
#2010
if(!file.exists("D:/CIAT_DEFORESTATION/DATA/NEW/GHSL/GHSL_2010.tif")){
folder <- "D:/CIAT_DEFORESTATION/DATA/NEW/GHSL/2010"
x <- list.files(folder,pattern = ".tif")
x_vrt <- terra::vrt(paste0(folder,"/",x))
x_vrta  <- terra::project(x_vrt,"EPSG:4326")
x4a <- terra::crop(x_vrta,x_shp)
x4a <- terra::mask(x4a,x_shp)
#plot(x4a)
terra::writeRaster(x4a,"D:/CIAT_DEFORESTATION/DATA/NEW/GHSL/GHSL_2010.tif")
rm(x4a)
}
################################################################################
#2015
if(!file.exists("D:/CIAT_DEFORESTATION/DATA/NEW/GHSL/GHSL_2015.tif")){
folder <- "D:/CIAT_DEFORESTATION/DATA/NEW/GHSL/2015"
x <- list.files(folder,pattern = ".tif")
x_vrt <- terra::vrt(paste0(folder,"/",x))
x_vrta  <- terra::project(x_vrt,"EPSG:4326")
x4a <- terra::crop(x_vrta,x_shp)
x4a <- terra::mask(x4a,x_shp)
#plot(x4a)
terra::writeRaster(x4a,"D:/CIAT_DEFORESTATION/DATA/NEW/GHSL/GHSL_2015.tif")
rm(x4a)
}
################################################################################
#2020
if(!file.exists("D:/CIAT_DEFORESTATION/DATA/NEW/GHSL/GHSL_2020.tif")){
folder <- "D:/CIAT_DEFORESTATION/DATA/NEW/GHSL/2020"
x <- list.files(folder,pattern = ".tif")
x_vrt <- terra::vrt(paste0(folder,"/",x))
x_vrta  <- terra::project(x_vrt,"EPSG:4326")
x4a <- terra::crop(x_vrta,x_shp)
x4a <- terra::mask(x4a,x_shp)
#plot(x4a)
terra::writeRaster(x4a,"D:/CIAT_DEFORESTATION/DATA/NEW/GHSL/GHSL_2020.tif")
}
################################################################################

pop_2010 <- terra::rast("D:/CIAT_DEFORESTATION/DATA/NEW/GHSL/GHSL_2010.tif")
pop_2015 <- terra::rast("D:/CIAT_DEFORESTATION/DATA/NEW/GHSL/GHSL_2015.tif")
pop_2020 <- terra::rast("D:/CIAT_DEFORESTATION/DATA/NEW/GHSL/GHSL_2020.tif")

mean_pop <- mean(pop_2010,pop_2015,pop_2020)
median_pop <- terra::app(c(pop_2010,pop_2015,pop_2020),fun=sd)
mean_pop_sd <- terra::app(c(pop_2010,pop_2015,pop_2020),fun=median)
terra::writeRaster(median_pop,paste0(data_dir_COVER,"/GHSL/","median_pop_2010_2020.tif"))

plot(median_pop)
# pb <-
#   utils::txtProgressBar(min = 0,
#                         max = nrow(x_shp),
#                         style = 3)
# 
# #extracting livestock values per county using weighted mnean
# for(i in 1:nrow(x_shp)){
#   ####extracting cattle per county
#   x_Ext <- terra::extract(livestock,
#                           x_shp[which(x_shp$GID_3==x_shp$GID_3[[i]]),],
#                           na.rm = TRUE, weights = F,fun=mean,method="simple",ID=F)
#   x_shp$cattle_mean[[i]] <- as.numeric(x_Ext)
#   ####extracting goat per county
#   x_Ext2 <- terra::extract(livestock_goat,
#                            x_shp[which(x_shp$GID_3==x_shp$GID_3[[i]]),],
#                            na.rm = TRUE, weights = F,fun=mean,method="simple",ID=F)
#   
#   ####extracting sheep per county
#   x_Ext3 <- terra::extract(livestock_sheep,
#                            x_shp[which(x_shp$GID_3==x_shp$GID_3[[i]]),],
#                            na.rm = TRUE, weights = F,fun=mean,method="simple",ID=F) 
#   
#   
#   
#   
#   x_shp$cattle_mean[[i]] <- as.numeric(x_Ext)
#   x_shp$goat_mean[[i]] <- as.numeric(x_Ext2)
#   x_shp$sheep_mean[[i]] <- as.numeric(x_Ext3)
#   
#   
#   
#   utils::setTxtProgressBar(pb, i)
# };rm(i)
# 
# close(pb)