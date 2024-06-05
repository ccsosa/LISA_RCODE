rm(list = ls());gc()
#load libraries
require(readxl);require(dplyr);require(geodata);require(sf);require(terra);require(matrixStats);require(parallel)
#require(rgeoda);require(sf);require(terra)
#getting folder
data_dir  <- "D:/CIAT_DEFORESTATION/DATA/OneDrive_1_17-5-2024"
#load data
load("D:/CIAT_DEFORESTATION/RESULTS/2_FOREST_PROCAREA.RData")
#load livestock 
livestock <- terra::rast(paste0(data_dir,"/","5_Ct_2015_Da.tif"))
livestock_goat <- terra::rast("D:/CIAT_DEFORESTATION/DATA/NEW/LIVESTOCK/GOATS_5_Gt_2015_Da.tif")
livestock_sheep <- terra::rast("D:/CIAT_DEFORESTATION/DATA/NEW/LIVESTOCK/SHEEP_Sh_2015_Da.tif")
#creating empty variable
x_shp$cattle_mean <- NA
x_shp$goat_mean <- NA
x_shp$sheep_mean <- NA

pb <-
  utils::txtProgressBar(min = 0,
                        max = nrow(x_shp),
                        style = 3)

#extracting livestock values per county using weighted mnean
for(i in 1:nrow(x_shp)){
  ####extracting cattle per county
  x_Ext <- terra::extract(livestock,
                          x_shp[which(x_shp$GID_3==x_shp$GID_3[[i]]),],
                          na.rm = TRUE, weights = F,fun=mean,method="simple",ID=F)
  x_shp$cattle_mean[[i]] <- as.numeric(x_Ext)
  ####extracting goat per county
  x_Ext2 <- terra::extract(livestock_goat,
                          x_shp[which(x_shp$GID_3==x_shp$GID_3[[i]]),],
                          na.rm = TRUE, weights = F,fun=mean,method="simple",ID=F)
  
  ####extracting sheep per county
  x_Ext3 <- terra::extract(livestock_sheep,
                           x_shp[which(x_shp$GID_3==x_shp$GID_3[[i]]),],
                           na.rm = TRUE, weights = F,fun=mean,method="simple",ID=F) 
  
  
  

  x_shp$cattle_mean[[i]] <- as.numeric(x_Ext)
  x_shp$goat_mean[[i]] <- as.numeric(x_Ext2)
  x_shp$sheep_mean[[i]] <- as.numeric(x_Ext3)


  
  utils::setTxtProgressBar(pb, i)
};rm(i)

close(pb)
###########################################################################
#plot(st_geometry(x_shp[which(x_shp$ID_NORM==x_shp$ID_NORM[[1]]),]))
#getting land use degradation (lud)
lud <- terra::rast(paste0(data_dir,"/","kenya_ldma_2020.tif/kenya_ldma_2020.tif"))
#using lud values above 3 (4,5)
lud2 <- lud
lud2[which(lud2[]<4)] <- NA
lud2[which(lud2[]>3)] <- 1

#getting total area
lud2_a <- terra::expanse(lud2,unit="km",transform=T,byValue=T)
x_shp$lud_45 <- NA
#lud_list <- list()
#for(i in 1:5){
  #lud_list[[i]]
#extracting land use degradation values per county using weighte
for(i in 1:nrow(x_shp)){
  #i <- 1
  #crop  lud layer to each county
  x1 <- terra::crop(lud2,x_shp[which(x_shp$GID_3==x_shp$GID_3[[i]]),],mask=T)
  x1 <- terra::mask(x1,x_shp[which(x_shp$GID_3==x_shp$GID_3[[i]]),])
  #calculating area
  x_Ext <- terra::expanse(x1,unit="km",transform=T,byValue=F)
  x_shp$lud_45[[i]] <- as.numeric(x_Ext$area)
  utils::setTxtProgressBar(pb, i)
};rm(i)

close(pb)
###########################################################################

save.image("D:/CIAT_DEFORESTATION/RESULTS/3_FOREST_PROCAREA_LI_LUD.RData")


