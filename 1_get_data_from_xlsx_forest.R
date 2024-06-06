rm(list = ls());gc()
#load libraries
require(readxl);require(dplyr);require(geodata);require(sf);require(matrixStats)
#require(rgeoda);require(sf);require(terra)


#getting folder
data_dir  <- "D:/CIAT_DEFORESTATION/DATA/OneDrive_1_17-5-2024"
data_dir_COVER <- "D:/CIAT_DEFORESTATION/DATA/NEW"
################################################################################
#creating folder to save results
if(!dir.exists("D:/CIAT_DEFORESTATION/RESULTS")){
  dir.create("D:/CIAT_DEFORESTATION/RESULTS")
}

if(!dir.exists("D:/CIAT_DEFORESTATION/RESULTS/LISA")){
  dir.create("D:/CIAT_DEFORESTATION/RESULTS/LISA")
}
if(!dir.exists("D:/CIAT_DEFORESTATION/RESULTS/MAPS")){
  dir.create("D:/CIAT_DEFORESTATION/RESULTS/MAPS")
}
#geting shapefile
#loading shapefile from GADM3
# x_shp <- geodata::gadm(country = "KEN",level = 3,
#                        path = "D:/CIAT_DEFORESTATION/DATA/NEW/GADM")
x_shp <- sf::st_read("D:/CIAT_DEFORESTATION/DATA/NEW/KEN_ILRI/gadm41_KEN_3.shp")
x_shp <- sf::st_as_sf(x_shp)
#sf::write_sf(x_shp,"D:/CIAT_DEFORESTATION/DATA/NEW/GADM/KEN_ADM3.shp")

base <- sf::st_read("D:/CIAT_DEFORESTATION/DATA/NEW/KEN_ILRI/gadm41_KEN_0.shp")
#to use area
sf_use_s2(T)
#calculating counties area
#plot(st_geometry(base))
area <- (as.numeric(st_area(x_shp)))*(1e-6/1)
x_shp$area <- area
#adding adm1 and adm2 in one to join information later
#x_shp$ID_NORM <- paste0(x_shp$,"_",x_shp$NAME_2)
#save shp
sf::write_sf(x_shp,"D:/CIAT_DEFORESTATION/DATA/NEW/ILRI/KEN_ADM3.shp")
################################################################################
#loading tree cover loss

################################################################################
#FOREST LOSS
#lossyear_40N_080W.tif
#base <- sf::st_read(paste0(data_dir,"/","ken_adm_iebc_20191031_shp/ken_admbnda_adm2_iebc_20191031.shp"))
#Calculating deforestation year loss
if(!file.exists("D:/CIAT_DEFORESTATION/DATA/NEW/rasters/ILRI/lossyear.tif")){
  x1 <- terra::rast("
https://storage.googleapis.com/earthenginepartners-hansen/GFC-2023-v1.11/Hansen_GFC-2023-v1.11_lossyear_10N_030E.tif")
  x2 <- terra::rast("
https://storage.googleapis.com/earthenginepartners-hansen/GFC-2023-v1.11/Hansen_GFC-2023-v1.11_lossyear_10N_040E.tif")
  x3 <- terra::rast("
https://storage.googleapis.com/earthenginepartners-hansen/GFC-2023-v1.11/Hansen_GFC-2023-v1.11_lossyear_00N_030E.tif")
  x4 <- terra::rast("
https://storage.googleapis.com/earthenginepartners-hansen/GFC-2023-v1.11/Hansen_GFC-2023-v1.11_lossyear_00N_040E.tif")
#joining, merging and masking
    s <- terra::sprc(x1, x2,x3,x4)
  m <- terra::merge(s)
  m2 <- terra::crop(m,base)
  m3a <- terra::mask(m2,base)
  
  terra::writeRaster(m3a,"D:/CIAT_DEFORESTATION/DATA/NEW/rasters/ILRI/lossyear.tif")  
} else {
  m3a <- terra::rast("D:/CIAT_DEFORESTATION/DATA/NEW/rasters/ILRI/lossyear.tif")
}

m4 <- m3a
# m4[which(m4[]<10)] <- NA
# m4[which(m4[]>21)] <- NA
################################################################################
#obtaining area affected per year
raster_LU_List <- list()
pb <-
  utils::txtProgressBar(min = 0,
                        max = nrow(x_shp),
                        style = 3)

for(i in 1:nrow(x_shp)){
  #i <- 1
  x1 <- terra::crop(m4,x_shp[which(x_shp$GID_3==x_shp$GID_3[[i]]),],mask=T)
  x1 <- terra::mask(x1,x_shp[which(x_shp$GID_3==x_shp$GID_3[[i]]),])
  #calculating area by raster value
  x_Ext <- terra::expanse(x1,unit="km",transform=T,byValue=T)
  x_Ext$GID_3 <- x_shp$GID_3[[i]]
  utils::setTxtProgressBar(pb, i)
  raster_LU_List[[i]] <- x_Ext
};rm(i)
close(pb)


raster_LU_List2 <- do.call(rbind,raster_LU_List)

#saving raw data
write.csv(raster_LU_List2,paste0(data_dir_COVER,"/KENYA_HANSSN_VALUES_ILRIC.csv"),row.names = F)
#removing years 2010,2021,2022,2023
raster_LU_List3 <- raster_LU_List2[which(raster_LU_List2$value>10),]
raster_LU_List3 <- raster_LU_List3[which(raster_LU_List3$value<21),]


years <- unique(raster_LU_List3$value)
df_to_see <- as.data.frame(matrix(ncol=length(years)+1,nrow = nrow(x_shp)))
colnames(df_to_see) <- c("ADM2_PCODE",years)
df_to_see[,1] <- x_shp$ADM2_PCODE


#adding data data to shapefile

for(i in 1:nrow(x_shp)){
  #print(paste0("i: ",i))
  x1 <- raster_LU_List3[which(raster_LU_List3$GID_3==x_shp$GID_3[[i]]),]
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
write.csv(df_to_see,paste0(data_dir_COVER,"/KENYA_HANSSEN_VALUES_WIDE_ILRIC.csv"),row.names = F,na = "")

x_shp$tc_loss_med_11_20 <- df_to_see$median
x_shp$tc_loss_med_prop <- x_shp$tc_loss_med_11_20/(x_shp$area)*(1/100)
################################################################################
###ADDING INFO FROM ABOVE GROUND BIOMASS (Mg/Ha()
A1 <- terra::rast("D:/CIAT_DEFORESTATION/DATA/NEW/rasters/ILRI/AGB_2010.tif")  
A2 <- terra::rast("D:/CIAT_DEFORESTATION/DATA/NEW/rasters/ILRI/AGB_2020.tif")  
#2010-2020
#A_DIF <- ((A2-A1)/A1)*100
A_DIF_1 <- A1-A2


x_shp$ABG_2010_2020 <- NA
pb <-
  utils::txtProgressBar(min = 0,
                        max = nrow(x_shp),
                        style = 3)
for(i in 1:nrow(x_shp)){
  #calculating raster value  by county
  x_Ext <- terra::extract(A_DIF_1,
                          x_shp[which(x_shp$GID_3==x_shp$GID_3[[i]]),],
                          na.rm = TRUE, weights = F,fun=mean,method="simple",ID=F)
  #x_Ext <- terra::expanse(x1,unit="km",transform=T,byValue=F)
  x_shp$ABG_2010_2020[[i]] <- as.numeric(x_Ext)
  utils::setTxtProgressBar(pb, i)
};rm(i)

close(pb)

################################################################################
save.image("D:/CIAT_DEFORESTATION/RESULTS/1_FOREST.RData")
#write_sf(x_shp,"D:/CIAT_DEFORESTATION/DATA/NEW/GADM/KEN_ADM2_AC.shp")
