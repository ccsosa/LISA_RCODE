rm(list = ls());gc()
#load libraries
require(readxl);require(dplyr);require(geodata);require(sf);require(matrixStats);require(parallel)
#require(rgeoda);require(sf);require(terra)
#getting folder
load("D:/CIAT_DEFORESTATION/RESULTS/4_FOREST_PROCAREA_LI_LUD_CONFLICT.RData")
##getting folder with the ESA files
data_dir_COVER <- "D:/CIAT_DEFORESTATION/DATA/NEW/WORLDCOVER/ESA_WORLDCOVER_10M_2021_V200/MAP"
################################################################################
#raster values labels

labels <- data.frame(ID = c(10,20,30,40,50,60,70,80,90,95,100),
                       #seq(0,220,10), #old data
                     NAME = c(
  "Tree cover",
  "Shrubland",
  "Grassland",
  "Cropland",
  "Built-up",
  "Bare/sparse vegetation",
  "Snow and Ice",
  "Permanent water bodies",
  "Herbaceous wetland",
  "Mangroves",
  "Moss and lichen"
##old data                       
#   "No Data",
#   "Cropland, rainfed",
#   "Cropland, irrigated or post-flooding",
#   "Mosaic cropland (>50%) / natural vegetation (tree, shrub, herbaceous cover)
# (<50%)",
#   "Mosaic natural vegetation (tree, shrub, herbaceous cover) (>50%) / cropland
# (<50%)",
#   "Tree cover, broadleaved, evergreen, closed to open (>15%)",
#   "Tree cover, broadleaved, deciduous, closed to open (>15%)",
#   "Tree cover, needleleaved, evergreen, closed to open (>15%)",
#   "Tree cover, needleleaved, deciduous, closed to open (>15%)",
#   "Tree cover, mixed leaf type (broadleaved and needleleaved)",
#   "Mosaic tree and shrub (>50%) / herbaceous cover (<50%)",
#   "Mosaic herbaceous cover (>50%) / tree and shrub (<50%)",
#   "Shrubland",
#   "Grassland",
#   "Lichens and mosses",
#   "Sparse vegetation (tree, shrub, herbaceous cover) (<15%)",
#   "Tree cover, flooded, fresh or brakish water",
#   "Tree cover, flooded, saline water",
#   "Shrub or herbaceous cover, flooded, fresh/saline/brakish water",
#   "Urban areas",
#   "Bare areas",
#   "Water bodies",
#   "Permanent snow and ice"
  )
)

################################################################################
#listing raster files

if(!file.exists(paste0(data_dir_COVER,"/KENYA_LU_VALUES_ILRIC.csv"))){
  print("Obtaining LU values area")
files_to <- list.files(data_dir_COVER,recursive = T,pattern = ".tif")
#

#list to save results
raster_list <- list()

pb <-
  utils::txtProgressBar(min = 0,
                        max = 12,
                        style = 3)


#loading an fitting data to Kenya shapefile
for(i in 1:12){
  x1 <- terra::rast(paste0(data_dir_COVER,"/",files_to[[i]]))
  x1 <- terra::crop(x1,x_shp,mask=T)  
  raster_list[[i]] <- x1
  utils::setTxtProgressBar(pb, i)
};rm(i)
close(pb)

#joining files in one
s <- terra::sprc(raster_list)
m <- terra::merge(s)
#terra::writeRaster(m,"D:/CIAT_DEFORESTATION/DATA/NEW/WORLDCOVER/ESA_WORLDCOVER_10M_2021_V200/LANDUSE_2021.tif")


###list to save results
raster_LU_List <- list()
pb <-
  utils::txtProgressBar(min = 0,
                        max = nrow(x_shp),
                        style = 3)
for(i in 1:nrow(x_shp)){
  #i <- 1
  #crop  LU layer to each county
  x1 <- terra::crop(m,x_shp[which(x_shp$GID_3==x_shp$GID_3[[i]]),],mask=T)
  x1 <- terra::mask(x1,x_shp[which(x_shp$GID_3==x_shp$GID_3[[i]]),])
  #calculating area by raster value
  x_Ext <- terra::expanse(x1,unit="km",transform=T,byValue=T)
  x_Ext$GID_3 <- x_shp$GID_3[[i]]
  utils::setTxtProgressBar(pb, i)
  raster_LU_List[[i]] <- x_Ext
};rm(i)

close(pb)


raster_LU_List2 <- do.call(rbind,raster_LU_List)

write.csv(raster_LU_List2,paste0(data_dir_COVER,"/KENYA_LU_VALUES_ILRIC.csv"),row.names = F)
} else {
  print("loading LU area data  previously obtained")
  raster_LU_List2 <- read.csv(paste0(data_dir_COVER,"/KENYA_LU_VALUES_ILRIC.csv"),header = T)
}
################################################################################
#Adding data 
#unique(raster_LU_List2$value)
pb <-
  utils::txtProgressBar(min = 0,
                        max = nrow(labels),
                        style = 3)
for(i in 1:nrow(labels)){
  #i <- 1
  x_shp[,labels$NAME[[i]]] <- NA
  for(j in 1:nrow(x_shp)){
    #j <- 1
    x_i <- raster_LU_List2[(which(raster_LU_List2$GID_3==x_shp$GID_3[[j]] &
                                    raster_LU_List2$value==labels$ID[[i]])),]$area
    if(length(x_i)>0){
      x_shp[j,labels$NAME[[i]]][[1]] <- x_i
    } else {
      x_shp[j,labels$NAME[[i]]][[1]] <- 0
    }
    
    
  };rm(j)
  utils::setTxtProgressBar(pb, i)
};rm(i)
close(pb)
#plot(st_geometry(x_shp),add=T)
# x1 <- terra::mask(x1,x_shp)
# plot(x1)


x_shp2 <- x_shp
colnames(x_shp2) <- abbreviate(colnames(x_shp),minlength = 8)
sf::write_sf(x_shp2,"D:/CIAT_DEFORESTATION/RESULTS/KEN_20240604.shp")
write.csv(data.frame(VARNAME=colnames(x_shp),
                     abbrev=colnames(x_shp2)),
          "D:/CIAT_DEFORESTATION/RESULTS/x_shp_abbr_metadata.csv")
save.image("D:/CIAT_DEFORESTATION/RESULTS/5_FOREST_LU.RData")
 