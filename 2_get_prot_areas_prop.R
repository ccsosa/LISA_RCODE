rm(list = ls());gc()
#load libraries
require(readxl);require(dplyr);require(geodata);require(sf);require(matrixStats);require(parallel)
#require(rgeoda);require(sf);require(terra)
#getting folder
data_dir  <- "D:/CIAT_DEFORESTATION/DATA/OneDrive_1_17-5-2024"
#load data
load("D:/CIAT_DEFORESTATION/RESULTS/1_FOREST.RData")
proc <- readRDS(paste0(data_dir,"/","protected areas.RDS"))

# area <- (as.numeric(st_area(x_shp)))*(1e-6/1)#st_area(x_shp)
counties <- x_shp$GID_3


pb <-
  utils::txtProgressBar(min = 0,
                        max = nrow(x_shp),
                        style = 3)

#x_shp2 <- st_make_valid(x_shp)
# plot(st_geometry(x_shp[which(x_shp$ID_NORM==x_shp$ID_NORM[[i]]),]))
# plot(st_geometry(proc),add=T,col="blue")
# plot(st_geometry(x1),add=T,col="red")
x_shp$protarea_area <- NA
#Doing intersection of protected area per county (Fastest way)
for(i in 1:nrow(x_shp)){
  #i <- 164
  x1 <- sf::st_intersection(x_shp[which(x_shp$GID_3==x_shp$GID_3[[i]]),],proc)
  #print(paste("i= ",i," j= ",nrow(x1)))
  if(nrow(x1)>0){
    x_shp$protarea_area[[i]] <- (as.numeric(sum(sf::st_area(x1))))*(1e-6/1) 
  } else {
    x_shp$protarea_area[[i]] <- 0
  }
  utils::setTxtProgressBar(pb, i)
};rm(i)
close(pb)
#Calculating proportion of protected area per county
x_shp$protarea_prop <- x_shp$protarea_area/x_shp$area
save.image("D:/CIAT_DEFORESTATION/RESULTS/2_FOREST_PROCAREA.RData")
