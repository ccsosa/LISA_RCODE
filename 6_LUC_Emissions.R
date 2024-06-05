rm(list = ls());gc()
#load libraries
require(readxl);require(dplyr);require(geodata);require(sf);require(matrixStats);require(parallel);require(ncdf4)
#require(rgeoda);require(sf);require(terra)
#getting folder
load("D:/CIAT_DEFORESTATION/RESULTS/5_FOREST_LU.RData")

r <- terra::rast("D:/CIAT_DEFORESTATION/DATA/NEW/LUC_EMISSIONS/2011_2020_mean_net_setnull.tif")

r <- terra::crop(r,x_shp,mask=T)
#plot(r)
# rot <- function(x) "[<-"(x, , rev(x))
# ##getting folder with the ESA files
# ncin <- nc_open("D:/CIAT_DEFORESTATION/DATA/NEW/LUC_EMISSIONS/Landuse_carbon_flux_BLUE_gridded_GCB2022_2012-2021_mean.nc")
# print(ncin)
# tmp_array <- ncvar_get(ncin,"Landuse_Cflux")
# lon <- ncvar_get(ncin, "lon")
# lat <- ncvar_get(ncin, "lat")
# 
# nc_close(ncin) 
# x_1 <-t(tmp_array)
# x_1 <- x_1[ nrow(x_1):1, ]
# #t(tmp_array)
# r <- terra::rast(x_1
#           #xmin=min(lon), 
#           #xmax=max(lon),
#           #ymin=min(lat), 
#           #ymax=max(lat),
#           #nrow=1440,
#           #ncol=720,
#           #crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0")
#           )
# 
# xmin(r) <-min(lon)
# xmax(r) <-max(lon)
# ymin(r) <-min(lat)
# ymax(r) <-max(lat)
# 
# 
x_shp$LUC_emissions <- NA
pb <-
  utils::txtProgressBar(min = 0,
                        max = nrow(x_shp),
                        style = 3)
for(i in 1:nrow(x_shp)){
  #calculating raster value  by county
  x_Ext <- terra::extract(r,
                          x_shp[which(x_shp$GID_3==x_shp$GID_3[[i]]),],
                          na.rm = TRUE, weights = F,fun=mean,method="simple",ID=F)
  #x_Ext <- terra::expanse(x1,unit="km",transform=T,byValue=F)
  x_shp$LUC_emissions[[i]] <- as.numeric(x_Ext)
  utils::setTxtProgressBar(pb, i)
};rm(i)

close(pb)

x_shp2 <- x_shp
colnames(x_shp2) <- abbreviate(colnames(x_shp),minlength = 8)
sf::write_sf(x_shp2,"D:/CIAT_DEFORESTATION/RESULTS/KEN_20240604.shp")
write.csv(data.frame(VARNAME=colnames(x_shp),
                     abbrev=colnames(x_shp2)),
          "D:/CIAT_DEFORESTATION/RESULTS/x_shp_abbr_metadata.csv")

save.image("D:/CIAT_DEFORESTATION/RESULTS/6_FOREST_LU_LUIEMISS.RData")
