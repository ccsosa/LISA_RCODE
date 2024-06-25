rm(list = ls());gc()
#load libraries
require(readxl);require(dplyr);require(geodata);require(sf);require(terra);require(matrixStats);require(parallel)
#require(rgeoda);require(sf);require(terra)
#getting folder
data_dir  <- "D:/CIAT_DEFORESTATION/DATA/OneDrive_1_17-5-2024"
#load data
load("D:/CIAT_DEFORESTATION/RESULTS/3_FOREST_PROCAREA_LI_LUD.RData")

data <- readxl::read_xlsx(paste0(data_dir,"/","Africa_1997-2023_Feb23.xlsx"),
                          sheet = "Sheet1",col_names = T)

#filter by country
data <- data[which(data$COUNTRY=="Kenya"),]
data_or <- data#filters in association 

#filter by years
#data <- data[data$YEAR %in% c(2015,2016,2017,2018,2019,2020),]
data <- data[data$YEAR %in% c(2011:2020),]

data_or <- data_or[data_or$YEAR %in% c(2011:2023),]

#data2 <- data[data$YEAR %in% c(2011:2023),]
#save a copy of the original data without extra filtering
filters <- c(
  "Borana Ethnic Group (Kenya); Gabra Ethnic Group (Kenya); Pastoralists (Kenya)",
  "Farmers (Kenya)",
  "Farmers (Kenya); Azimio la Umoja One Kenya Coalition Party; DAP-K: Democratic Action Party of Kenya",
  "Farmers (Kenya); Greenpeace",
  "Farmers (Kenya); Labor Group (Kenya)",
  "Farmers (Kenya); Meru Ethnic Group (Kenya)",
  "Farmers (Kenya); Students (Kenya); Teachers (Kenya)",
  "Farmers (Kenya); Taxi Drivers (Kenya)",
  "Fishers (Kenya)",
  "Kenya Kwanza Alliance; Pastoralists (Kenya); Pokot Ethnic Group (Kenya); UDA: United Democratic Alliance",
  "Marakwet Ethnic Group (Kenya); Pastoralists (Kenya)",
  "Pastoralists (Ethiopia)",
  "Pastoralists (Kenya)",
  "Pastoralists (Kenya); Haki Africa",
  "Pastoralists (Kenya); Samburu Ethnic Group (Kenya); Turkana Ethnic Group (Kenya)",
  "Pastoralists (Kenya); Samburu Ethnic Militia (Kenya)",
  "Pastoralists (Kenya); Vigilante Group (Kenya)",
  "Pastoralists (Somalia)",
  "Pastoralists (Uganda)",
  "Pokot Ethnic Group (Kenya); Ilchamus Ethnic Group (Kenya); Pastoralists (Kenya)",
  "Pokot Ethnic Group (Kenya); Pastoralists (Kenya)",
  "Pokot Ethnic Group (Kenya); Pastoralists (Kenya); Students (Kenya)",
  "Pokot Ethnic Militia (Kenya); Pastoralists (Kenya)",
  "Samburu Ethnic Group (Kenya); Pastoralists (Kenya)",
  "Vigilante Group (Kenya); Pastoralists (Kenya)"
)

#filters by association actor
data <- data[data$ASSOC_ACTOR_1 %in% filters,]
data_or <- data_or[data_or$ASSOC_ACTOR_1 %in% filters,]

unique(data$EVENT_TYPE)
#doing data to count fatalities
#data2 <- data
#loading matrix as point shapefile

data2 <-  sf::st_as_sf(data,coords = c('LONGITUDE', 'LATITUDE'))
data2 <-  sf::st_set_crs(data2,4326)

data2_or <- sf::st_as_sf(data_or,coords = c('LONGITUDE', 'LATITUDE'))
data2_or <-  sf::st_set_crs(data2_or,4326)

  
x_shp$n_fat <- NA
x_shp$n_evts <- NA
#x_shp$n_fat_t <- NA
#x_shp$n_evts_t <- NA
#11-24
x_shp$n_fat1124 <- NA
x_shp$n_evts1124 <- NA
#x_shp$n_fat_t1124 <- NA
#x_shp$n_evts_t1124 <- NA
x_shp$n_fat1416 <- NA
x_shp$n_evts1416 <- NA
x_shp$n_fat1921 <- NA
x_shp$n_evts1921 <- NA
################################################################################
#defining years for movible averages
years1 <- 2014:2016
years2 <- 2019:2021
################################################################################
#filling out events and fatalities
pb <-
  utils::txtProgressBar(min = 0,
                        max = nrow(x_shp),
                        style = 3)
for(i in 1:nrow(x_shp)){
  
  #counting fatalities and events using sum of fatalities and number of columns 
#  i <- 1
  x1 <- sf::st_intersection(data2,x_shp[which(x_shp$GID_3==x_shp$GID_3[[i]]),])
  if(nrow(x1)>0){
    x_shp$n_fat[[i]] <- sum(x1$FATALITIES)
    x_shp$n_evts[[i]] <- nrow(x1)
    #x_shp$n_fat1124[[i]] <- sum(x2$FATALITIES)
    #x_shp$n_evts_tt1124[[i]] <- nrow(x2)
    
  } else {
    x_shp$n_fat[[i]] <- 0
    x_shp$n_evts[[i]] <- 0
    #x_shp$n_fat1124[[i]] <- 0
    #x_shp$n_evts_tt1124[[i]] <-0
  }


  utils::setTxtProgressBar(pb, i)
};rm(i)
close(pb)
################################################################################
###############################adding 0 when no data

pb <-
  utils::txtProgressBar(min = 0,
                        max = nrow(x_shp),
                        style = 3)
for(i in 1:nrow(x_shp)){
  #i <- 1
  x2 <- sf::st_intersection(data2_or,x_shp[which(x_shp$GID_3==x_shp$GID_3[[i]]),])
  #x2_years1 <- x2
  
  if(nrow(x2)>0){
    #x_shp$n_fat[[i]] <- sum(x1$FATALITIES)
    #x_shp$n_evts[[i]] <- nrow(x1)
    x_shp$n_fat1124[[i]] <- sum(x2$FATALITIES)
    x_shp$n_evts1124[[i]] <- nrow(x2)
    # x_shp$n_fat1416[[i]] <- sum(x2_years1$FATALITIES)
    # x_shp$n_evts1416[[i]] <- nrow(x2_years1)
    # x_shp$n_fat1921[[i]] <- sum(x2_years2$FATALITIES)
    # x_shp$n_evts1921[[i]] <- nrow(x2_years2)
    
  } else {
    #x_shp$n_fat[[i]] <- 0
    #x_shp$n_evts[[i]] <- 0
    x_shp$n_fat1124[[i]] <- 0
    x_shp$n_evts1124[[i]] <-0
    #x_shp$n_fat1416[[i]] <- 0
    #x_shp$n_evts1416[[i]] <- 0
    #x_shp$n_fat1921[[i]] <- 0
    #x_shp$n_evts1921[[i]] <- 0
  }
  ##############################################################################
  x2_years1 <-x2[which(x2$YEAR %in% years1),]
  #x2_years2 <- x2[which(x2$YEAR %in% years2),]
  if(nrow(x2_years1)>0){
    #x_shp$n_fat[[i]] <- sum(x1$FATALITIES)
    #x_shp$n_evts[[i]] <- nrow(x1)
    #x_shp$n_fat1124[[i]] <- sum(x2$FATALITIES)
    #x_shp$n_evts1124[[i]] <- nrow(x2)
     x_shp$n_fat1416[[i]] <- sum(x2_years1$FATALITIES)
     x_shp$n_evts1416[[i]] <- nrow(x2_years1)
    # x_shp$n_fat1921[[i]] <- sum(x2_years2$FATALITIES)
    # x_shp$n_evts1921[[i]] <- nrow(x2_years2)
    
  } else {
    #x_shp$n_fat[[i]] <- 0
    #x_shp$n_evts[[i]] <- 0
    #x_shp$n_fat1124[[i]] <- 0
    #x_shp$n_evts1124[[i]] <-0
    x_shp$n_fat1416[[i]] <- 0
    x_shp$n_evts1416[[i]] <- 0
    #x_shp$n_fat1921[[i]] <- 0
    #x_shp$n_evts1921[[i]] <- 0
  }
  ##############################################################################
  x2_years2 <- x2[which(x2$YEAR %in% years2),]
  if(nrow(x2_years2)>0){
    #x_shp$n_fat[[i]] <- sum(x1$FATALITIES)
    #x_shp$n_evts[[i]] <- nrow(x1)
    #x_shp$n_fat1124[[i]] <- sum(x2$FATALITIES)
    #x_shp$n_evts1124[[i]] <- nrow(x2)
    #x_shp$n_fat1416[[i]] <- sum(x2_years1$FATALITIES)
    #x_shp$n_evts1416[[i]] <- nrow(x2_years1)
     x_shp$n_fat1921[[i]] <- sum(x2_years2$FATALITIES)
     x_shp$n_evts1921[[i]] <- nrow(x2_years2)
    
  } else {
    #x_shp$n_fat[[i]] <- 0
    #x_shp$n_evts[[i]] <- 0
    #x_shp$n_fat1124[[i]] <- 0
    #x_shp$n_evts1124[[i]] <-0
    #x_shp$n_fat1416[[i]] <- 0
    #x_shp$n_evts1416[[i]] <- 0
    x_shp$n_fat1921[[i]] <- 0
    x_shp$n_evts1921[[i]] <- 0
  }
  
utils::setTxtProgressBar(pb, i)
};rm(i)
close(pb)
################################################################################
# #loading population data
# median_pop <- terra::rast(paste0(data_dir_COVER,"/GHSL/","median_pop_2010_2020.tif"))
# x_shp$median_pop_2010_2020 <- NA
# #i <- 1
# 
# pb <-
#   utils::txtProgressBar(min = 0,
#                         max = nrow(x_shp),
#                         style = 3)
# 
# for(i in 1:nrow(x_shp)){
#   #i <- 1
#   #crop  lud layer to each county
#   x1 <- terra::crop(median_pop,x_shp[which(x_shp$GID_3==x_shp$GID_3[[i]]),],mask=T)
#   x1 <- terra::mask(x1,x_shp[which(x_shp$GID_3==x_shp$GID_3[[i]]),])
#   
#   #sum data to get median of human population per county for 2010-2020
#   x_Ext <- terra::extract(x1,
#                           x_shp[which(x_shp$GID_3==x_shp$GID_3[[i]]),],
#                           na.rm = TRUE, weights = F,fun=sum,method="simple",ID=F)
#   #calculating area
#   x_shp$median_pop_2010_2020[[i]] <- as.numeric(x_Ext[[1]])
#   utils::setTxtProgressBar(pb, i)
# };rm(i)

#close(pb)
################################################################################
#weighting conflict/population
#fatalities rates per 1000 habitants
  #https://www.inei.gob.pe/media/MenuRecursivo/metodologias/mortalidad01.pdf
#x_shp$n_fat_pop <- (x_shp$n_fatalities/x_shp$median_pop_2010_2020)*1000
#x_shp$n_fat_pop_total <- (x_shp$n_fatalities_total/x_shp$median_pop_2010_2020)*1000
################################################################################
#################using worldpop 2020
#loading population data
median_pop_worldpop <- terra::rast("D:/CIAT_DEFORESTATION/DATA/NEW/worldpop/ken_ppp_2020_UNadj_constrained.tif")
x_shp$pop2020_w <- NA

pb <-
  utils::txtProgressBar(min = 0,
                        max = nrow(x_shp),
                        style = 3)

for(i in 1:nrow(x_shp)){
  #i <- 1
  #crop  lud layer to each county
  x1 <- terra::crop(median_pop_worldpop,x_shp[which(x_shp$GID_3==x_shp$GID_3[[i]]),],mask=T)
  x1 <- terra::mask(x1,x_shp[which(x_shp$GID_3==x_shp$GID_3[[i]]),])
  
  #sum data to get median of human population per county for 2010-2020
  x_Ext <- terra::extract(x1,
                          x_shp[which(x_shp$GID_3==x_shp$GID_3[[i]]),],
                          na.rm = TRUE, weights = F,fun=sum,method="simple",ID=F)
  #calculating area
  x_shp$pop2020_w[[i]] <- as.numeric(x_Ext[[1]])
  utils::setTxtProgressBar(pb, i)
};rm(i)

close(pb)


#weighting conflict/population
#fatalities rates per 1000 habitants
#https://www.inei.gob.pe/media/MenuRecursivo/metodologias/mortalidad01.pdf
x_shp$n_fat_p <- (x_shp$n_fat/x_shp$pop2020_w)*1000
#x_shp$n_fat_pop_t_w <- (x_shp$/x_shp$pop2020_worldpop)*1000
x_shp$n_fat_p1123 <- (x_shp$n_fat1124/x_shp$pop2020_w)*1000
################################################################################
##creating deforestation per entities per year
YEARS_EACH <- 2011:2023
x_data_series <- as.data.frame(matrix(ncol=14,nrow=nrow(x_shp)))

pb <-
  utils::txtProgressBar(min = 0,
                        max =length(YEARS_EACH),
                        style = 3)


for(i in 1:length(YEARS_EACH)){
#  i <- 1
  x_i <- data2_or[which(data2_or$YEAR==YEARS_EACH[[i]]),]
  
  for(j in 1:nrow(x_shp)){
   #i <- 1
    x_j<- sf::st_intersection(x_i,x_shp[which(x_shp$GID_3==x_shp$GID_3[[j]]),])
    if(length(x_j)>0){
      x_data_series[j,i+1] <- nrow(x_j)
    } else {
      x_data_series[j,i+1] <- 0
    }
    #rm(x_j)
  };rm(j)
  #rm(x_i)
  utils::setTxtProgressBar(pb, i)
};rm(i)

close(pb)

x_time <- x_shp["GID_3"]
x_time <- cbind(x_time,x_data_series)
x_time$V1 <- NULL
colnames(x_time) <- c("GID_3",YEARS_EACH,"geometry")
sf::write_sf(x_time,"D:/CIAT_DEFORESTATION/RESULTS/n_evts_time.shp")
################################################################################
x_shp$n_evtsm1416 <- x_shp$n_evts1416/3
x_shp$n_evtsm1921 <- x_shp$n_evts1921/3
################################################################################
#saving shapefile 
x_shp2 <- x_shp
colnames(x_shp2) <- abbreviate(colnames(x_shp),minlength = 8)
sf::write_sf(x_shp2,"D:/CIAT_DEFORESTATION/RESULTS/KEN_20240605.shp")
write.csv(data.frame(VARNAME=colnames(x_shp),
                     abbrev=colnames(x_shp2)),
          "D:/CIAT_DEFORESTATION/RESULTS/x_shp_abbr_metadata.csv")

save.image("D:/CIAT_DEFORESTATION/RESULTS/4_FOREST_PROCAREA_LI_LUD_CONFLICT.RData")

