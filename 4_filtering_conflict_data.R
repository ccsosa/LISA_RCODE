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
#filter by years
#data <- data[data$YEAR %in% c(2015,2016,2017,2018,2019,2020),]
data <- data[data$YEAR %in% c(2011:2020),]
#save a copy of the original data without extra filtering
data_or <- data
#filters in association 
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

#doing data to count fatalities
data2 <- data
#loading matrix as point shapefile

data2 <-  sf::st_as_sf(data2,coords = c('LONGITUDE', 'LATITUDE'))
data2 <-  sf::st_set_crs(data2,4326)
x_shp$n_fatalities <- NA
x_shp$n_events <- NA
x_shp$n_fatalities_total <- NA
x_shp$n_events_total <- NA

pb <-
  utils::txtProgressBar(min = 0,
                        max = nrow(x_shp),
                        style = 3)
for(i in 1:nrow(x_shp)){
  
  #counting fatalities and events using sum of fatalities and number of columns 
  #i <- 1
  x1 <- sf::st_intersection(data2,x_shp[which(x_shp$GID_3==x_shp$GID_3[[i]]),])  
  if(nrow(x1)>0){
    x_shp$n_fatalities[[i]] <- sum(x1$FATALITIES)
    x_shp$n_events[[i]] <- nrow(x1)
  } else {
    x_shp$n_fatalities[[i]] <- 0
    x_shp$n_events[[i]] <- 0
  }
  utils::setTxtProgressBar(pb, i)
};rm(i)
close(pb)
################################################################################
#extracting total data without filtering

data2_total <-  sf::st_as_sf(data_or,coords = c('LONGITUDE', 'LATITUDE'))
data2_total <-  sf::st_set_crs(data2_total,4326)


pb <-
  utils::txtProgressBar(min = 0,
                        max = nrow(x_shp),
                        style = 3)
for(i in 1:nrow(x_shp)){
  
  #counting fatalities and events using sum of fatalities and number of columns 
  #i <- 1
  x1 <- sf::st_intersection(data2_total,x_shp[which(x_shp$GID_3==x_shp$GID_3[[i]]),])  
  if(nrow(x1)>0){
    x_shp$n_fatalities_total[[i]] <- sum(x1$FATALITIES)
    x_shp$n_events_total[[i]] <- nrow(x1)
  } else {
    x_shp$n_fatalities_total[[i]] <- 0
    x_shp$n_events_total[[i]] <- 0
  }
  utils::setTxtProgressBar(pb, i)
};rm(i)
close(pb)

################################################################################
#loading population data
median_pop <- terra::rast(paste0(data_dir_COVER,"/GHSL/","median_pop_2010_2020.tif"))
x_shp$median_pop_2010_2020 <- NA
#i <- 1

pb <-
  utils::txtProgressBar(min = 0,
                        max = nrow(x_shp),
                        style = 3)

for(i in 1:nrow(x_shp)){
  #i <- 1
  #crop  lud layer to each county
  x1 <- terra::crop(median_pop,x_shp[which(x_shp$GID_3==x_shp$GID_3[[i]]),],mask=T)
  x1 <- terra::mask(x1,x_shp[which(x_shp$GID_3==x_shp$GID_3[[i]]),])
  
  #sum data to get median of human population per county for 2010-2020
  x_Ext <- terra::extract(x1,
                          x_shp[which(x_shp$GID_3==x_shp$GID_3[[i]]),],
                          na.rm = TRUE, weights = F,fun=sum,method="simple",ID=F)
  #calculating area
  x_shp$median_pop_2010_2020[[i]] <- as.numeric(x_Ext[[1]])
  utils::setTxtProgressBar(pb, i)
};rm(i)

close(pb)
################################################################################
#weighting conflict/population
#fatalities rates per 1000 habitants
  #https://www.inei.gob.pe/media/MenuRecursivo/metodologias/mortalidad01.pdf
x_shp$n_fat_pop <- (x_shp$n_fatalities/x_shp$median_pop_2010_2020)*1000
x_shp$n_fat_pop_total <- (x_shp$n_fatalities_total/x_shp$median_pop_2010_2020)*1000
################################################################################
#saving shapefile 
x_shp2 <- x_shp
colnames(x_shp2) <- abbreviate(colnames(x_shp),minlength = 8)
sf::write_sf(x_shp2,"D:/CIAT_DEFORESTATION/RESULTS/KEN_20240605.shp")
write.csv(data.frame(VARNAME=colnames(x_shp),
                     abbrev=colnames(x_shp2)),
          "D:/CIAT_DEFORESTATION/RESULTS/x_shp_abbr_metadata.csv")

save.image("D:/CIAT_DEFORESTATION/RESULTS/4_FOREST_PROCAREA_LI_LUD_CONFLICT.RData")

