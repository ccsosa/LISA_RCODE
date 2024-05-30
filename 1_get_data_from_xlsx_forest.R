rm(list = ls());gc()
#load libraries
require(readxl);require(dplyr);require(geodata);require(sf);require(matrixStats)
#require(rgeoda);require(sf);require(terra)
#getting folder
data_dir  <- "D:/CIAT_DEFORESTATION/DATA/OneDrive_1_17-5-2024"
#geting shapefile
#loading shapefile from GADM3
x_shp <- geodata::gadm(country = "KEN",level = 2,
                       path = "D:/CIAT_DEFORESTATION/DATA/NEW/GADM")
x_shp <- sf::st_as_sf(x_shp)
area <- (as.numeric(st_area(x_shp)))*(1e-6/1)
x_shp$area <- area
#adding adm1 and adm2 in one to join information later
x_shp$ID_NORM <- paste0(x_shp$NAME_1,"_",x_shp$NAME_2)
#save shp
sf::write_sf(x_shp,"D:/CIAT_DEFORESTATION/DATA/NEW/GADM/KEN_ADM2.shp")
################################################################################
#loading tree cover loss
data <- readxl::read_xlsx(paste0(data_dir,"/","KEN.xlsx"),
                          sheet = "Subnational 2 tree cover loss",col_names = T)
data <- data[which(data$threshold==30),]
data$ID_NORM <- paste0(data$subnational1,"_",data$subnational2)

#count <- ne_countries(returnclass = "sf",scale = "large")
#base <- sf::st_read(paste0(data_dir,"/","ken_adm_iebc_20191031_shp/ken_admbnda_adm2_iebc_20191031.shp"))

#data_filtered <- as.data.frame(data[,c(2,3,6,7,8,31)])#2018
data_filtered <- as.data.frame(data[,c(2,3,6,7,8,31)])#2015
#not matching
anti_join(y = data.frame(NAME=x_shp$ID_NORM),
          x = data.frame(NAME=data$ID_NORM),
)
#2015-2020
#data_filtered$tc_loss_med_18_20 <- rowMedians(cbind(data$tc_loss_ha_2018,data$tc_loss_ha_2019,data$tc_loss_ha_2020))
# data_filtered$tc_loss_med_15_20 <- matrixStats::rowMedians(cbind(
#                                                     data$tc_loss_ha_2015,
#                                                     data$tc_loss_ha_2016,
#                                                     data$tc_loss_ha_2017,
#                                                     data$tc_loss_ha_2018,
#                                                     data$tc_loss_ha_2019,
#                                                     data$tc_loss_ha_2020))

data_filtered$tc_loss_med_11_20 <- matrixStats::rowMedians(cbind(
                                                    data$tc_loss_ha_2011,
                                                    data$tc_loss_ha_2012,
                                                    data$tc_loss_ha_2013,
                                                    data$tc_loss_ha_2014,
                                                    data$tc_loss_ha_2015,
                                                    data$tc_loss_ha_2016,
                                                    data$tc_loss_ha_2017,
                                                    data$tc_loss_ha_2018,
                                                    data$tc_loss_ha_2019,
                                                    data$tc_loss_ha_2020))
################################################################################
#loading Subnational 2 carbon data
data2 <- readxl::read_xlsx(paste0(data_dir,"/","KEN.xlsx"),
                           sheet = "Subnational 2 carbon data",col_names = T)
data2 <- data2[which(data2$umd_tree_cover_density_2000__threshold==30),]
data2$ID_NORM <- paste0(data2$subnational1,"_",data2$subnational2)
#count <- ne_countries(returnclass = "sf",scale = "large")
#base <- sf::st_read(paste0(data_dir,"/","ken_adm_iebc_20191031_shp/ken_admbnda_adm2_iebc_20191031.shp"))
#filtering
#data_filtered2 <- as.data.frame(data2[,c(2,3,5,6,7,33)])#2018
data_filtered2 <- as.data.frame(data2[,c(2,3,5,6,7,33)])#2015
#not matching
anti_join(y = data.frame(NAME=x_shp$ID_NORM),
          x = data.frame(NAME=data2$ID_NORM),
)
#data_filtered2$carbon_gross_18_20 <- matrixStats::rowMedians(cbind(data2$gfw_forest_carbon_gross_emissions_2018__Mg_CO2e,
#data2$gfw_forest_carbon_gross_emissions_2018__Mg_CO2e,data2$gfw_forest_carbon_gross_emissions_2018__Mg_CO2e))
#2015-2020
# data_filtered2$carbon_gross_15_20 <- matrixStats::rowMedians(cbind(
#   data2$gfw_forest_carbon_gross_emissions_2015__Mg_CO2e,
#   data2$gfw_forest_carbon_gross_emissions_2016__Mg_CO2e,
#   data2$gfw_forest_carbon_gross_emissions_2017__Mg_CO2e,
#   data2$gfw_forest_carbon_gross_emissions_2018__Mg_CO2e,
#   data2$gfw_forest_carbon_gross_emissions_2019__Mg_CO2e,
#   data2$gfw_forest_carbon_gross_emissions_2020__Mg_CO2e))
#2011-2020
data_filtered2$carbon_gross_11_20 <- matrixStats::rowMedians(cbind(
  data2$gfw_forest_carbon_gross_emissions_2011__Mg_CO2e,
  data2$gfw_forest_carbon_gross_emissions_2012__Mg_CO2e,
  data2$gfw_forest_carbon_gross_emissions_2013__Mg_CO2e,
  data2$gfw_forest_carbon_gross_emissions_2014__Mg_CO2e,
  data2$gfw_forest_carbon_gross_emissions_2015__Mg_CO2e,
  data2$gfw_forest_carbon_gross_emissions_2016__Mg_CO2e,
  data2$gfw_forest_carbon_gross_emissions_2017__Mg_CO2e,
  data2$gfw_forest_carbon_gross_emissions_2018__Mg_CO2e,
  data2$gfw_forest_carbon_gross_emissions_2019__Mg_CO2e,
  data2$gfw_forest_carbon_gross_emissions_2020__Mg_CO2e))


################################################################################
#put into a file
vars_i <- c("extent_2000_ha", #data_filtered
            "extent_2010_ha", #data_filtered
            "gain_2000-2020_ha", #data_filtered
            "tc_loss_med_11_20", #data_filtered
            "umd_tree_cover_extent_2000__ha", #data_filtered2,
            #"gfw_aboveground_carbon_stocks_2000__Mg_C",
            "carbon_gross_11_20" #data_filtered2
            )
#Adding information by cell and variable
for(i in 1:length(vars_i)){
  #i <- 1
  #new variable in NA
  x_shp[,vars_i[[i]]] <- NA
  #print(paste("i -",i))
  
  for(j in 1:nrow(x_shp)){
    #print(paste("i -",i,"/ j -",j))
    #i <5 means take from the first sheet and > 5 from the second sheet
    if(i <5){
      x_shp[j,vars_i[i]] <- 
        data_filtered[,vars_i[i]][which(data_filtered$ID_NORM==x_shp$ID_NORM[[j]])] 
    } else {
      x_shp[j,vars_i[i]] <- 
        data_filtered2[,vars_i[i]][which(data_filtered2$ID_NORM==x_shp$ID_NORM[[j]])]     
    }
  };rm(j)
};rm(i)
################################################################################
#cover loss median by area to get proportion 
x_shp$tc_loss_med_prop <- x_shp$tc_loss_med_11_20/(x_shp$area)*(1/100)
save.image("D:/CIAT_DEFORESTATION/RESULTS/1_FOREST.RData")
#write_sf(x_shp,"D:/CIAT_DEFORESTATION/DATA/NEW/GADM/KEN_ADM2_AC.shp")
