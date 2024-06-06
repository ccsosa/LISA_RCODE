rm(list = ls());gc()
#load libraries
require(readxl);require(dplyr);require(geodata);
require(sf);require(matrixStats);require(parallel);
require(ggplot2);require(MetBrewer);require(corrplot);
require(factoextra);require(FactoMineR);require(xlsx)
#data_dir <- "D:/CIAT_DEFORESTATION/RESULTS"

x_shp1 <- sf::st_read("D:/CIAT_DEFORESTATION/DATA/NEW/KEN_ILRI/gadm41_KEN_1.shp")


#x_shp1 <- geodata::gadm(country = "KEN",level = 1,
#                        path = "D:/CIAT_DEFORESTATION/DATA/NEW/GADM")
x_shp1 <- sf::st_as_sf(x_shp1)
#load("D:/CIAT_DEFORESTATION/RESULTS/4_FOREST_PROCAREA_LI_LUD_CONFLICT.RData")
#load("D:/CIAT_DEFORESTATION/RESULTS/5_FOREST_LU.RData")
load("D:/CIAT_DEFORESTATION/RESULTS/6_FOREST_LU_LUIEMISS.RData")

data_dir <- "D:/CIAT_DEFORESTATION/RESULTS"

#check colors
#MetBrewer::display_all()

vars <- c(#"gain_2000-2020_ha",
          "tc_loss_med_11_20",
          "tc_loss_med_prop",
          "ABG_2010_2020",
          #"carbon_gross_11_20",
          #"tc_loss_med_prop",
          "protarea_area",            
          "protarea_prop",
          "cattle_mean",
          "goat_mean",
          "sheep_mean",
          "lud_45",
          "n_fatalities",
          "n_events",
          "n_fatalities_total",
          "n_events_total",
          #new
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
          "Moss and lichen",
          "LUC_emissions",
          "median_pop_2010_2020",
          "n_fat_pop",
          "n_fat_pop_total"
          )

captions <- c(#"Forest gain (ha) (2000-2020)",
              "Tree cover loss (km2) (Median: 2011-2020)",
              "Tree cover loss proportion of county (2011-2020)",
              "Average forest above-ground biomass difference (Mg/ha)(2010-2020)",
              #"Forest carbon gross emissions (Mg CO2e) (Median: 2011-2020)",
              #"Tree cover loss proportion per county area (2011-2020)",
              "Protected area (km2)",            
              "Protected area proportion per county area",
              "Cattle (animal numbers for 2015)",
              "Goats (animal numbers for 2015)",
              "Sheep (animal numbers for 2015)",
              "High and very high land use degradation (km2) (2020)",
              "Fatalities involving farmers, fishers or pastoralists (2011-2020)",
              "Conflict events involving farmers, fishers or pastoralists (2011-2020)",
              "Fatalities (2011-2020)",
              "Conflict events (2011-2020)",
              #new
              "Tree cover (km2) (2021)",
              "Shrubland (km2) (2021)",
              "Grassland (km2) (2021)",
              "Cropland (km2) (2021)",
              "Built-up (km2) (2021)",
              "Bare or sparse vegetation (km2) (2021)",
              "Snow and Ice (km2) (2021)",
              "Permanent water bodies (km2) (2021)",
              "Herbaceous wetland (km2) (2021)",
              "Mangroves (km2) (2021)",
              "Moss and lichen (km2) (2021)",
              "Average Land use change emissions (T C yr-1) (2011-2020)",
              "Human population (Median:2010-2020)",
              "Death rate involving farmers, fishers or pastoralists (2011-2020)",
              "Death rate (2011-2020)"
              )

#i <- 1
for(i in 1:length(vars)){
  print(i)
  x_i <- x_shp[vars[[i]]]
  breaks <- as.data.frame(x_i[,vars[[i]]])[,1]
  breaks[which(breaks==0)] <- NA
  x_i[vars] <- breaks
  
  #x_i[vars[[1]]][which()] #NULL
  #breaks1 <- quantile(breaks)
  #total <- scale_fill_gradientn
  #x_shp[,vars[[i]]] <- as.numeric(x_shp[,vars[[i]]])
  ghmc <- ggplot() + 
    geom_sf(data = x_i, aes(fill = breaks), col = 'grey59', lwd = 0.3) + 
    #scale_fill_gradientn(colors = rev(met.brewer('VanGogh3', 5))) +
    #scale_fill_gradientn(colors = rev(met.brewer('VanGogh3', 5))) +
    scale_fill_gradientn(colors = met.brewer('VanGogh3', 5)) +
    geom_sf(data = x_shp1, fill = NA, col = 'white', lwd = 0.75) +
    # geom_sf(data = expn, fill = '#C7C7C7', col = 'white', lwd = 0.3) +
    # geom_sf(data = crgn, fill = 'grey70', col = '#E0E0E0', lwd = 0.3) + 
    # geom_sf(data = mpos, fill = '#FFFFFF', col = 'grey90', lwd = 0.3) +
    # geom_sf(data = rios, fill = '#3F73A3', col = '#3F73A3') + 
    # geom_sf_text(data = crgn, aes(label = corregimie), family = 'serif', size = 2.5, col = 'grey40') +
    # geom_sf_text(data = cmns, aes(label = comuna), family = 'serif', size = 4, face = 'bold', col = 'white') +
    # geom_sf_text(data = expn, aes(label = gid), family = 'serif', size = 2.5, col = 'grey40') +
    # geom_sf_text(data = mpos, aes(label = MPIO_CNMBR), family = 'serif', size = 2.5, col = 'grey40') +
    labs(x = 'Longitude', y = 'Latitude',fill="") + 
    ggtitle(label = captions[[i]]) + 
    #coord_sf(xlim = ext(shp)[1:2], ylim = ext(shpf)[3:4]) +
    theme_minimal() + 
    theme(legend.position = 'right', 
          legend.text = element_text(family = 'serif'), 
          legend.title = element_text(family = 'serif', face = 'bold'),
          plot.title = element_text(size = 16, face = 'bold', hjust = 0.5, family = 'serif'),
          plot.caption = element_text(family = 'serif'),
          axis.text.x = element_text(family = 'serif'), 
          axis.text.y = element_text(hjust = 0.5, angle = 90, family = 'serif'),
          axis.title.x = element_text(family = 'serif'),
          axis.title.y = element_text(family = 'serif'),
          legend.key.height = unit(2.5, 'line')) 
  #ghmc
  ggsave(plot = ghmc, filename = paste0(data_dir,"/maps/",vars[[i]],".tiff"), units = 'in', width = 7, height = 8, dpi = 300)
}
  #annotation_scale(location =  "br", width_hint = 0.5) +
# annotation_north_arrow(location = "br", which_north = "true", 
#                        pad_x = unit(0.1, "in"), pad_y = unit(0.2, "in"), 
#                        style = north_arrow_fancy_orienteering(text_family = 'serif'))
var_df <- as.data.frame(x_shp[,vars])
var_df$geometry <- NULL
row.names(var_df)  <- x_shp$GID_3
# x_PCA <- PCA(var_df,scale.unit = T)
# eig.val <- get_eigenvalue(x_PCA)
# fviz_eig(x_PCA, addlabels = TRUE, ylim = c(0, 50))
# var <- get_pca_var(x_PCA)
# corrplot(var$cos2, is.corr=FALSE)
# corrplot(as.matrix(var_df), is.corr=FALSE)

var_mat <- as.matrix(var_df)
  
var_mat2 <- var_mat[complete.cases(var_mat),]
colnames(var_mat2) <- captions
# pdf(file = "D:/CIAT_DEFORESTATION/RESULTS/CORRELATIONS.pdf",width = 20,height = 18)
# corrplot(cor(var_mat2,method = "spearman"),
#          #method = "number", 
#          method = 'color',
#          cl.pos = 'b', addCoef.col = 'black',
#          type = "lower", 
#          title = "Kenya variables per county correlations (292 counties/300 counties) with complete data", 
#          mar = c(0,0,1,0),number.cex = 1.2, number.digits = 2,col = COL2('BrBG'),diag = F)
# 
#  dev.off()
 ###############################################################################
#png(file = "D:/CIAT_DEFORESTATION/RESULTS/CORRELATIONS.png",width = 1200,height = 1100,units = "px")
png(file = "D:/CIAT_DEFORESTATION/RESULTS/CORRELATIONS.png",width = 1600,height = 1300,units = "px")
 corrplot( cor(var_mat2,method = "spearman"),
   #cor(var_mat2[,-c(24,25)],method = "spearman"),
          #method = "number", 
          method = 'color',
          cl.pos = 'b', addCoef.col = 'black',
          type = "lower", 
          title = "Kenya variables per county correlations (292 counties/300 counties) with complete data", 
          mar = c(0,0,1,0),
          #number.cex = 0.9,
          number.cex = 0.8,
          number.digits = 2,col = COL2('BrBG'),diag = F,tl.cex = 1.2)
 
 dev.off()
 ###############################################################################
 #saving excel file
 sf::st_write(x_shp, "D:/CIAT_DEFORESTATION/RESULTS/DATA_20240605.csv", layer_options = "GEOMETRY=AS_XY",append = F)
 
 ###############################################################################
 x_Cor <- cor(var_mat2,method = "spearman")
 #plot(hclust(as.dist(1-(x_Cor[-c(25),-c(25)])),method = "ward.D2"))
 plot(hclust(as.dist(1-(x_Cor[-c(24),-c(24)])),method = "ward.D2"))
 # 
 
 
# var_mat3 <- var_mat2[,c(7,8,9,11,12,13,14)]
 var_mat3 <- var_mat2[,c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,25,26,27,28)]
 x_Cor2 <- cor(var_mat3,method = "spearman")
 plot(hclust(as.dist(1-(x_Cor2)),method = "ward.D2"))
 # 
 
 png(file = "D:/CIAT_DEFORESTATION/RESULTS/CORRELATIONS_SEL.png",width = 1600,height = 1300,units = "px")
 
 corrplot(cor(var_mat3,method = "spearman"),
          #method = "number", 
          method = 'color',
          cl.pos = 'b', addCoef.col = 'black',
          type = "lower", 
          title = "Kenya variables per county correlations (292 counties/300 counties) with complete data", 
          mar = c(0,0,1,0),
          #number.cex = 0.9,
          number.cex = 0.8,
          number.digits = 2,col = COL2('BrBG'),diag = T,tl.cex = 1.2)
 
 
 dev.off()
 # write.csv(as.data.frame(x_shp),"D:/CIAT_DEFORESTATION/RESULTS/DATA_20240527.csv",quote = T,
 #            row.names = F,na = "")#showNA = F,#col.names = T,
            #sheetName = "variables")
 # 
 # pdf(file = "D:/CIAT_DEFORESTATION/RESULTS/CORRELATIONS_ALL.pdf",width = 20,height = 18)
 # corrplot(cor(var_mat,method = "spearman"),
 #          #method = "number", 
 #          method = 'color',
 #          cl.pos = 'b', addCoef.col = 'black',
 #          type = "lower", 
 #          title = "Kenya variables per county correlations (300 counties)", 
 #          mar = c(0,0,1,0),number.cex = 1.2, number.digits = 2,col = COL2('BrBG'),diag = F)
 # 
 # dev.off()
 # 