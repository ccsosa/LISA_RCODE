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
################################################################################
#check colors
#MetBrewer::display_all()
x_shp$LUC_emissions <- x_shp$LUC_emissions/1000
x_shp$loss_m_p1120 <- x_shp$loss_m_p1120*100
x_shp$livestock <- rowSums(cbind(x_shp$cattle_mean,x_shp$sheep_mean,x_shp$goat_mean),na.rm = T)

################################################################################
vars <- c(#"gain_2000-2020_ha",
  "loss_m1120",
  "loss_m1123",
  "loss_m_p1120",
  "ABG_10_20",
  "ABG_2020",
  #"carbon_gross_11_20",
  #"tc_loss_med_prop",
  "prot_area",            
  "prot_prop",
  "cattle_mean",
  "goat_mean",
  "sheep_mean",
  "lud_45",
  "n_fat",
  "n_evts",
  #"n_fat_t",
  #"n_evts_t",
  "n_evts1124",
  "n_fat1124",
  #new
  "Tree cover",
  "Shrubland",
  "Grassland",
  "Cropland",
  "Built-up",
  #"Bare/sparse vegetation",
  "Snow and Ice",
  "Permanent water bodies",
  "Herbaceous wetland",
#  "Mangroves",
#  "Moss and lichen",
  "LUC_emissions",
  #"median_pop_2010_2020",
  #"n_fat_pop",
  #"n_fat_pop_total",
  "pop2020_w",
  "n_fat_p",
  #"n_fat_pop_total_world",
  "n_fat_p1123",
  "livestock"
)

captions <- c(#"Forest gain (ha) (2000-2020)",
  "Tree cover loss (km2) (Median: 2011-2020)",
  "Tree cover loss (km2) (Median: 2011-2023)",
  "Tree cover loss proportion (%) (Median: 2011-2020)",
  "Average forest above-ground biomass (Mg/ha)(2010-2020)",
  "Average forest above-ground biomass(Mg/ha)(2020)",
  #"Forest carbon gross emissions (Mg CO2e) (Median: 2011-2020)",
  #"Tree cover loss proportion per county area (2011-2020)",
  "Protected area (km2)",            
  "Protected area proportion per county area",
  "Cattle (animal number for 2015)",
  "Goats (animal number for 2015)",
  "Sheep (animal number for 2015)",
  "High and very high land use degradation (km2) (2020)",
  "Fatalities (farmers, fishers or pastoralists) (2011-2020)",
  "Conflict events (farmers, fishers or pastoralists) (2011-2020)",
  #"Fatalities (2011-2020)",
  #"Conflict events (2011-2020)",
  "Conflict events (farmers, fishers or pastoralists) (2011-2023)",
  "Fatalities (farmers, fishers or pastoralists) (2011-2023)",
  #new
  "Tree cover (km2) (2021)",
  "Shrubland (km2) (2021)",
  "Grassland (km2) (2021)",
  "Cropland (km2) (2021)",
  "Built-up (km2) (2021)",
 #"Bare or sparse vegetation (km2) (2021)",
  "Snow and Ice (km2) (2021)",
  "Permanent water bodies (km2) (2021)",
  "Herbaceous wetland (km2) (2021)",
#  "Mangroves (km2) (2021)",
#  "Moss and lichen (km2) (2021)",
  "Average Land use change emissions (T C yr-1)/1000 (2011-2020)",
  #"Human population (Median:2010-2020)",
  #"Death rate involving farmers, fishers or pastoralists (2011-2020)",
  #"Death rate (2011-2020)",
  "Human population UNDP estimate (2020)",
  "Death rate farmers, fishers or pastoralists (UNDP estimate) (2011-2020)",
  #"Death rate (UNDP estimate) (2011-2020)",
  "Death rate farmers, fishers or pastoralists (UNDP estimate) (2011-2023)",
  "Livestock (Cattle, goats ans sheep for 2015)"
)

pals <- c(#"gain_2000-2020_ha",
  "Ingres",
  "Ingres",
  "Derain",
  "Benedictus",
  "VanGogh3",
  #"carbon_gross_11_20",
  #"tc_loss_med_prop",
  "VanGogh3",            
  "VanGogh3",
  "Hokusai3",
  "Hokusai3",
  "Hokusai3",
  "OKeeffe2",
  "OKeeffe2",
  "OKeeffe2",
  #"n_fat_t",
  #"n_evts_t",
  "OKeeffe2",
  "OKeeffe2",
  #new
  "VanGogh3",
  "VanGogh3",
  "VanGogh3",
  "VanGogh3",
  "Monet",
  #"Bare/sparse vegetation",
  "Isfahan1",
  "Hokusai2",
  "VanGogh3",
  #  "Mangroves",
  #  "Moss and lichen",
  "Peru2",
  #"median_pop_2010_2020",
  #"n_fat_pop",
  #"n_fat_pop_total",
  "Isfahan1",
  "OKeeffe2",
  #"n_fat_pop_total_world",
  "OKeeffe2",
  "Hokusai3"
)
################################################################################
options(scipen = 999)

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
      #scale_fill_gradientn(colors = met.brewer('VanGogh3', 5)) +
    scale_fill_gradientn(colors = met.brewer(pals[[i]], 6),na.value = "grey40") +
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

################################################################################
#correlations
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
          title ="",# "Kenya variables per county correlations (292 counties/300 counties) with complete data", 
          mar = c(0,0,1,0),
          #number.cex = 0.9,
          number.cex = 0.8,
          number.digits = 2,col = COL2('BrBG'),diag = T,tl.cex = 1.2)
 
 dev.off()
 ###############################################################################
 #saving excel file
 sf::st_write(x_shp, "D:/CIAT_DEFORESTATION/RESULTS/DATA_20240617_2.csv", layer_options = "GEOMETRY=AS_XY",append = F,fid_column_name = "GID3")
 
 ###############################################################################
 x_Cor <- cor(var_mat2,method = "spearman")
 #plot(hclust(as.dist(1-(x_Cor[-c(25),-c(25)])),method = "ward.D2"))
 #plot(hclust(as.dist(1-(x_Cor[-c(24),-c(24)])),method = "ward.D2"))
 plot(hclust(as.dist(1-(x_Cor)),method = "ward.D2"))
 # 
x_Cor[lower.tri(x_Cor)]
x_Cor[lower.tri(x_Cor,diag=TRUE)] <- NA
  


graph_list <- list()
for(i in 1:ncol(x_Cor)){
  #i <- 1
  graph_2 <- as.data.frame(matrix(nrow=ncol(x_Cor),ncol = 3))
  for(j in 1:ncol(x_Cor)){
    #print(j)
    if(j>i){
      graph_2[,1] <- row.names(x_Cor)[i]
      graph_2[j,2] <- colnames(x_Cor)[j]
      graph_2[j,3] <- x_Cor[i,j]
    } else {
      graph_2[,1] <- row.names(x_Cor)[i]
      graph_2[j,2] <- colnames(x_Cor)[j]
      graph_2[j,3] <- NA
    }
  };rm(j)
  graph_list[[i]] <- graph_2
}

graph_f <- do.call(rbind,graph_list)
graph_f <- graph_f[which(!is.na(graph_f[,3])),]
graph_f <- graph_f[which(abs(graph_f[,3])>=0.5),]
colnames(graph_f) <- c("source","target","Spearman")
write.csv(graph_f,"D:/CIAT_DEFORESTATION/RESULTS/cor_selected.csv",na="",row.names = F)

#########################################################################################
#selected
var_mat_selected <- var_df[,c(
                              #"ABG_2020",
                              "cattle_mean",
                              "sheep_mean",
                              "goat_mean",
                              "livestock",
                              "lud_45",
                              "loss_m_p1120",
                              "Grassland",
                              "Cropland",
                              "n_evts")]
var_mat_selected <- var_mat_selected[complete.cases(var_mat_selected),]
png(file = "D:/CIAT_DEFORESTATION/RESULTS/CORRELATIONS_SELECTED.png",width = 1600,height = 1300,units = "px")
corrplot( cor(var_mat_selected,method = "spearman"),
          #cor(var_mat2[,-c(24,25)],method = "spearman"),
          #method = "number", 
          method = 'color',
          cl.pos = 'b', addCoef.col = 'black',
          type = "lower", 
          title ="",# "Kenya variables per county correlations (292 counties/300 counties) with complete data", 
          mar = c(0,0,1,0),
          #number.cex = 0.9,
          number.cex = 2,
          number.digits = 2,col = COL2('BrBG'),diag = T,tl.cex = 1.5)

dev.off()
# var_mat3 <- var_mat2[,c(7,8,9,11,12,13,14)]
 # var_mat3 <- var_mat2[,c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,25,26,27,28,29,30,31)]
 # x_Cor2 <- cor(var_mat3,method = "spearman")
 # plot(hclust(as.dist(1-(x_Cor2)),method = "ward.D2"))
 # # 
 # 
 # png(file = "D:/CIAT_DEFORESTATION/RESULTS/CORRELATIONS_SEL.png",width = 1600,height = 1300,units = "px")
 # 
 # corrplot(cor(var_mat3,method = "spearman"),
 #          #method = "number", 
 #          method = 'color',
 #          cl.pos = 'b', addCoef.col = 'black',
 #          type = "lower", 
 #          title = "Kenya variables per county correlations (292 counties/300 counties) with complete data", 
 #          mar = c(0,0,1,0),
 #          #number.cex = 0.9,
 #          number.cex = 0.8,
 #          number.digits = 2,col = COL2('BrBG'),diag = T,tl.cex = 1.2)
 # 
 # 
 # dev.off()
 # # write.csv(as.data.frame(x_shp),"D:/CIAT_DEFORESTATION/RESULTS/DATA_20240527.csv",quote = T,
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