rm(list = ls());gc()
#load libraries1
require(readxl);require(dplyr);require(geodata);
require(sf);require(matrixStats);require(parallel);
require(ggpmisc)
require(ggplot2);require(MetBrewer);require(corrplot);
require(factoextra);require(FactoMineR);require(xlsx);require(rgeoda);library(car)
#data_dir <- "D:/CIAT_DEFORESTATION/RESULTS"
#outdir
#folder to save files
out_dir <- "D:/CIAT_DEFORESTATION/RESULTS"
#create  folder
#load data
load("D:/CIAT_DEFORESTATION/RESULTS/6_FOREST_LU_LUIEMISS.RData")

x_shp$LUC_emissions <- x_shp$LUC_emissions/1000
################################################################################
vars <- c(#"gain_2000-2020_ha",
  "loss_m1120",
  "loss_m1123",
  #"tc_loss_med_prop",
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
  "n_fat_p1123"
)


#x_data <- x_shp[vars]
#subsetting (Only using subcounties with data)
data_to_moran <- x_shp[vars]
data_to_moran$geometry <- NULL
data_to_moran <- data_to_moran[complete.cases(data_to_moran),]

#subsetting shapefile with data available
x_moran <- x_shp[as.numeric(row.names(data_to_moran)),]
x_moran_2 <- x_moran
x_moran_2 <- x_moran_2[vars]
x_moran_2$geometry <- NULL

X_pca <- PCA(x_moran_2, scale.unit = TRUE, ncp = 5, graph = F)
fviz_eig(X_pca, addlabels = TRUE, ylim = c(0, 50))

var <- get_pca_var(X_pca)
corrplot(var$cos2, is.corr=FALSE)
fviz_pca_var(X_pca, col.var = "contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = T
)

x_Cor <- cor(x_moran_2,method = "spearman")


x_Cor2 <- x_Cor[c("n_fat","n_evts"),]
#plot(hclust(as.dist(1-(x_Cor[-c(25),-c(25)])),method = "ward.D2"))
#plot(hclust(as.dist(1-(x_Cor[-c(24),-c(24)])),method = "ward.D2"))
plot(hclust(as.dist(1-(x_Cor)),method = "ward.D2"))
###############################################################################
#x_moran$ID_i <- 1:nrow(x_moran)


xx <- c(
  "loss_m1120",
  #"loss_m1123",
  #"tc_loss_med_prop",
  "ABG_10_20",
  "ABG_2020",
  #"carbon_gross_11_20",
  #"tc_loss_med_prop",
  "prot_area",            
  #"prot_prop",
  "cattle_mean",
  "goat_mean",
  "sheep_mean",
  "lud_45",
  #"n_fat",
  
  #"n_fat_t",
  #"n_evts_t",
  #"n_evts1124",
  #"n_fat1124",
  #new
  #"Tree cover",
  #"Shrubland",
  "Grassland",
  "Cropland",
  #"Built-up",
  #"Bare/sparse vegetation",
  #"Snow and Ice",
  "Permanent water bodies",
  #"Herbaceous wetland",
  #  "Mangroves",
  #  "Moss and lichen",
  "LUC_emissions",
  #"median_pop_2010_2020",
  #"n_fat_pop",
  #"n_fat_pop_total",
  #"pop2020_w",
  #"n_fat_p",
  #"n_fat_pop_total_world",
  #"n_fat_p1123"
  "n_evts"
)

xxx2 <- x_shp[xx]
xxx2$geometry <- NULL
xxx2 <- xxx2[complete.cases(xxx2),]
xx_cor <- cor(xxx2,method = "spearman")



png(file = "D:/CIAT_DEFORESTATION/RESULTS/CORRELATIONS_22.png",width = 1600,height = 1300,units = "px")
corrplot( xx_cor,
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
