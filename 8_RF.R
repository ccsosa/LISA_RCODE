rm(list = ls());gc()
#load libraries
require(readxl);require(dplyr);require(geodata);
require(sf);require(matrixStats);require(parallel);
require(ggpmisc)
require(ggplot2);require(MetBrewer);require(corrplot);
require(factoextra);require(FactoMineR);require(xlsx);require(rgeoda);library(car)
#data_dir <- "D:/CIAT_DEFORESTATION/RESULTS"
#outdir
#folder to save files
out_dir <- "D:/CIAT_DEFORESTATION/RESULTS/LISA"

#create summary folder
if(!dir.exists(paste0(out_dir,"/CLUSTERS_SUMMARIES"))){
  dir.create(paste0(out_dir,"/CLUSTERS_SUMMARIES"))
}
#create plot folder
if(!dir.exists(paste0(out_dir,"/PLOTS"))){
  dir.create(paste0(out_dir,"/PLOTS"))
}
#create plot biplot folder
if(!dir.exists(paste0(out_dir,"/PLOTS/BIPLOT"))){
  dir.create(paste0(out_dir,"/PLOTS/BIPLOT"))
}
#create pval folder
if(!dir.exists(paste0(out_dir,"/PVAL"))){
  dir.create(paste0(out_dir,"/PVAL"))
}

#create raw folder
if(!dir.exists(paste0(out_dir,"/RAWS"))){
  dir.create(paste0(out_dir,"/RAWS"))
}
#create raw folder
if(!dir.exists(paste0(out_dir,"/SHP"))){
  dir.create(paste0(out_dir,"/SHP"))
}
################################################################################
#load
load("D:/CIAT_DEFORESTATION/RESULTS/6_FOREST_LU_LUIEMISS.RData")
################################################################################
vars <- c(#"gain_2000-2020_ha",
  "tc_loss_med_11_20",
  "ABG_2010_2020",
  #"carbon_gross_11_20",
  "tc_loss_med_prop",
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
  "Average forest above-ground biomass difference (Mg/ha)(2010-2020)",
  #"Forest carbon gross emissions (Mg CO2e) (Median: 2011-2020)",
  "Tree cover loss proportion per county area (2011-2020)",
  "Protected area (km2)",            
  "Protected area proportion per county",
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

###############################################################################
data_to_cl <- x_shp[c("n_fat_pop",
                      "tc_loss_med_11_20",
                      "median_pop_2010_2020",
                      "ABG_2010_2020",
                      "cattle_mean" ,
                      #"carbon_gross_11_20" , 
                      "lud_45" ,
                      "LUC_emissions" ,
                      "goat_mean" ,
                      "sheep_mean",
                      "median_pop_2010_2020")]
data_to_cl$geometry <- NULL
data_to_cl <- data_to_cl[complete.cases(data_to_cl),]
data_to_cl <- x_shp[as.numeric(row.names(data_to_cl)),c("n_fat_pop",
                                                        "tc_loss_med_11_20" ,
                                                        "ABG_2010_2020",
                                                        "cattle_mean" ,
                                                        #"carbon_gross_11_20" , 
                                                        "lud_45" ,
                                                        "LUC_emissions" ,
                                                        "goat_mean" ,
                                                        "sheep_mean",
                                                        "median_pop_2010_2020")]
queen_w_cl <- rgeoda::queen_weights(data_to_cl)
wcl_clusters <- skater(4, queen_w_cl, data_to_cl)

###############################################################################
xx <- x_shp[,-c(1:18)]
xx$geometry <- NULL
#colnames(xx) <- captions
xx <- xx[complete.cases(xx),]

model <- lm(n_fat_pop ~
              #gain_20002020_ha    +
              tc_loss_med_11_20 +
              ABG_2010_2020 +
              #carbon_gross_11_20 +
              #tc_loss_med_prop +  
              #protarea_area +
              #protarea_prop + 
              cattle_mean +
              goat_mean + 
              sheep_mean + 
              lud_45 + 
              #Tree cover + 
              Shrubland +
              Grassland + 
              Cropland + 
              #Built-up               +
              #Bare/sparse vegetation +
              #Snow and Ice           +
              #Permanent water bodies +
              #Herbaceous wetland     +
              #Mangroves              +
              #Moss and lichen        +
              LUC_emissions +
              median_pop_2010_2020, 
            data = xx)
#plot(model)
plot(model, which = 1, main = "Model Fit")
summary(model)
#create vector of VIF values
vif_values <- car::vif(model)
vif_values

#create horizontal bar chart to display each VIF value
barplot(vif_values, main = "VIF Values", horiz = T, col = "steelblue",las=2)

vif_values <-vif_values[vif_values < 10]
#add vertical line at 5
#abline(v = 5, lwd = 3, lty = 2)
#row.names(xx) <- NULL#as.character(1:nrow(xx))
#st_geometry(xx) <- NULL
#colnames(xx) <- captions
res.pca = PCA(xx,scale.unit = T,graph = F)
x_H <- HCPC(res.pca, nb.clust = -1, min = 3, max = NULL, graph = F)
# fviz_dend(x_H, 
#           cex = 0.7,                     # Label size
#           palette = "jco",               # Color palette see ?ggpubr::ggpar
#           rect = TRUE, rect_fill = TRUE, # Add rectangle around groups
#           rect_border = "jco",           # Rectangle color
#           labels_track_height = 0.8      # Augment the room for labels
# )

fviz_eig(res.pca, addlabels = TRUE, ylim = c(0, 50))
fviz_pca_var(res.pca, col.var = "black")


fviz_pca_biplot(res.pca, geom.ind = "point",
                col.ind = x_H$data.clust$clust, # color by groups
                #palette = c("#00AFBB", "#E7B800", "#FC4E07"),
                addEllipses = TRUE, ellipse.type = "convex",
                legend.title = "Groups",col.var = "black",
)


###############################################################################
require(caret);require(ranger)
#lmHeight = lm(n_events~tc_loss_med_11_20+cattle_mean+carbon_gross_11_20, data = x_shp) #Create the linear regression
set.seed(1)

num_trees = seq(0,10000,10)
num_trees[1] <- 10
#m_try = sqrt(5)

RF_list <- list()
tuning_df <- list()
x_shp_to <- as.data.frame(x_shp)
x_shp_to <- x_shp_to[complete.cases(x_shp_to[,c("n_fat_pop",
                                                # "tc_loss_med_11_20" ,
                                                #   "cattle_mean" ,
                                                #   "carbon_gross_11_20" ,
                                                #   "lud_45" ,
                                                #   "LUC_emissions" ,
                                                #   "goat_mean" ,
                                                #   "sheep_mean"
                                                names(vif_values))]),]


for(i in 1:length(num_trees)){
  # i <- 1
  x <- as.data.frame(matrix(ncol=5,nrow=1))
  colnames(x) <- c("mtry","ntree","OOB","RMSE","i")
  #lmHeight = lm(n_events~tc_loss_med_11_20+cattle_mean+carbon_gross_11_20, data = x_shp) #Create the linear regression
  
  first_rf <- ranger(n_fat_pop ~
                       ABG_2010_2020 + 
                       #gain_20002020_ha    +
                       tc_loss_med_11_20 +
                       #carbon_gross_11_20 +
                       #tc_loss_med_prop +
                       #protarea_area +
                       #protarea_prop +
                       cattle_mean +
                       goat_mean +
                       sheep_mean +
                       lud_45 +
                       #Tree cover +
                       Shrubland +
                       Grassland +
                       Cropland +
                       #Built-up               +
                       #Bare/sparse vegetation +
                       #Snow and Ice           +
                       #Permanent water bodies +
                       #Herbaceous wetland     +
                       #Mangroves              +
                       #Moss and lichen        +
                       LUC_emissions+
                        median_pop_2010_2020,
                     num.trees = num_trees[i], #mtry = m_try, #
                     importance = "impurity",
                     data = x_shp_to,oob.error = T,num.threads = 4,replace = T,seed = 1000)
  
  RF_list[[i]] <- first_rf
  x$mtry <-  first_rf$mtry
  x$ntree <- num_trees[i]
  x$OOB <- first_rf$r.squared
  x$RMSE <- first_rf$prediction.error
  x$i <- i
  tuning_df[[i]] <- x
  rm(x)
  rm(first_rf)
};rm(i)

tuning_df <- do.call(rbind,tuning_df)
tuning_df <- tuning_df[order(tuning_df$OOB,decreasing = T),]
x <- predict(RF_list[tuning_df$i[1]][1], x_shp_to)
MAE(x_shp_to$n_fatalities,x[[1]]$predictions)
Metrics::rmse(x_shp_to$n_fatalities,x[[1]]$predictions)
plot(x_shp_to$n_fatalities,x[[1]]$predictions)
################################################################################
library(ggpmisc);library(ggplot2)
x_o <- data.frame(Observed=x_shp_to$n_fatalities,predicted=x[[1]]$predictions)
#using default formula, label and methods
ggplot(data = x_o, aes(x = Observed, y = predicted)) +
  #stat_poly_line() +
  #stat_poly_eq() +
  geom_point()


res <- caret::postResample(x[[1]]$predictions, x_shp_to$n_fatalities)
res
# #rm(x)
# 
# 
# 
imps <- data.frame(var = names(vif_values),
                   imps = RF_list[tuning_df$i[1]][[1]]$variable.importance/max(RF_list[tuning_df$i[1]][[1]]$variable.importance))

imps <- imps[order(imps$imps,decreasing = F),]
colnames(imps) <- c("vars","score")
imps$vars <- factor(imps$vars,levels = imps$var)
#x <imps %>%
ghmc <-   ggplot(data=imps,aes(x = vars,y = score)) +
  geom_point(size = 10, colour = "#ff6767") +
  coord_flip() +
  labs(x = "Predictors", y = "Importance scores") +
  theme_bw(18)

ggsave(plot = ghmc, filename = paste0(out_dir,"/","GINI.png"), 
       units = 'in', width = 10, height = 8, dpi = 300)


save.image("D:/CIAT_DEFORESTATION/RESULTS/8_RF.RData")

################################################################################
# tgrid <- expand.grid(
#   .mtry = 2:200,
#   .splitrule = "gini",
#   .min.node.size = c(10, 20)
# )
# 
# 
# model_caret <- train(n_fatalities ~
#                        ABG_2010_2020 + 
#                        #gain_20002020_ha    +
#                        tc_loss_med_11_20 +
#                        #carbon_gross_11_20 +
#                        #tc_loss_med_prop +
#                        #protarea_area +
#                        #protarea_prop +
#                        cattle_mean +
#                        goat_mean +
#                        sheep_mean +
#                        lud_45 +
#                        #Tree cover +
#                        Shrubland +
#                        Grassland +
#                        Cropland +
#                        #Built-up               +
#                        #Bare/sparse vegetation +
#                        #Snow and Ice           +
#                        #Permanent water bodies +
#                        #Herbaceous wetland     +
#                        #Mangroves              +
#                        #Moss and lichen        +
#                        LUC_emissions,
#                       data = x_shp_to,
#                      method = "ranger",
#                      trControl = trainControl(method="cv", number = 10, verboseIter =F, classProbs =F),
#                      tuneGrid = tgrid,
#                      num.trees = 100,
#                      importance = "permutation")
#https://developer.ibm.com/tutorials/awb-implement-xgboost-in-r/
# 80/20 split

# require(xgboost)
# split_indices <- caret::createDataPartition(x_shp_to$LUC_emissions, p = 0.8, list = FALSE)
# X_train <- x_shp_to[split_indices, ]
# X_test <- x_shp_to[-split_indices, ]
# y_train <- x_shp_to[split_indices]
# y_test <- x_shp_to[-split_indices]
# 
# default_model <- xgboost(data = x_shp_to,
#                          label = y_train,
#                          booster = "gbtree",
#                          objective = "binary:logistic",
#                          nrounds = 100,
#                          verbose = 0)
