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
#create  folder
if(!dir.exists(out_dir)){
  dir.create(out_dir)
}
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
  "n_fat_pop_total",
  "pop2020_worldpop",
  "n_fat_pop_world",
  "n_fat_pop_total_world"
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
  "Death rate (2011-2020)",
  "Human population UNDP estimate (2020)",
  "Death rate involving farmers, fishers or pastoralists (UNDP estimate) (2011-2020)",
  "Death rate (UNDP estimate) (2011-2020)"
)

x_shp_to_csv <- x_shp
x_shp_to_csv$geometry <- NULL
write.csv(x_shp_to_csv,paste0(out_dir,"/","TO_CSV.csv"),row.names = F,na = "")
# #y
# var_y <- c('n_fatalities')
# #x
# var_x <- 'tc_loss_med_prop'


LISA_MULTI <- function(var_x,var_y,out_dir,x_shp,vars,captions){
  #https://bookdown.org/victor_morales/SpatialEconometrics/pruebas-de-autocorrelaci%C3%B3n.html
################################################################################
#using data to correctly plot data
colors_to_plot <- data.frame(Cluster=c("Not significant",
                             "High-High",
                             "Low-Low",
                             "High-Low",
                             "Low-High",
                             "Undefined",
                             "Isolated"),
                             values=seq(0,6,1),
                             color= c(
                               "#eeeeee",
                               "#FF0000",
                               "#0000FF",
                               "#a7adf9",
                               "#f4ada8",
                               "#464646",
                               "#999999" 
                             )
                               )

#colors_to_plot$Cluster <- factor(colors_to_plot$Cluster,levels = colors_to_plot$Cluster)
#colors_to_plot$color <- factor(colors_to_plot$color,levels = colors_to_plot$color)
################################################################################
# x_shp1 <- geodata::gadm(country = "KEN",level = 1,
#                         path = "D:/CIAT_DEFORESTATION/DATA/NEW/GADM")
x_shp1 <- sf::st_read("D:/CIAT_DEFORESTATION/DATA/NEW/KEN_ILRI/gadm41_KEN_1.shp")
x_shp1 <- sf::st_as_sf(x_shp1)
#plot(st_geometry(x_shp1))
#load("D:/CIAT_DEFORESTATION/RESULTS/4_FOREST_PROCAREA_LI_LUD_CONFLICT.RData")
#load("D:/CIAT_DEFORESTATION/RESULTS/5_FOREST_LU.RData")


#summary(queen_w)
# nbrs <- rgeoda::get_neighbors(queen_w, idx = 1)
# cat("\nNeighbors of the 1-st observation are:", nbrs)
# lag <- spatial_lag(queen_w, x_shp[c('n_fatalities', 'tc_loss_med_prop')])
# lag

#save_weights(gda_w, id_variable, out_path, layer_name = "")
#queen to LISA
#crm_prp = x_shp["tc_loss_med_prop"]
#using moran bivariatee 


#subsetting (Only using subcounties with data)
data_to_moran <- x_shp[c(var_x,var_y)]
data_to_moran$geometry <- NULL
data_to_moran <- data_to_moran[complete.cases(data_to_moran),]

#subsetting shapefile with data available
x_moran <- x_shp[as.numeric(row.names(data_to_moran)),]
x_moran$ID_i <- 1:nrow(x_moran)
sf::write_sf(x_moran,paste0(out_dir,"/SHP/","LI_",var_x,"_",var_y,".shp"))
#base <- sf::st_read(paste0(data_dir,"/","ken_adm_iebc_20191031_shp/ken_admbnda_adm2_iebc_20191031.shp"))
queen_w <- rgeoda::queen_weights(x_moran)
save_weights(queen_w, x_moran["ID_i"],#x_moran,
             out_path = paste0(out_dir,"/SHP/","LI_",var_x,"_",var_y,".gal"))


#data_to_moran[,var_y][which(data_to_moran[,var_y]==0)] <- 0.000000001
#data_to_moran[,var_x][which(data_to_moran[,var_x]==0)] <- 0.00000001
#https://geodacenter.github.io/workbook/6c_local_multi/lab6c.html#bivariate-local-moran
qsa <- rgeoda::local_bimoran(w = queen_w, 
                          df = x_moran[c(var_x,var_y)],
                          permutations = 9999,
                          significance_cutoff = 0.05,
                          cpu_threads = 4,
                          seed = 10000)

# plot(as.numeric(scale(data_to_moran[,1],center = T,scale = T)),
#      spatial_lag(queen_w, x_shp[c(var_y)])[,1],col=cats)
# 

# gda_lisa_path<- local_bijoincount(queen_w,
#                           x_shp[c('n_fatalities', 'tc_loss_med_prop')],
#                           #permutations = 9999,
#                           #significance_cutoff = 0.05,
#                           #cpu_threads = 4,
#                           #seed = 1000
#                           )
#using model uni7
#lisa <- local_moran(queen_w, crm_prp,permutations = 10000)
#spatial lag
# crm_lag1 <- rgeoda::local_moran(queen_w, x_shp[c('n_fatalities')])
# crm_lag2 <- rgeoda::local_moran(queen_w, x_shp[c('tc_loss_med_prop')])

# crm_lag1 <- rgeoda::spatial_lag(queen_w, x_shp[c('n_fatalities')])
# crm_lag2 <- rgeoda::spatial_lag(queen_w, x_shp[c('tc_loss_med_prop')])

#Spatial autocorrelation values
lms <- rgeoda::lisa_values(gda_lisa = qsa)
#mean(lms)
#lms

#get p-values
pvals <- rgeoda::lisa_pvalues(gda_lisa = qsa)
#get the cluster label
lbls <- rgeoda::lisa_labels(gda_lisa = qsa)
#lbls
#col_lisa <- lisa_colors(gda_lisa)
clrs <- setNames(rgeoda::lisa_colors(gda_lisa = qsa), nm = lbls)
#FDR
#fdr <- rgeoda::lisa_fdr(gda_lisa = qsa, 0.05)
#fdr
#get the LIS clusters
cats <- rgeoda::lisa_clusters(gda_lisa = qsa)#, cutoff = fdr)

if(sum(cats)>0){
#cats
#validating clusters   
results <- rgeoda::spatial_validation(sf_obj = x_moran, clusters = cats, w = queen_w)
#results
#x to y plots
# values_plot <- data.frame(
#            #VAR1=lisa_values(crm_lag1),
#            #VAR2=lisa_values(crm_lag2),
#           VAR1= scale(as.numeric(as.matrix(x_shp[c('n_fatalities')])[,1]),
#                       center = T,scale = T),
#           #VAR1=crm_lag1[,1],
#           VAR2=crm_lag2[,1],
#            CLUSTER=cats,
#            col2=NA,
#            pvals=pvals)
# for(i in 1:length(clrs)){
#   #i <- 1
#   values_plot$col2[which(values_plot$CLUSTER==i-1)] <- as.character(clrs[i])
#   values_plot$CLUSTER[which(values_plot$CLUSTER==i-1)] <- names(clrs[i])
#  
# }
# #values_plot$pvals >0.05
# values_plot$col2 <- factor(values_plot$col2)
# values_plot$CLUSTER <- factor(values_plot$CLUSTER)
# 
# p <- ggplot(data = values_plot, aes(x = VAR1, 
#                              y = VAR2,
#                              colour= CLUSTER))
#                              
#                              #group = CLUSTER))
# p <-  p +
#    geom_hline(yintercept = mean(values_plot$VAR2),linewidth = 2)+
#    geom_vline(xintercept = mean(values_plot$VAR1),linewidth = 2)+
#   geom_point(size = 4)+
#  scale_colour_manual(values = as.character(clrs),labels=names(clrs))
# #+
#   # geom_point(colour = "grey90", size = 1.5)
# p


#Clusters

# plot(st_geometry(x_shp), 
#      col=sapply(cats, function(x){return(lisa_colors(gda_lisa)[[x+1]])}), 
#      border = "#333333", lwd=0.2
#      )
# title(main = "Local Moran Map for n_fatalities and tc_loss_med_prop")
# legend('bottomleft', legend = lbls, fill = clrs, border = "#eeeeee",cex = 0.5)


#adding new values to plot maps
x_shp_copy <- x_moran
x_shp_copy$CLUSTERS <- cats
x_shp_copy$pvals <- pvals
x_shp_copy$col2 <- NA


################################################################################
#preparing for plotting, adding colors and labels
for(i in 1:nrow(colors_to_plot)){
  #i <- 1
  x_shp_copy$CLUSTERS[which(x_shp_copy$CLUSTERS==colors_to_plot$values[[i]])] <- colors_to_plot$Cluster[[i]]
  x_shp_copy$col2[which(x_shp_copy$CLUSTERS==colors_to_plot$Cluster[[i]])] <- colors_to_plot$color[[i]]

}

#adding factor to standardize
x_shp_copy$col2 <- factor(x_shp_copy$col2,levels = colors_to_plot$color)
x_shp_copy$CLUSTERS <- factor(x_shp_copy$CLUSTERS,levels = colors_to_plot$Cluster)

################################################################################
#data to plot xy plot
var_reg <- x_shp_copy[,c(var_x,var_y)]
var_reg$geometry <- NULL
lag_y <- rgeoda::spatial_lag(gda_w =queen_w,df = x_moran[,var_y])
data_to_plot <- data.frame(x=var_reg[,1],
                           y=lag_y$Spatial.Lag,
                           cluster=x_shp_copy$CLUSTERS,
                           colors=x_shp_copy$col2)
# Create a regression model
M <- lm(lag_y$Spatial.Lag ~ var_reg[,1])
#cf <- coef(M)
# Plot the data

################################################################################
#Bivariate Moran plot

#captions to titles
caption_to_x <- vars==var_x;
caption_to_x <- 1:length(captions)*caption_to_x
caption_to_x <- caption_to_x[caption_to_x!=0]
var_to_x <- vars[caption_to_x]
caption_to_x <- captions[caption_to_x]
#captions to titles
caption_to_y <- vars==var_y;
caption_to_y <- 1:length(captions)*caption_to_y
caption_to_y <- caption_to_y[caption_to_y!=0]
var_to_y <- vars[caption_to_y]
caption_to_y <- captions[caption_to_y]
################################################################################
x_lag_plot <- ggplot(data_to_plot,aes(x,y,color = cluster)) +
  geom_point(aes(colour =cluster)) +
  scale_color_manual(values = c("Not significant" ="#eeeeee",
                                "High-High" ="#FF0000",
                                "Low-Low" ="#0000FF",
                                "High-Low" ="#a7adf9",
                                "Low-High" ="#f4ada8",
                                "Undefined" ="#464646",
                                "Isolated" ="#999999"))+
  xlab(caption_to_x)+
  ylab(paste("Lagged",caption_to_y))+
  # geom_smooth(aes(colour=cluster), method = "lm") +
  #geom_smooth(color="black", method = "lm") +
  
  #stat_smooth(method="lm",se=F)+
  annotate("text",x=max(data_to_plot$x)+-2*sd(data_to_plot$x),
           y=max(data_to_plot$y)-2*sd(data_to_plot$y),
           label=paste0("I==",round(mean(lms),3)),parse=TRUE)+
  #geom_smooth()+
  #stat_poly_line() +
  # stat_poly_eq(use_label("eq")) +
  # stat_poly_eq(label.y = 0.9) +
  #stat_smooth(#formula = lag_y$Spatial.Lag ~ var_reg[,1], 
  #data=cbind(var_reg[,1],lag_y$Spatial.Lag),
  #            fullrange = F,
  #            method = lm,color="black",
  #            se = F,
  #            show.legend = T, size=, colour = 'blue')+
  #
   geom_abline(intercept = M$coefficients[1],
               slope=M$coefficients[2])+
  geom_hline(yintercept = mean(lag_y$Spatial.Lag))+
  geom_vline(xintercept = mean(var_reg[,1]))
  #geom_hline(yintercept = mean(var_reg[,1]))+
  #geom_vline(xintercept = mean(var_reg[,1]))
#+
#scale_x_continuous(limits=c(0,70)) +
#scale_y_continuous(limits=c(0,100)) + 
#coord_fixed(ratio=70/100)  

ggsave(plot = x_lag_plot, filename = paste0(out_dir,"/PLOTS/BIPLOT/","LISA_PLOT_",var_to_x,"_",var_to_y,".tiff"), 
       units = 'in', width = 10, height = 8, dpi = 300)


################################################################################
ghmc <- ggplot(x_shp_copy) + 
  geom_sf(data = x_shp_copy, aes(fill=CLUSTERS), col = 'white', lwd = 0.3) + 
  scale_fill_manual(values = c("Not significant" ="#eeeeee",
                               "High-High" ="#FF0000",
                               "Low-Low" ="#0000FF",
                               "High-Low" ="#a7adf9",
                               "Low-High" ="#f4ada8",
                               "Undefined" ="#464646",
                               "Isolated" ="#999999")
                    )+
  #scale_fill_gradientn(colors = rev(met.brewer('VanGogh3', 5))) +
  #scale_fill_gradientn(colors = rev(met.brewer('VanGogh3', 5))) +
  
  #scale_fill_gradientn(colors = met.brewer('VanGogh3', 5)) +
  geom_sf(data = x_shp1, fill = NA, col = "gray60", lwd = 0.75) +
  # geom_sf(data = expn, fill = '#C7C7C7', col = 'white', lwd = 0.3) +
  # geom_sf(data = crgn, fill = 'grey70', col = '#E0E0E0', lwd = 0.3) + 
  # geom_sf(data = mpos, fill = '#FFFFFF', col = 'grey90', lwd = 0.3) +
  # geom_sf(data = rios, fill = '#3F73A3', col = '#3F73A3') + 
  # geom_sf_text(data = crgn, aes(label = corregimie), family = 'serif', size = 2.5, col = 'grey40') +
  # geom_sf_text(data = cmns, aes(label = comuna), family = 'serif', size = 4, face = 'bold', col = 'white') +
  # geom_sf_text(data = expn, aes(label = gid), family = 'serif', size = 2.5, col = 'grey40') +
  # geom_sf_text(data = mpos, aes(label = MPIO_CNMBR), family = 'serif', size = 2.5, col = 'grey40') +
  labs(x = 'Longitude', y = 'Latitude',fill="") + 
  ggtitle(label = paste("LISA for: ",caption_to_x,"and",caption_to_y)) + 
  #coord_sf(xlim = ext(shp)[1:2], ylim = ext(shpf)[3:4]) +
  theme_minimal() + 
  theme(legend.position = 'right', 
        legend.text = element_text(family = 'serif'), 
        legend.title = element_text(family = 'serif', face = 'bold'),
        plot.title = element_text(size = 10, face = 'bold', hjust = 0.5, family = 'serif'),
        plot.caption = element_text(family = 'serif'),
        axis.text.x = element_text(family = 'serif'), 
        axis.text.y = element_text(hjust = 0.5, angle = 90, family = 'serif'),
        axis.title.x = element_text(family = 'serif'),
        axis.title.y = element_text(family = 'serif'),
        legend.key.height = unit(2.5, 'line')) 
#


ggsave(plot = ghmc, filename = paste0(out_dir,"/PLOTS/","LI_",var_to_x,"_",var_to_y,".tiff"), 
       units = 'in', width = 10, height = 8, dpi = 300)


#}

# #p values

png(file =paste0(out_dir,"/PVAL/","LI_",var_to_x,"_",var_to_y,".png"),width = 900,height = 900,units = "px")
par(mfrow = c(1, 2)) # Create a 2 x 2 plotting matrix


#Clusters

plot(st_geometry(x_shp_copy),
     col=sapply(cats, function(x){return(lisa_colors(gda_lisa=qsa)[[x+1]])}),
     border = "#333333", lwd=0.2
     )

legend('bottomleft', legend = lbls, fill = clrs, border = "#eeeeee")

p_labels <- c("Not significant", "p <= 0.05", "p <= 0.01", "p <= 0.001")
p_colors <- c("#eeeeee", "#84f576", "#53c53c", "#348124")


plot(st_geometry(x_shp_copy),
     col=sapply(pvals, function(x){
       if (x <= 0.001) return(p_colors[4])
       else if (x <= 0.01) return(p_colors[3])
       else if (x <= 0.05) return (p_colors[2])
       else return(p_colors[1])
     }),
     border = "#333333", lwd=0.2)
 #title(main = paste("p values clusters for: ",caption_to_x,"and",caption_to_y))
 legend('bottomleft', legend = p_labels, fill = p_colors, border = "#eeeeee")
 
 
 title(main = paste("LISA for: ",caption_to_x,"and",caption_to_y),
       line = -4, outer = TRUE,cex.main=1)
dev.off()

################################################################################
#save results
x_data <- x_shp_copy[,c("NAME_1","NAME_2","NAME_3","GID_3",var_x,var_y,"CLUSTERS","pvals")]
x_data <- cbind(x_data,lms)
if(file.exists(paste0("D:/CIAT_DEFORESTATION/RESULTS/LISA/RAWS/",var_to_x,"_",var_to_y,".csv"))){
  print("already saved")
} else {
  sf::st_write(x_data, paste0("D:/CIAT_DEFORESTATION/RESULTS/LISA/RAWS/",var_to_x,"_",var_to_y,".csv"),
               layer_options = "GEOMETRY=AS_XY")  
}
################################################################################
#cluster_number summary


cluster_unique <- unique(x_data$CLUSTERS)
cluster_unique <- cluster_unique[which(cluster_unique!="Not significant")]

summary_df_List <- list()

for(i in 1:length(cluster_unique)){
  #i <- 1
  #subsetting
  summary_df <- data.frame(matrix(nrow = 2,ncol = 8))
  colnames(summary_df) <- c("variable","cluster","min","max","mean","sd","median","n")
  
  #X variable
  x1 <- x_data[which(x_data$CLUSTERS==as.character(cluster_unique[[i]])),][var_to_x]
  x1$geometry <- NULL
  #Y variable
  x2 <- x_data[which(x_data$CLUSTERS==as.character(cluster_unique[[i]])),][var_to_y]
  x2$geometry <- NULL
  #
  x3 <- x_data[which(x_data$CLUSTERS==as.character(cluster_unique[[i]])),]["lms"]
  x3$geometry <- NULL
  
  #
  summary_df$variable <- c(var_to_x,var_to_y)
  summary_df$cluster <- as.character(cluster_unique[[i]])
  summary_df$min <- c(min(x1[,1],na.rm = T),min(x2[,1],na.rm = T))
  summary_df$max <- c(max(x1[,1],na.rm = T),max(x2[,1],na.rm = T))
  summary_df$mean <- c(mean(x1[,1],na.rm = T),mean(x2[,1],na.rm = T))
  summary_df$sd <- c(sd(x1[,1],na.rm = T),sd(x2[,1],na.rm = T))
  summary_df$median <- c(median(x1[,1],na.rm = T),median(x2[,1],na.rm = T))
  summary_df$n <- c(nrow(x1),nrow(x2))
  summary_df$moran_cluster <- mean(x3$lms,na.rm=T)
  summary_df$moran_mean <- mean(lms,na.rm=T)
  summary_df$bonferroni <- lisa_bo(qsa,current_p = 0.05)
  #CLUSTERS_SUMMARIES
  summary_df_List[[i]] <- summary_df
  
};rm(i)
summary_df_List <- do.call(rbind,summary_df_List)
write.csv(summary_df_List,paste0("D:/CIAT_DEFORESTATION/RESULTS/LISA/CLUSTERS_SUMMARIES/",
                            var_to_x,"_",var_to_y,".csv"),row.names = F)
} else {
  print("NO ASSOCIATIONS FOUND!")
}

print("DONE!")
return(ghmc)
}

################################################################################

var_y <- 'n_fatalities'
#x
var_x <- 'tc_loss_med_prop'

#load
#load("D:/CIAT_DEFORESTATION/RESULTS/6_FOREST_LU_LUIEMISS.RData")


LISA_MULTI(var_x,var_y,out_dir,x_shp,vars,captions)

################################################################################
var_y <- 'n_events'
#x
var_x <- 'tc_loss_med_prop'

#load
#load("D:/CIAT_DEFORESTATION/RESULTS/6_FOREST_LU_LUIEMISS.RData")


LISA_MULTI(var_x,var_y,out_dir,x_shp,vars,captions)
################################################################################
var_y <- 'n_fatalities'
#x
var_x <- 'tc_loss_med_11_20'

#load
#load("D:/CIAT_DEFORESTATION/RESULTS/6_FOREST_LU_LUIEMISS.RData")


LISA_MULTI(var_x,var_y,out_dir,x_shp,vars,captions)
################################################################################
var_y <- 'n_events'
#x
var_x <- 'tc_loss_med_11_20'

#load
#load("D:/CIAT_DEFORESTATION/RESULTS/6_FOREST_LU_LUIEMISS.RData")


LISA_MULTI(var_x,var_y,out_dir,x_shp,vars,captions)


################################################################################
var_y <- 'n_events'
#x
var_x <- 'lud_45'

#load
#load("D:/CIAT_DEFORESTATION/RESULTS/6_FOREST_LU_LUIEMISS.RData")


LISA_MULTI(var_x,var_y,out_dir,x_shp,vars,captions)
################################################################################
var_y <- 'n_fatalities'
#x
var_x <- 'lud_45'

#load
#load("D:/CIAT_DEFORESTATION/RESULTS/6_FOREST_LU_LUIEMISS.RData")


LISA_MULTI(var_x,var_y,out_dir,x_shp,vars,captions)
################################################################################
var_y <- 'n_fatalities'
#x
var_x <- 'cattle_mean'

#load
#load("D:/CIAT_DEFORESTATION/RESULTS/6_FOREST_LU_LUIEMISS.RData")


LISA_MULTI(var_x,var_y,out_dir,x_shp,vars,captions)

################################################################################
var_y <- 'n_events'
#x
var_x <- 'cattle_mean'

#load
#load("D:/CIAT_DEFORESTATION/RESULTS/6_FOREST_LU_LUIEMISS.RData")


LISA_MULTI(var_x,var_y,out_dir,x_shp,vars,captions)
# ################################################################################
var_y <- 'n_events'
#x
var_x <- 'LUC_emissions'

#load
#load("D:/CIAT_DEFORESTATION/RESULTS/6_FOREST_LU_LUIEMISS.RData")
LISA_MULTI(var_x,var_y,out_dir,x_shp,vars,captions)
###############################################################################

var_y <- 'n_fatalities'
#x
var_x <- 'LUC_emissions'

#load
#load("D:/CIAT_DEFORESTATION/RESULTS/6_FOREST_LU_LUIEMISS.RData")


LISA_MULTI(var_x,var_y,out_dir,x_shp,vars,captions)
# ###############################################################################
###############################################################################

var_y <- 'n_fatalities'
#x
var_x <- 'goat_mean'

#load
#load("D:/CIAT_DEFORESTATION/RESULTS/6_FOREST_LU_LUIEMISS.RData")


LISA_MULTI(var_x,var_y,out_dir,x_shp,vars,captions)
# ###############################################################################
var_y <- 'n_events'
#x
var_x <- 'goat_mean'

#load
#load("D:/CIAT_DEFORESTATION/RESULTS/6_FOREST_LU_LUIEMISS.RData")


LISA_MULTI(var_x,var_y,out_dir,x_shp,vars,captions)
# ###############################################################################
var_y <- 'n_fatalities'
#x
var_x <- 'tc_loss_med_11_20'
LISA_MULTI(var_x,var_y,out_dir,x_shp,vars,captions)
# ###############################################################################
var_y <- 'n_events'
#x
var_x <- 'tc_loss_med_11_20'
LISA_MULTI(var_x,var_y,out_dir,x_shp,vars,captions)
# ###############################################################################
var_y <- 'n_fatalities_total'
#x
var_x <- 'n_events_total'
LISA_MULTI(var_x,var_y,out_dir,x_shp,vars,captions)
# ###############################################################################
var_y <- 'n_fatalities'
#x
var_x <- 'n_events'
LISA_MULTI(var_x,var_y,out_dir,x_shp,vars,captions)
# ###############################################################################
var_y <- 'n_fatalities'
#x
var_x <- 'protarea_area'
LISA_MULTI(var_x,var_y,out_dir,x_shp,vars,captions)
# ###############################################################################
var_y <- 'n_fatalities'
#x
var_x <- 'ABG_2010_2020'
LISA_MULTI(var_x,var_y,out_dir,x_shp,vars,captions)
# ###############################################################################
var_y <- 'n_events'
#x
var_x <- 'ABG_2010_2020'
LISA_MULTI(var_x,var_y,out_dir,x_shp,vars,captions)
# ###############################################################################

var_y <- 'n_fat_pop'
#x
var_x <- 'ABG_2010_2020'
LISA_MULTI(var_x,var_y,out_dir,x_shp,vars,captions)
# ###############################################################################

var_y <- 'n_fat_pop'
#x
var_x <- 'tc_loss_med_11_20'
LISA_MULTI(var_x,var_y,out_dir,x_shp,vars,captions)
# ###############################################################################
var_y <- 'n_fat_pop'
#x
var_x <- 'cattle_mean'
LISA_MULTI(var_x,var_y,out_dir,x_shp,vars,captions)
# ###############################################################################
var_y <- 'n_fat_pop'
#x
var_x <- 'lud_45'
LISA_MULTI(var_x,var_y,out_dir,x_shp,vars,captions)
# ###############################################################################
var_y <- 'n_fat_pop'
#x
var_x <- 'LUC_emissions'
LISA_MULTI(var_x,var_y,out_dir,x_shp,vars,captions)
# ###############################################################################
var_y <- 'n_fat_pop'
#x
var_x <- 'median_pop_2010_2020'
LISA_MULTI(var_x,var_y,out_dir,x_shp,vars,captions)
# ###############################################################################
# 
# 
# var_y <- 'tc_loss_med_11_20'
# #x
# var_x <- 'cattle_mean'
# 
# #load
# #load("D:/CIAT_DEFORESTATION/RESULTS/6_FOREST_LU_LUIEMISS.RData")
# 
# 
# LISA_MULTI(var_x,var_y,out_dir,x_shp,vars,captions)
# 
# 
# var_y <- 'tc_loss_med_11_20'
# #x
# var_x <- 'n_events'
# 
# #load
# #load("D:/CIAT_DEFORESTATION/RESULTS/6_FOREST_LU_LUIEMISS.RData")
# 
# 
# LISA_MULTI(var_x,var_y,out_dir,x_shp,vars,captions)
# 
