rm(list = ls());gc()
#load libraries
require(readxl);require(dplyr);require(geodata);
require(sf);require(matrixStats);require(parallel);
require(ggplot2);require(MetBrewer);require(corrplot);
require(factoextra);require(FactoMineR);require(xlsx);
require(reshape2);require(ggpubr);require(tseries)
#data_dir <- "D:/CIAT_DEFORESTATION/RESULTS"
library(dtw);library(NbClust)
library(dtwclust);
require(BBmisc)
x_shp1 <- sf::st_read("D:/CIAT_DEFORESTATION/DATA/NEW/KEN_ILRI/gadm41_KEN_1.shp")

out_dir <- "D:/CIAT_DEFORESTATION/RESULTS"
#x_shp1 <- geodata::gadm(country = "KEN",level = 1,
#                        path = 

def_shp <- sf::st_read(paste0(out_dir,"/","deforestation_time.shp"))
time_shp <- sf::st_read(paste0(out_dir,"/","n_evts_time.shp"))

def_shp2 <- def_shp
area <- (as.numeric(st_area(def_shp2)))*(1e-6/1)
def_shp2$area <- area



def_shp_df <- def_shp
time_shp_df <- time_shp

def_shp_df$geometry <- NULL
time_shp_df$geometry <- NULL

def_shp_df[,-1] <- (def_shp_df[,-1]/area)*100
#def_shp_df[,-1] <- def_shp_df[,-1]/area
# def_shp_df_2 <- rowSums(def_shp_df[,-1],na.rm = T)
# time_shp_df_2 <- rowSums(time_shp_df[,-1],na.rm = T)
# plot(def_shp_df_2,time_shp_df_2)

x <- reshape2::melt(def_shp_df,id.vars = "GID_3")
x$variable <- as.character(x$variable)
y <- reshape2::melt(time_shp_df,id.vars = "GID_3")
y$variable <- as.character(y$variable)
x$var <- "Deforestation"
y$var <- "Events"
years <- 11:23
for(i in 1:length(years)){
  #i <- 1
  x$variable[which(x$variable==paste0("D_",years[[i]]))] <- paste0("20",years[[i]])  
  y$variable[which(y$variable==paste0("X",20,years[[i]]))] <- paste0("20",years[[i]])
}

xy <- rbind(x,y)

x_2 <- cbind(x,y)
x_2 <- x_2[,c(1,2,3,7)]
colnames(x_2) <- c("GID_3","year","Deforestation","events" )
#x_3 <- x_2 
x_3 <- x_2[which(x_2$events>0),]
x_2 <- x_2[which(!is.na(x_2$Deforestation)),]
# #$variable <- as.numeric(xy$variable)
ggscatter(x_2, x = "Deforestation", y = "events",facet.by = "year",
          color="black", shape = 16, size = 3, # Points color, shape and size
          add = "reg.line",  # Add regressin line
          #add = "loess",
          #add.params = list(color = "year", fill = "year"), # Customize reg. line
          conf.int = TRUE, # Add confidence interval
          cor.coef = TRUE, # Add correlation coefficient. see ?stat_cor
          cor.coeff.args = list(method = "spearman", label.x = 3, label.sep = "\n")
)

################################################################################
#diffs
def_shp_df_diffs <- data.frame(matrix(ncol = 13,nrow = nrow(def_shp_df))) 
#def_shp_df_diffs[,2] <- NULL
time_shp_df_diffs <-  data.frame(matrix(ncol = 13,nrow = nrow(time_shp_df))) 
colnames(def_shp_df_diffs) <- c("GID_3",years[-1])
colnames(time_shp_df_diffs) <- c("GID_3",years[-1])
def_shp_df_diffs$GID_3 <- def_shp_df$GID_3
time_shp_df_diffs$GID_3 <- time_shp_df$GID_3
#time_shp_df_diffs[,2] <- NULL
for(i in 1:nrow(def_shp_df)){
  def_shp_df_diffs[i,-1] <- diff(as.numeric(def_shp_df[i,-1]),differences = 1)
  time_shp_df_diffs[i,-1] <- diff(as.numeric(time_shp_df[i,-1]),differences = 1)
}

x_r <- reshape2::melt(def_shp_df_diffs,id.vars = "GID_3")
#x$variable <- as.character(x$variable)
y_r <- reshape2::melt(time_shp_df_diffs,id.vars = "GID_3")
xy_r <- cbind(x_r,y_r$value)
colnames(xy_r) <- c("GID_3","year","Deforestation","Events")
xy_r$Def <- x$value[x$variable %in% 2012:2023]

ggscatter(xy_r, x = "Def", y = "Events",facet.by = "year",
          color="black", shape = 16, size = 3, # Points color, shape and size
          add = "reg.line",  # Add regressin line
          #add = "loess",
          #add.params = list(color = "year", fill = "year"), # Customize reg. line
          conf.int = TRUE, # Add confidence interval
          cor.coef = TRUE, # Add correlation coefficient. see ?stat_cor
          cor.coeff.args = list(method = "spearman", label.x = 0.04, label.sep = "\n")
)
################################################################################
dtw_df <- data.frame(matrix(ncol = 3,nrow = nrow(def_shp_df)))
dtw_df[,1] <- def_shp_df$GID_3

for(i in 1:nrow(dtw_df)){
  
    # if(sum(is.na(def_shp_df[i,-1]))==length(def_shp_df[i,-1]) |
    #    sum(is.na(time_shp_df[i,-1]))==length(time_shp_df[i,-1]) ){
  if(sum(is.na(def_shp_df[i,-1]))>0 |
     sum(is.na(time_shp_df[i,-1]))>0){

      dtw_df[i,3] <- 0
    } else {
      dtw_df[i,3] <- 1
          }
  
  
}



x_Dist <- dtw::dtwDist(
                       mx = as.matrix(def_shp_df[as.numeric(row.names(dtw_df[which(dtw_df[,3]==1),])),-1]),
                       my = as.matrix(time_shp_df[as.numeric(row.names(dtw_df[which(dtw_df[,3]==1),])),-1])
                       )

colnames(x_Dist) <- def_shp_df[as.numeric(row.names(dtw_df[which(dtw_df[,3]==1),])),1]
row.names(x_Dist) <- def_shp_df[as.numeric(row.names(dtw_df[which(dtw_df[,3]==1),])),1]
plot(hclust(as.dist(x_Dist),method =  "ward.D2"),cex=0.4,main="",sub="")
X_N <- NbClust(data = NULL, diss = as.dist(x_Dist), distance = NULL,#"euclidean",
               min.nc = 2, max.nc = 100, method = "ward.D2",index = "silhouette")


def_sf <- st_read("D:/CIAT_DEFORESTATION/RESULTS/deforestation_time.shp")
x_def_sel <- def_sf[as.numeric(row.names(dtw_df[which(dtw_df[,3]==1),])),]
x_def_sel$cluster <- as.character(X_N$Best.partition)

time_sf <- st_read("D:/CIAT_DEFORESTATION/RESULTS/n_evts_time.shp")
x_time_sel <- time_sf[as.numeric(row.names(dtw_df[which(dtw_df[,3]==1),])),]
x_time_sel$cluster <- as.character(X_N$Best.partition)

x_selected1 <- x_def_sel[which(x_def_sel$cluster==1),-c(1,16)];x_selected1$geometry <- NULL
y_selected1 <- x_time_sel[which(x_def_sel$cluster==1),-c(1,16)];y_selected1$geometry <- NULL

x_selected2 <- x_def_sel[which(x_def_sel$cluster==2),-c(1,16)];x_selected2$geometry <- NULL
y_selected2 <- x_time_sel[which(x_def_sel$cluster==2),-c(1,16)];y_selected2$geometry <- NULL


time_sel_df <- data.frame(year = rep(2011:2023,2),
                          cluster = c(rep(1,length(2011:2023)),
                                      rep(2,length(2011:2023))),
                          Deforestation = c(colMeans(as.matrix(x_selected1),na.rm = T),
                                            colMeans(as.matrix(x_selected2),na.rm = T)),
                          Events=c(colMeans(as.matrix(y_selected1),na.rm = T),
                                   colMeans(as.matrix(y_selected2),na.rm = T))
)
time_sel_df2 <- melt(time_sel_df,id.vars = c("year","cluster"))
#time_sel_df2$cluster <- as.character(time_sel_df2$cluster)
time_sel_df2$year <- as.character(time_sel_df2$year)
time_sel_df2$cluster[which(time_sel_df2$cluster==1)] <- "Cluster 1"
time_sel_df2$cluster[which(time_sel_df2$cluster==2)] <- "Cluster 2"
time_sel_df2$variable <- as.character(time_sel_df2$variable)
time_sel_df2$variable[which(time_sel_df2$variable=="Deforestation")] <- "Tree cover loss (km2)"
time_sel_df2$variable[which(time_sel_df2$variable=="Events")] <- "Conflict events (farmers, fishers or pastoralists)"


time_sel_df3 <- time_sel_df2
time_sel_df3 <- time_sel_df3[time_sel_df3$year %in% as.character(2011:2020),]
ggline(time_sel_df3, "year", "value",
       linetype = "variable",shape = "variable",facet.by =  "cluster",point.size = 2,
       fill = "variable",color = "variable", palette = c("#00AFBB", "#E7B800"))+ 
  
#  scale_y_continuous(trans = "sqrt")+
labs_pubr()#,#color = "cluster")#)
################################################################################
ghmc <- ggplot() + 
  geom_sf(data = def_sf, col = 'grey59', lwd = 0.3) + 
  geom_sf(data = x_def_sel, aes(fill = cluster), col = 'grey59', lwd = 0.3) + 
  #scale_fill_gradientn(colors = rev(met.brewer('VanGogh3', 5))) +
  #scale_fill_gradientn(colors = rev(met.brewer('VanGogh3', 5))) +
  #scale_fill_gradientn(colors = met.brewer('VanGogh3', 5)) +
  scale_fill_manual(values = c("1"="red","2"="blue"),na.value = "white") +
  geom_sf(data = x_shp1, fill = NA, col = 'white', lwd = 0.6) +
  # geom_sf(data = expn, fill = '#C7C7C7', col = 'white', lwd = 0.3) +
  # geom_sf(data = crgn, fill = 'grey70', col = '#E0E0E0', lwd = 0.3) + 
  # geom_sf(data = mpos, fill = '#FFFFFF', col = 'grey90', lwd = 0.3) +
  # geom_sf(data = rios, fill = '#3F73A3', col = '#3F73A3') + 
  # geom_sf_text(data = crgn, aes(label = corregimie), family = 'serif', size = 2.5, col = 'grey40') +
  # geom_sf_text(data = cmns, aes(label = comuna), family = 'serif', size = 4, face = 'bold', col = 'white') +
  # geom_sf_text(data = expn, aes(label = gid), family = 'serif', size = 2.5, col = 'grey40') +
  # geom_sf_text(data = mpos, aes(label = MPIO_CNMBR), family = 'serif', size = 2.5, col = 'grey40') +
  labs(x = 'Longitude', y = 'Latitude',fill="") + 
 # ggtitle(label = captions[[i]]) + 
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

ggsave(plot = ghmc, filename = "D:/CIAT_DEFORESTATION/RESULTS/0_time_series.tiff", units = 'in', width = 7, height = 8, dpi = 300)
################################################################################
x_total <- x_def_sel;x_total$geometry <- NULL
y_total <- x_time_sel;y_total$geometry <- NULL
time_sel_df_TOTAL <- data.frame(year = rep(2011:2023,1),
                       
                          Deforestation = colMeans(as.matrix(x_total[,-c(1,15)]),na.rm = T),
                          Events=colMeans(as.matrix(y_total[,-c(1,15)]),na.rm =T)
)
time_sel_df_TOTAL <- melt(time_sel_df_TOTAL,"year")
time_sel_df_TOTAL$year <- as.character(time_sel_df_TOTAL$year)
time_sel_df_TOTAL$variable <- as.character(time_sel_df_TOTAL$variable)
time_sel_df_TOTAL$variable[which(time_sel_df_TOTAL$variable=="Deforestation")] <- "Average tree cover loss proportion (%)"
time_sel_df_TOTAL$variable[which(time_sel_df_TOTAL$variable=="Events")] <- "Average conflict events (farmers, fishers or pastoralists)"
tot <- ggline(time_sel_df_TOTAL, "year", "value",
       linetype = "variable",shape = "variable",point.size = 2,
       fill = "variable",color = "variable", palette = c("#E7B800","#00AFBB"))+ 
  
  #  scale_y_continuous(trans = "sqrt")+
  labs_pubr()#,#color = "cluster")#)
ggsave(plot = tot, filename = "D:/CIAT_DEFORESTATION/RESULTS/0_time_series_total.tiff", units = 'in', width = 9, height = 8, dpi = 300)


save.image("D:/CIAT_DEFORESTATION/RESULTS/0_time_series.RData")
#load("D:/CIAT_DEFORESTATION/RESULTS/0_time_series.RData")
################################################################################
#ghmc



#fviz_nbclust(X_N$All.index) #+ theme_minimal()

# for(i in 1:nrow(dtw_df)){
#   i <- 1
#   
#   if(sum(is.na(def_shp_df[i,-1]))==length(def_shp_df[i,-1]) | 
#      sum(is.na(time_shp_df[i,-1]))==length(time_shp_df[i,-1]) ){
#     x_dtw <- NA
#     dtw_df[i,2] <- x_dtw
#   } else {
#     x_dtw <-dtw(x=time_shp_df[i,-1],
#                 y=def_shp_df[i,-1],#keep.internals = T,
#                 distance.only=T)  
#     dtw_df[i,2] <- x_dtw$distance
#     
#   }
#   rm(x_dtw)
# };rm(i)
# ## Display thBBe warping curve, i.e. the alignment curve
# plot(x_dtw, xlab="a1 - blue", ylab="a2 - magenta", xaxp  = c(0,10,10), yaxp = c(0,10,10), type="threeway")
# 
# x_dtw$distance

#y$var <- "Events"
# x_3 <- x_2[which(x_2$events>0),]
# 
# def
# 
# ggscatter(x_3, x = "Deforestation", y = "events",#,facet.by = "year",
#           color="black",fill="year",shape = 16, size = 3, # Points color, shape and size
#           add = "reg.line",  # Add regressin line
#           #add = "loess",
#           #add.params = list(color = "year", fill = "year"), # Customize reg. line
#           conf.int = TRUE, # Add confidence interval
#           cor.coef = TRUE, # Add correlation coefficient. see ?stat_cor
#           cor.coeff.args = list(method = "spearman", label.x = 3, label.sep = "\n")
# )
# x_i <- cbind(as.numeric(def_shp_df[1045,-1]),
#              as.numeric(time_shp_df[1045,-1]))
# row.names(x_i) <- years
# x_i <- x_i[complete.cases(x_i),]
# ccf(x_i[,1],x_i[,2])
# ggplot(x_2, aes(Deforestation, events, color=year,fill =year))+
#   geom_point()
#      #geom_boxplot()+
#   #ylim(c(0,2))#+ scale_y_continuous(label=variable)
#      