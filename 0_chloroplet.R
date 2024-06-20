rm(list = ls());gc()
#load libraries
require(biscale)
require(sf)
require(ggplot2)
require(cowplot)
#load data
load("D:/CIAT_DEFORESTATION/RESULTS/6_FOREST_LU_LUIEMISS.RData")

out_dir <- "D:/CIAT_DEFORESTATION/RESULTS"
#create  folder
if (!dir.exists(out_dir)) {
  dir.create(out_dir)
}
#create folder
chl_folder <- paste0(out_dir, "/", "chloroplet")
if (!dir.exists(chl_folder)) {
  dir.create(chl_folder)
}
x_shp$livestock <- rowSums(cbind(x_shp$cattle_mean,x_shp$sheep_mean,x_shp$goat_mean),na.rm = T)
################################################################################
vars <- c(#"gain_2000-2020_ha",
  "loss_m1120",
  "loss_m1123",
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
  "loss_m_p1120",
  "livestock"
)

captions <- c(#"Forest gain (ha) (2000-2020)",
  "Tree cover loss (km2) (Median: 2011-2020)",
  "Tree cover loss (km2) (Median: 2011-2023)",
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
  "Tree cover loss proportion (Median: 2011-2020)",
  "Livestock (Cattle, goats ans sheep for 2015)"
)

##################################################


x_shp1 <- sf::st_read("D:/CIAT_DEFORESTATION/DATA/NEW/KEN_ILRI/gadm41_KEN_1.shp")
x_shp1 <- sf::st_as_sf(x_shp1)


chloro_function <- function(x_shp,
                            x_shp1,
                            var_to_x,
                            var_to_y,
                            caption_x,
                            caption_y,
                            palette_i) {
  ################################################################################
  x1 <- x_shp[c(var_to_x, var_to_y)] #USED
  #x_geom <- x1 #USED
  #x1$geometry <- NULL #USED
  #x1 <- x1[complete.cases(x1), ]
  
  #subsetting shapefile with data available
  #x_geom <- x_geom[as.numeric(row.names(x1)), ] #USED
  x_geom <- x1
  #x <- x_geom[var_to_x];x$geometry <- NULL
  #y <- x_geom[var_to_y];y$geometry <- NULL
  #plot(st_geometry(x_shp1))
  #x_geom$geometry <- NULL
  colnames(x_geom) <- c("x", "y", "geometry")
  data <- biscale::bi_class(
    x_geom,
    x = "x",
    y = "y",
    style = "equal",
    dim = 4
  )
  
  
  #plot(st_geometry(x_geom))
  
  # create map
  map <- ggplot2::ggplot() +
    geom_sf(
      data = data,
      mapping = aes(fill = bi_class),
      color = "white",
      size = 0.1,
      show.legend = FALSE,
    ) +
    bi_scale_fill(pal = palette_i, dim = 3,na.value = "white") +
    labs(title = "", subtitle = "") +
    geom_sf(
      data = x_shp1,
      fill = NA,
      col = "gray60",
      lwd = 0.75
    ) +
    bi_theme()
  #map
  
  
  legend <- bi_legend(
    pal = palette_i,
    dim = 3,
    xlab = paste0("", caption_x),
    ylab = paste0("", caption_y),
    size = 6.9
  )
  
  finalPlot <- cowplot::ggdraw() +
    draw_plot(map, 0, 0, 1, 1) +
    draw_plot(legend, 0.1, .65, 0.3, 0.3,scale = 1.2)
  
  
  #finalPlot
  
  ggsave(
    plot = finalPlot,
    filename = paste0(chl_folder, "/", "CHL", var_to_x, "_", var_to_y, ".png"),
    units = 'in',
    width = 20,
    height = 10,
    dpi = 300,
    bg = "white"
  )
return("DONE!") 
  #return(finalPlot) 
} 
################################################################################
################################################################################
################################################################################
#colnames(x_shp)
# var_y <- 'n_fat'
# #x
# var_x <- 'ABG_2020'
# LISA_MULTI(var_x,var_y,out_dir,x_shp,vars,captions)


var_to_x  <- vars[4]
var_to_y  <- vars[11]
caption_x  <- captions[4]
caption_y  <- captions[11]
# 
chloro_function(x_shp,
                x_shp1,
                var_to_x,
                var_to_y,
                caption_x,
                caption_y,
                "Bluegill")
# ###############################################################################
#colnames(x_shp)
# var_y <- 'n_fat'
# #x
# var_x <- 'ABG_10_20'
# LISA_MULTI(var_x,var_y,out_dir,x_shp,vars,captions)
var_to_x  <- vars[3]
var_to_y  <- vars[11]
caption_x  <- captions[3]
caption_y  <- captions[11]
# 
chloro_function(x_shp,
                x_shp1,
                var_to_x,
                var_to_y,
                caption_x,
                caption_y,
                "BlueYl")

# ###############################################################################
#x_shp$n_fat
# var_y <- 'n_fat'
# var_x <- 'loss_m1120'
# LISA_MULTI(var_x,var_y,out_dir,x_shp,vars,captions)
var_to_x  <- vars[27]
var_to_y  <- vars[11]
caption_x  <- captions[27]
caption_y  <- captions[11]
# 
chloro_function(x_shp,
                x_shp1,
                var_to_x,
                var_to_y,
                caption_x,
                caption_y,
                "PurpleOr")

# ###############################################################################
# var_y <- 'n_fat'
# #x
# var_x <- 'cattle_mean'
# LISA_MULTI(var_x,var_y,out_dir,x_shp,vars,captions)
var_to_x  <- vars[7]
var_to_y  <- vars[11]
caption_x  <- captions[7]
caption_y  <- captions[11]
# 
chloro_function(x_shp,
                x_shp1,
                var_to_x,
                var_to_y,
                caption_x,
                caption_y,
                "Brown")

# ###############################################################################
# var_y <- 'n_fat'
# #x
# var_x <- 'lud_45'
# LISA_MULTI(var_x,var_y,out_dir,x_shp,vars,captions)
var_to_x  <- vars[10]
var_to_y  <- vars[11]
caption_x  <- captions[10]
caption_y  <- captions[11]
# 
chloro_function(x_shp,
                x_shp1,
                var_to_x,
                var_to_y,
                caption_x,
                caption_y,
                "Brown")

# ###############################################################################
# var_y <- 'n_fat'
# #x
# var_x <- 'pop2020_w'
# LISA_MULTI(var_x,var_y,out_dir,x_shp,vars,captions)

# var_to_x  <- vars[24]
# var_to_y  <- vars[11]
# caption_x  <- captions[24]
# caption_y  <- captions[11]
# # 
# chloro_function(x_shp,
#                 x_shp1,
#                 var_to_x,
#                 var_to_y,
#                 caption_x,
#                 caption_y)
# 

# ###############################################################################
# var_y <- 'n_fat'
# #x
# var_x <- 'LUC_emissions'
# LISA_MULTI(var_x,var_y,out_dir,x_shp,vars,captions)
# var_to_x  <- vars[23]
# var_to_y  <- vars[11]
# caption_x  <- captions[23]
# caption_y  <- captions[11]
# # 
# chloro_function(x_shp,
#                 x_shp1,
#                 var_to_x,
#                 var_to_y,
#                 caption_x,
#                 caption_y)

################################################################################
# var_y <- 'n_fat'
# #x
# var_x <- 'lud_45'
# LISA_MULTI(var_x,var_y,out_dir,x_shp,vars,captions)
# var_to_x  <- vars[5]
# var_to_y  <- vars[11]
# caption_x  <- captions[5]
# caption_y  <- captions[11]
# # 
# chloro_function(x_shp,
#                 x_shp1,
#                 var_to_x,
#                 var_to_y,
#                 caption_x,
#                 caption_y)
# 
################################################################################
# var_y <- 'n_fat'
# #x
# var_x <- 'sheep_mean'
# LISA_MULTI(var_x,var_y,out_dir,x_shp,vars,captions)

var_to_x  <- vars[9]
var_to_y  <- vars[11]
caption_x  <- captions[9]
caption_y  <- captions[11]
# 
chloro_function(x_shp,
                x_shp1,
                var_to_x,
                var_to_y,
                caption_x,
                caption_y,
                "Brown")

################################################################################
# var_y <- 'n_fat'
# #x
# var_x <- 'goat_mean'
# LISA_MULTI(var_x,var_y,out_dir,x_shp,vars,captions)
var_to_x  <- vars[8]
var_to_y  <- vars[11]
caption_x  <- captions[8]
caption_y  <- captions[11]
# 
chloro_function(x_shp,
                x_shp1,
                var_to_x,
                var_to_y,
                caption_x,
                caption_y,
                "Brown")

################################################################################
# var_y <- 'n_fat'
# #x
# var_x <- 'Cropland'
# LISA_MULTI(var_x,var_y,out_dir,x_shp,vars,captions)
var_to_x  <- vars[18]
var_to_y  <- vars[11]
caption_x  <- captions[18]
caption_y  <- captions[11]
# 
chloro_function(x_shp,
                x_shp1,
                var_to_x,
                var_to_y,
                caption_x,
                caption_y,
                "Bluegill")
################################################################################
# var_y <- 'n_fat'
# #x
# var_x <- 'loss_m_p1120'
# LISA_MULTI(var_x,var_y,out_dir,x_shp,vars,captions)
var_to_x  <- vars[27]
var_to_y  <- vars[11]
caption_x  <- captions[27]
caption_y  <- captions[11]
# 
chloro_function(x_shp,
                x_shp1,
                var_to_x,
                var_to_y,
                caption_x,
                caption_y,
                "PurpleOr")
################################################################################
# var_y <- 'n_fat'
# #x
# var_x <- 'Grassland'
# LISA_MULTI(var_x,var_y,out_dir,x_shp,vars,captions)
var_to_x  <- vars[17]
var_to_y  <- vars[11]
caption_x  <- captions[17]
caption_y  <- captions[11]
# 
chloro_function(x_shp,
                x_shp1,
                var_to_x,
                var_to_y,
                caption_x,
                caption_y,
                "PurpleOr")

################################################################################
# var_y <- 'n_fat'
# #x
# var_x <- 'livestock'
# LISA_MULTI(var_x,var_y,out_dir,x_shp,vars,captions)
var_to_x  <- vars[28]
var_to_y  <- vars[11]
caption_x  <- captions[28]
caption_y  <- captions[11]
# 
chloro_function(x_shp,
                x_shp1,
                var_to_x,
                var_to_y,
                caption_x,
                caption_y,
                "Brown")


################################################################################
################################################################################
################################################################################
################################################################################
#EVENTS

################################################################################
#colnames(x_shp)
# var_y <- 'n_fat'
# #x
# var_x <- 'ABG_2020'
# LISA_MULTI(var_x,var_y,out_dir,x_shp,vars,captions)


var_to_x  <- vars[4]
var_to_y  <- vars[12]
caption_x  <- captions[4]
caption_y  <- captions[12]
# 
chloro_function(x_shp,
                x_shp1,
                var_to_x,
                var_to_y,
                caption_x,
                caption_y,
                "Bluegill")
# ###############################################################################
#colnames(x_shp)
# var_y <- 'n_fat'
# #x
# var_x <- 'ABG_10_20'
# LISA_MULTI(var_x,var_y,out_dir,x_shp,vars,captions)
var_to_x  <- vars[3]
var_to_y  <- vars[12]
caption_x  <- captions[3]
caption_y  <- captions[12]
# 
chloro_function(x_shp,
                x_shp1,
                var_to_x,
                var_to_y,
                caption_x,
                caption_y,
                "BlueYl")

# ###############################################################################
#x_shp$n_fat
# var_y <- 'n_fat'
# var_x <- 'loss_m1120'
# LISA_MULTI(var_x,var_y,out_dir,x_shp,vars,captions)
var_to_x  <- vars[27]
var_to_y  <- vars[12]
caption_x  <- captions[27]
caption_y  <- captions[12]
# 
chloro_function(x_shp,
                x_shp1,
                var_to_x,
                var_to_y,
                caption_x,
                caption_y,
                "PurpleOr")

# ###############################################################################
# var_y <- 'n_fat'
# #x
# var_x <- 'cattle_mean'
# LISA_MULTI(var_x,var_y,out_dir,x_shp,vars,captions)
var_to_x  <- vars[7]
var_to_y  <- vars[12]
caption_x  <- captions[7]
caption_y  <- captions[12]
# 
chloro_function(x_shp,
                x_shp1,
                var_to_x,
                var_to_y,
                caption_x,
                caption_y,
                "Brown")

# ###############################################################################
# var_y <- 'n_fat'
# #x
# var_x <- 'lud_45'
# LISA_MULTI(var_x,var_y,out_dir,x_shp,vars,captions)
var_to_x  <- vars[10]
var_to_y  <- vars[12]
caption_x  <- captions[10]
caption_y  <- captions[12]
# 
chloro_function(x_shp,
                x_shp1,
                var_to_x,
                var_to_y,
                caption_x,
                caption_y,
                "Brown")

# ###############################################################################
# var_y <- 'n_evts'
# #x
# var_x <- 'livestock'
# LISA_MULTI(var_x,var_y,out_dir,x_shp,vars,captions)
var_to_x  <- vars[28]
var_to_y  <- vars[12]
caption_x  <- captions[28]
caption_y  <- captions[12]
# 
chloro_function(x_shp,
                x_shp1,
                var_to_x,
                var_to_y,
                caption_x,
                caption_y,
                "Brown")

# var_y <- 'n_fat'
# #x
# var_x <- 'pop2020_w'
# LISA_MULTI(var_x,var_y,out_dir,x_shp,vars,captions)

# var_to_x  <- vars[24]
# var_to_y  <- vars[11]
# caption_x  <- captions[24]
# caption_y  <- captions[11]
# # 
# chloro_function(x_shp,
#                 x_shp1,
#                 var_to_x,
#                 var_to_y,
#                 caption_x,
#                 caption_y)
# 

# ###############################################################################
# var_y <- 'n_fat'
# #x
# var_x <- 'LUC_emissions'
# LISA_MULTI(var_x,var_y,out_dir,x_shp,vars,captions)
# var_to_x  <- vars[23]
# var_to_y  <- vars[11]
# caption_x  <- captions[23]
# caption_y  <- captions[11]
# # 
# chloro_function(x_shp,
#                 x_shp1,
#                 var_to_x,
#                 var_to_y,
#                 caption_x,
#                 caption_y)

################################################################################
# var_y <- 'n_fat'
# #x
# var_x <- 'lud_45'
# LISA_MULTI(var_x,var_y,out_dir,x_shp,vars,captions)
# var_to_x  <- vars[5]
# var_to_y  <- vars[11]
# caption_x  <- captions[5]
# caption_y  <- captions[11]
# # 
# chloro_function(x_shp,
#                 x_shp1,
#                 var_to_x,
#                 var_to_y,
#                 caption_x,
#                 caption_y)
# 
################################################################################
# var_y <- 'n_fat'
# #x
# var_x <- 'sheep_mean'
# LISA_MULTI(var_x,var_y,out_dir,x_shp,vars,captions)

var_to_x  <- vars[9]
var_to_y  <- vars[12]
caption_x  <- captions[9]
caption_y  <- captions[12]
# 
chloro_function(x_shp,
                x_shp1,
                var_to_x,
                var_to_y,
                caption_x,
                caption_y,
                "Brown")

################################################################################
# var_y <- 'n_fat'
# #x
# var_x <- 'goat_mean'
# LISA_MULTI(var_x,var_y,out_dir,x_shp,vars,captions)
var_to_x  <- vars[8]
var_to_y  <- vars[12]
caption_x  <- captions[8]
caption_y  <- captions[12]
# 
chloro_function(x_shp,
                x_shp1,
                var_to_x,
                var_to_y,
                caption_x,
                caption_y,
                "Brown")

################################################################################
# var_y <- 'n_fat'
# #x
# var_x <- 'Cropland'
# LISA_MULTI(var_x,var_y,out_dir,x_shp,vars,captions)
var_to_x  <- vars[18]
var_to_y  <- vars[12]
caption_x  <- captions[18]
caption_y  <- captions[12]
# 
chloro_function(x_shp,
                x_shp1,
                var_to_x,
                var_to_y,
                caption_x,
                caption_y,
                "Bluegill")
################################################################################
# var_y <- 'n_fat'
# #x
# var_x <- 'loss_m_p1120'
# LISA_MULTI(var_x,var_y,out_dir,x_shp,vars,captions)
var_to_x  <- vars[27]
var_to_y  <- vars[12]
caption_x  <- captions[27]
caption_y  <- captions[12]
# 
chloro_function(x_shp,
                x_shp1,
                var_to_x,
                var_to_y,
                caption_x,
                caption_y,
                "PurpleOr")

################################################################################
# var_y <- 'n_evts'
# #x
# var_x <- 'Grassland'
# LISA_MULTI(var_x,var_y,out_dir,x_shp,vars,captions)
var_to_x  <- vars[17]
var_to_y  <- vars[12]
caption_x  <- captions[17]
caption_y  <- captions[12]
# 
chloro_function(x_shp,
                x_shp1,
                var_to_x,
                var_to_y,
                caption_x,
                caption_y,
                "PurpleOr")
# var_y <- 'n_evts'
# #x
# var_x <- 'LUC_emissions'
# LISA_MULTI(var_x,var_y,out_dir,x_shp,vars,captions)
# ################################################################################
# var_y <- 'n_evts'
# #x
# var_x <- 'lud_45'
# LISA_MULTI(var_x,var_y,out_dir,x_shp,vars,captions)
# ################################################################################
# var_y <- 'n_evts'
# #x
# var_x <- 'sheep_mean'
# LISA_MULTI(var_x,var_y,out_dir,x_shp,vars,captions)
# ################################################################################
# var_y <- 'n_evts'
# #x
# var_x <- 'goat_mean'
# LISA_MULTI(var_x,var_y,out_dir,x_shp,vars,captions)
# ################################################################################
# var_y <- 'n_evts'
# #x
# var_x <- 'Cropland'
# LISA_MULTI(var_x,var_y,out_dir,x_shp,vars,captions)
