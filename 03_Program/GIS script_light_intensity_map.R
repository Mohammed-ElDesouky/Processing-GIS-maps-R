
#Scripted by: Mohammed ELDESOUKT
# Date: 16/11/2021
# Title: GIS data processing

# Setting up the workspace  -----------------------------------------------

rm(list=ls())

install.packages(c("sp", "raster", "rgdal", "shiny", "shinyjs", "tmap", "ggplot2")
library (sp)
library (rgdal)
library (raster)
library(shiny)
library(shinyjs)
library(ggplot2)
library(tmap)


# Setting directory [adjust to your working directory] -------------------------------------------------------

getwd()
wd <- setwd("GitHub\Processing-GIS-maps-R\03_Program")
dir()



# Importing raster and shape files ----------------------------------------

raster <- raster("02_raster\4.tif") #renamed the raster to "4" on the desk
  summary(raster)
    raster <- setMinMax(raster)
      plot(raster)


shape0  <- readOGR('01_shape/gadm36_IND_0.shp')
  plot(shape0, add=T)
  
shape1  <- readOGR('01_shape/gadm36_IND_1.shp')
  plot(shape1, add=T)


  
# Trimming raster ---------------------------------------------------------

#---- first to crop, then to mask

cropped <- raster::crop(raster, shape0)
  plot(cropped, axes = FALSE)

masked <- mask(x = cropped, mask = shape0)
  plot (masked)



# Calculating Zonal Statistics --------------------------------------------

#---- calculating it, then adding it back to the shape file "shape1"
  
znl_stats <- extract(masked, shape1, fun=mean, sp=T, na.rm=T)
  summary(znl_stats$X4)

#---- extracting the calculating values in a vector, to use them later for visulizing

stats <- as.data.frame(znl_stats["X4"])
  
vector_stats <- sort(as.vector(stats[['X4']]))



# Visualizing  ------------------------------------------------------------


Ind_map <- tm_shape(znl_stats)+
  tm_polygons(col="X4" , breaks=vector_stats ,palette = "cividis", n=20,
              style="cont", legend.show =F, lwd=1.75, border.col="black") +
  tm_layout(main.title = "Light Intensity in India: Averaged on the State's Level (2014)",
            main.title.fontface = "bold", legend.title.fontfamily = "serif",
            main.title.position = "center", scale=1.2, frame = F)+
  tm_add_legend(type="fill", labels= c("Highest", "Lowest"), col= c("yellow", "darkblue")) +
  tm_text("NAME_1", size = .7, col = "black", fontfamily = "serif", remove.overlap = T)+
  tm_credits("Source: EOG Night Light Intensity - 2014", fontfamily= "serif",
             fontface = "italic", position = c("left", "bottom"))



tmap_save(Ind_map, filename = "India_light_intensity_eldesouky.jpeg",  height=8.5, width=11, units="in", dpi=300)



# End script --------------------------------------------------------------
#-----------------------------------------------
#---------------------------------
#--------------
#------
#----
#--
#-
#
