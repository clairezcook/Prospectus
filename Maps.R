library(tidyverse)
library(lubridate)
library(reshape2)
library(MBA)
library(mgcv)
library(ggplot2)
library(data.table)
library(dplyr)
library(plyr)
library(readxl)
library(gtools)
library(ggpubr)
library(tidyr)

#### First try, like the bathy one more ####
#world_coordinates <- map_data("world") 

# create world map using ggplot() function 
#ggplot() + 
  
  # geom_map() function takes world coordinates  
  # as input to plot world map 
  #geom_map( 
  #  data = world_coordinates, map = world_coordinates, 
  #  aes(long, lat, map_id = region), fill="#46c56e") +
 # scale_x_continuous(limits=c(-40, 0))+
 # scale_y_continuous(limits=c(30,50))+
 # geom_point(data=az, aes(x=Long, y=Lat), fill='white', color='black') +
 # theme_classic() +
 # theme(panel.background = element_rect(fill = "#54a5d5",
                                        colour = "#54a5d5",
                                        size = 0.5, linetype = "solid"))+
 #labs(x="Longitude (ºW)", y="Latitude (ºN)")

az <- read_excel("~/Desktop/OneDrive/Cruises/Azores/StationData.xlsx")


library(marmap)
library(mapdata)

# Get bathymetric data
azoresbathy <- getNOAA.bathy(lon1=-40, lon2=0, lat1=30, lat2=50, resolution = 4)

# Create nice color palettes
blues <- c("lightsteelblue4", "lightsteelblue3", "lightsteelblue2", "lightsteelblue1")
greys <- c(grey(0.6), grey(0.93), grey(0.99))

# Second option
plot(azoresbathy, im=TRUE, land=TRUE, bpal=list(c(min(azoresbathy),0,blues),
                                                c(0,max(azoresbathy),greys)), lwd=.05, las=1, 
     xlab="Longitude ºW", ylab="Latitude ºN")
map("worldHires", res=0, lwd=0.7, add=TRUE)

# Add -200m and -1000m isobath
plot(dat, deep=-200, shallow=-200, step=0, lwd=0.5, drawlabel=TRUE, add=TRUE)
plot(dat, deep=-1000, shallow=-1000, step=0, lwd=0.3, drawlabel=TRUE, add=TRUE)
points(az$Long, az$Lat, col = "white", pch = 16, cex = 1.2)
points(az$Long, az$Lat, col = "black", pch = 16, cex = 1)  
mtext("c)", side = 3, line = 1, outer = TRUE, at = par("usr")[2] + 0.5, cex = 1.5)
azplot <- recordPlot()


#### CCS plot ####
ccs <- read_excel("~/Desktop/OneDrive/Cruises/PUPCYCLE/ODV/surfacelysotracker.xlsx")

ccsbathy <-getNOAA.bathy(lon1=-126, lon2=-122, lat1=34, lat2=44, resolution = 1)
plot(ccsbathy, im=TRUE, land=TRUE, bpal=list(c(min(ccsbathy),0,blues),
                                                c(0,max(ccsbathy),greys)), lwd=.05, las=1, 
     xlab="Longitude ºW", ylab="Latitude ºN")
map("worldHires", res=0, lwd=0.7, add=TRUE)
plot(dat, deep=-200, shallow=-200, step=0, lwd=0.5, drawlabel=TRUE, add=TRUE)
plot(dat, deep=-1000, shallow=-1000, step=0, lwd=0.3, drawlabel=TRUE, add=TRUE)
points(ccs$Long_deg, ccs$Lat_deg, col = "white", pch = 16, cex = 1.2)
points(ccs$Long_deg, ccs$Lat_deg, col = "black", pch = 16, cex = 1)  
mtext("b)", side = 3, line = 1, outer = TRUE, at = par("usr")[2] + 0.5, cex = 1.5)
ccsplot <- recordPlot()

#### LTER PLOT ####
lter <- read.csv("~/Desktop/OneDrive/Cruises/LTER/allodv.csv")
lterbathy <-getNOAA.bathy(lon1=-69, lon2=-73, lat1=38, lat2=43, resolution = 1)
plot(lterbathy, im=TRUE, land=TRUE, bpal=list(c(min(lterbathy),0,blues),
                                             c(0,max(lterbathy),greys)), lwd=.05, las=1, 
     xlab="Longitude ºW", ylab="Latitude ºN")
map("worldHires", res=0, lwd=0.7, add=TRUE)
plot(dat, deep=-200, shallow=-200, step=0, lwd=0.5, drawlabel=TRUE, add=TRUE)
plot(dat, deep=-1000, shallow=-1000, step=0, lwd=0.3, drawlabel=TRUE, add=TRUE)
points(lter$Long, lter$Lat, col = "white", pch = 16, cex = 1.2)
points(lter$Long, lter$Lat, col = "black", pch = 16, cex = 1)  
mtext("b)", side = 3, line = 1, outer = TRUE, at = par("usr")[2] + 0.5, cex = 1.5)
lterplot <- recordPlot()

install.packages("plotrix")
library(plotrix)
plot.new()
replayPlot(lterplot)
size <- par("plt")
subplot(ccsplot, size[1], size[3], size[2], size[4], "left")

