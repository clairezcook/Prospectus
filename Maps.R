library(tidyverse)
library(lubridate)
library(reshape2)
library(mgcv)
library(ggplot2)
library(data.table)
library(dplyr)
library(plyr)
library(readxl)
library(gtools)
library(ggpubr)
library(tidyr)
library(sf)
library(marmap)
library(tidyverse)
library(rnaturalearth)
library(rnaturalearthdata)

# Load in data
lter <- read.csv("~/Desktop/OneDrive/Cruises/LTER/allodv.csv")
ccs <- read_excel("~/Desktop/OneDrive/Cruises/PUPCYCLE/ODV/surfacelysotracker.xlsx")
az <- read_excel("~/Desktop/OneDrive/Cruises/Azores/StationData.xlsx")


# Get bathymetric data
azoresbathy <- getNOAA.bathy(lon1=-40, lon2=0, lat1=20, lat2=60, resolution = 4)
bat_xyz <- as.xyz(azoresbathy)

# Import country data
country <- ne_countries(scale = "medium", returnclass = "sf")

# Plot using ggplot and sf
azplot <- ggplot() + 
  geom_sf(data = country) +
  geom_tile(data = bat_xyz, aes(x = V1, y = V2, fill = V3)) +
  geom_sf(data = country) +
  coord_sf(xlim = c(-40, -5), 
           ylim = c(30, 50)) +
  labs(x = "Longitude", y = "Latitude", fill = "Depth (m)") +
  theme_minimal() +
  geom_point(data=az, aes(x=Long, y=Lat), colour ="white", size=2) + 
  geom_point(data=az, aes(x=Long, y=Lat), colour ="black") + 
  theme(legend.position = 'none') +
  ggtitle("a)")
azplot

ccsbathy <-getNOAA.bathy(lon1=-127, lon2=-110, lat1=34, lat2=50, resolution = 1)

bat_xyz <- as.xyz(ccsbathy)
ccs$Long_deg <- as.numeric(ccs$Long_deg)
ccsplot <- ggplot() + 
  geom_sf(data = country) +
  geom_tile(data = bat_xyz, aes(x = V1, y = V2, fill = V3)) +
  geom_sf(data = country) +
  coord_sf(xlim = c(-127, -115), 
           ylim = c(34, 47)) +
  labs(x = "Longitude", y = "Latitude", fill = "Depth (m)") +
  theme_minimal() +
  geom_point(data=ccs, aes(x=Long_deg, y=Lat_deg), colour ="white", size=2) + 
  geom_point(data=ccs, aes(x=Long_deg, y=Lat_deg), colour ="black") + 
  theme(legend.position = 'none') +
  ggtitle("c)")+
  scale_x_continuous(breaks = seq(-127, -115, by = 4))
ccsplot

lterbathy <-getNOAA.bathy(lon1=-69, lon2=-75, lat1=38, lat2=45, resolution = 1)
bat_xyz <- as.xyz(lterbathy)
lterplot <- ggplot() + 
  geom_sf(data = country) +
  geom_tile(data = bat_xyz, aes(x = V1, y = V2, fill = V3)) +
 geom_sf(data = country) +
  coord_sf(xlim = c(-69, -73), 
           ylim = c(38, 43)) +
  labs(x = "Longitude", y = "Latitude", fill = "Depth (m)") +
  theme_minimal() +
  geom_point(data=lter, aes(x=Long, y=Lat), colour ="white", size=2) + 
  geom_point(data=lter, aes(x=Long, y=Lat), colour ="black") + 
  theme(legend.position = 'none') +
  ggtitle("b)")
lterplot

library(gridExtra)
grid.arrange(azplot, lterplot, ccsplot, nrow=1)
