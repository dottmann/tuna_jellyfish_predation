#----------------
#-- Study area --
#----------------

# Author: Daniel Ottmann
# Created on: February 2021
# Last update: June 2021

###################################################################################################
#       Readme

# Paper title: Spawning site selection of a migratory tuna reduces jellyfish predation on early life stages
# This script is designed to create the maps used in Figure 1

###################################################################################################


##################################################################
# Clear environment:
rm(list = ls())


# Plot commands:
outfile_plots <- F

# Load R libraries
library(tidyverse)
library(maps)
library(maptools)
library(raster)


#################################################################
# Load lat lon data of the sampling stations:
load("data/data_station_positions.RData")


#############################################
# Create a functions to draw scale bar:
createScaleBar <- function(lon,lat,distanceLon,distanceLat,distanceLegend, dist.units = "km"){
  # First rectangle
  bottomRight <- gcDestination(lon = lon, lat = lat, bearing = 90, dist = distanceLon, dist.units = dist.units, model = "WGS84")
  
  topLeft <- gcDestination(lon = lon, lat = lat, bearing = 0, dist = distanceLat, dist.units = dist.units, model = "WGS84")
  rectangle <- cbind(lon=c(lon, lon, bottomRight[1,"long"], bottomRight[1,"long"], lon),
                     lat = c(lat, topLeft[1,"lat"], topLeft[1,"lat"],lat, lat))
  rectangle <- data.frame(rectangle, stringsAsFactors = FALSE)
  
  # Second rectangle t right of the first rectangle
  bottomRight2 <- gcDestination(lon = lon, lat = lat, bearing = 90, dist = distanceLon*2, dist.units = dist.units, model = "WGS84")
  rectangle2 <- cbind(lon = c(bottomRight[1,"long"], bottomRight[1,"long"], bottomRight2[1,"long"], bottomRight2[1,"long"], bottomRight[1,"long"]),
                      lat=c(lat, topLeft[1,"lat"], topLeft[1,"lat"], lat, lat))
  rectangle2 <- data.frame(rectangle2, stringsAsFactors = FALSE)
  
  # Now let's deal with the text
  onTop <- gcDestination(lon = lon, lat = lat, bearing = 0, dist = distanceLegend, dist.units = dist.units, model = "WGS84")
  onTop2 <- onTop3 <- onTop
  onTop2[1,"long"] <- bottomRight[1,"long"]
  onTop3[1,"long"] <- bottomRight2[1,"long"]
  
  legend <- rbind(onTop, onTop2, onTop3)
  legend <- data.frame(cbind(legend, text = c(0, distanceLon, distanceLon*2)), stringsAsFactors = FALSE, row.names = NULL)
  return(list(rectangle = rectangle, rectangle2 = rectangle2, legend = legend))
}

scaleBar <- function(lon, lat, distanceLon, distanceLat, distanceLegend, dist.unit = "km", rec.fill = "white", rec.colour = "black", rec2.fill = "black", rec2.colour = "black", legend.colour = "black", legend.size = 4, orientation = TRUE, arrow.length = 500, arrow.distance = 300, arrow.North.size = 6){
  laScaleBar <- createScaleBar(lon = lon, lat = lat, distanceLon = distanceLon, distanceLat = distanceLat, distanceLegend = distanceLegend, dist.unit = dist.unit)
  # First rectangle
  rectangle1 <- geom_polygon(data = laScaleBar$rectangle, aes(x = lon, y = lat), fill = rec.fill, colour = rec.colour)
  
  # Second rectangle
  rectangle2 <- geom_polygon(data = laScaleBar$rectangle2, aes(x = lon, y = lat), fill = rec2.fill, colour = rec2.colour)
  
  # Legend
  scaleBarLegend <- annotate("text", label = paste(laScaleBar$legend[,"text"], dist.unit, sep=""), x = laScaleBar$legend[,"long"], y = laScaleBar$legend[,"lat"], size = legend.size*0.75, colour = legend.colour)
  
  res <- list(rectangle1, rectangle2, scaleBarLegend)
  
  if(orientation){# Add an arrow pointing North
    coordsArrow <- createOrientationArrow(scaleBar = laScaleBar, length = arrow.length, distance = arrow.distance, dist.unit = dist.unit)
    arrow <- list(geom_segment(data = coordsArrow$res, aes(x = x, y = y, xend = xend, yend = yend)), annotate("text", label = "N", x = coordsArrow$coordsN[1,"x"], y = coordsArrow$coordsN[1,"y"], size = arrow.North.size, colour = "black"))
    res <- c(res, arrow)
  }
  return(res)
}


###########################################
# Download map data from GADM:
spain <-  getData(path = "data/gdm", "GADM", country = "ESP", level = 0)
morocco <-  getData(path = "data/gdm", "GADM",country = "MAR",level = 0)
algeria <-  getData(path = "data/gdm", "GADM",country = "DZA",level = 0)
france <-  getData(path = "data/gdm", "GADM",country = "FRA",level = 0)
portugal <-  getData(path = "data/gdm", "GADM",country = "PRT",level = 0)
tunisia <-  getData(path = "data/gdm", "GADM",country = "TUN",level = 0)
italy <-  getData(path = "data/gdm", "GADM",country = "ITA",level = 0)
germany <-  getData(path = "data/gdm", "GADM",country = "DEU",level = 0)
swizerland <-  getData(path = "data/gdm", "GADM",country = "CHE",level = 0)
libya <-  getData(path = "data/gdm", "GADM",country = "LBY",level = 0)
poland <-  getData(path = "data/gdm", "GADM",country = "POL",level = 0)
austria <-  getData(path = "data/gdm", "GADM",country = "AUT",level = 0)
belgium <-  getData(path = "data/gdm", "GADM",country = "BEL",level = 0)
luxembourg <-  getData(path = "data/gdm", "GADM",country = "LUX",level = 0)
uk <-  getData(path = "data/gdm", "GADM",country = "GBR",level = 0)
slovenia <-  getData(path = "data/gdm", "GADM",country = "svn",level = 0)
croatia <-  getData(path = "data/gdm", "GADM",country = "HRV",level = 0)
netherlands <-  getData(path = "data/gdm", "GADM",country = "NLD",level = 0)
andorra <-  getData(path = "data/gdm", "GADM",country = "AND",level = 0)
denmark <-  getData(path = "data/gdm", "GADM",country = "DNK",level = 0)
sweeden <-  getData(path = "data/gdm", "GADM",country = "SWE",level = 0) 
czeck <-  getData(path = "data/gdm", "GADM",country = "CZE",level = 0)
slovakia <-  getData(path = "data/gdm", "GADM",country = "SVK",level = 0)
bosnia <-  getData(path = "data/gdm", "GADM",country = "BIH",level = 0)
albania <-  getData(path = "data/gdm", "GADM",country = "ALB",level = 0)
greece <-  getData(path = "data/gdm", "GADM",country = "GRC",level = 0)
serbia <-  getData(path = "data/gdm", "GADM",country = "SRB",level = 0)
montenegro <-  getData(path = "data/gdm", "GADM",country = "MNE",level = 0)
hungary <-  getData(path = "data/gdm", "GADM",country = "HUN",level = 0)
ireland <-  getData(path = "data/gdm", "GADM",country = "IRL",level = 0)


##############################################################
# Plot sampling Sites:

# Set x, y limits:
xlim1 <- c(0.7, 4.65)
ylim1 <- c(37.8, 40.4)


p <- ggplot() +
  geom_point(data = df, aes(lon, lat), size = 1.5) +
  geom_polygon(data = spain, aes(x = long, y = lat, group = group), fill = "light grey", colour = "dark grey") +
  coord_equal(xlim = xlim1, ylim = ylim1) +
  labs(x = "Longitude", y = "Latitude") +
  
  scaleBar(lon = 0.7, lat = 40.2,
           distanceLon = 50, distanceLat = 10,
           distanceLegend = 20, dist.unit = "km",
           orientation = FALSE, legend.size = 4 ) +
  
  theme_bw() +
  theme(panel.grid = element_blank())

if (outfile_plots == T) {
  png(filename = "plots/sampling_sites.png", width = 10, height = 7, units = "cm", res = 400)
  print(p)
  dev.off()
} else {
  p
}



##############################################################
# Plot western Mediterranean:

# Set x, y limits:
xlim2 <- c(-8, 17.5)
ylim2 <- c(35, 50)

p <- ggplot() +
  
  geom_polygon(data = spain, aes(x = long, y = lat, group = group), fill = "light grey", colour = "dark grey", size = .2) +
  geom_polygon(data = morocco, aes(x = long, y = lat, group = group), fill = "light grey", colour = "dark grey", size = .2) +
  geom_polygon(data = algeria, aes(x = long, y = lat, group = group), fill = "light grey", colour = "dark grey", size = .2) +
  geom_polygon(data = france, aes(x = long, y = lat, group = group), fill = "light grey", colour = "dark grey", size = .2) +
  geom_polygon(data = tunisia, aes(x = long, y = lat, group = group), fill = "light grey", colour = "dark grey", size = .2) +
  geom_polygon(data = portugal, aes(x = long, y = lat, group = group), fill = "light grey", colour = "dark grey", size = .2) +
  geom_polygon(data = italy, aes(x = long, y = lat, group = group), fill = "light grey", colour = "dark grey", size = .2) +
  geom_polygon(data = germany, aes(x = long, y = lat, group = group), fill = "light grey", colour = "dark grey", size = .2) +
  geom_polygon(data = swizerland, aes(x = long, y = lat, group = group), fill = "light grey", colour = "dark grey", size = .2) +
  geom_polygon(data = libya, aes(x = long, y = lat, group = group), fill = "light grey", colour = "dark grey", size = .2) +
  geom_polygon(data = poland, aes(x = long, y = lat, group = group), fill = "light grey", colour = "dark grey", size = .2) +
  geom_polygon(data = austria, aes(x = long, y = lat, group = group), fill = "light grey", colour = "dark grey", size = .2) +
  geom_polygon(data = belgium, aes(x = long, y = lat, group = group), fill = "light grey", colour = "dark grey", size = .2) +
  geom_polygon(data = netherlands, aes(x = long, y = lat, group = group), fill = "light grey", colour = "dark grey", size = .2) +
  geom_polygon(data = luxembourg, aes(x = long, y = lat, group = group), fill = "light grey", colour = "dark grey", size = .2) +
  geom_polygon(data = slovenia, aes(x = long, y = lat, group = group), fill = "light grey", colour = "dark grey", size = .2) +
  geom_polygon(data = croatia, aes(x = long, y = lat, group = group), fill = "light grey", colour = "dark grey", size = .2) +
  geom_polygon(data = uk, aes(x = long, y = lat, group = group), fill = "light grey", colour = "dark grey", size = .2) +
  geom_polygon(data = andorra, aes(x = long, y = lat, group = group), fill = "light grey", colour = "dark grey", size = .2) +
  geom_polygon(data = denmark, aes(x = long, y = lat, group = group), fill = "light grey", colour = "dark grey", size = .2) +
  geom_polygon(data = sweeden, aes(x = long, y = lat, group = group), fill = "light grey", colour = "dark grey", size = .2) +
  geom_polygon(data = czeck, aes(x = long, y = lat, group = group), fill = "light grey", colour = "dark grey", size = .2) +
  geom_polygon(data = slovakia, aes(x = long, y = lat, group = group), fill = "light grey", colour = "dark grey", size = .2) +
  geom_polygon(data = bosnia, aes(x = long, y = lat, group = group), fill = "light grey", colour = "dark grey", size = .2) +
  geom_polygon(data = albania, aes(x = long, y = lat, group = group), fill = "light grey", colour = "dark grey", size = .2) +
  geom_polygon(data = greece, aes(x = long, y = lat, group = group), fill = "light grey", colour = "dark grey", size = .2) +
  geom_polygon(data = serbia, aes(x = long, y = lat, group = group), fill = "light grey", colour = "dark grey", size = .2) +
  geom_polygon(data = montenegro, aes(x = long, y = lat, group = group), fill = "light grey", colour = "dark grey", size = .2) +
  geom_polygon(data = hungary, aes(x = long, y = lat, group = group), fill = "light grey", colour = "dark grey", size = .2) +
  geom_polygon(data = ireland, aes(x = long, y = lat, group = group), fill = "light grey", colour = "dark grey", size = .2) +
  
  geom_rect(aes(xmin = .7, xmax = 4.65, ymin = 37.8, ymax = 40.4), color = "black", alpha = 0) +
  
  coord_equal(xlim = xlim2, ylim = ylim2) +
  theme_bw() +
  theme(panel.grid = element_blank(),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank())

if (outfile_plots == T) {
  png(filename = "plots/western_mediterranean.png", width = 8, height = 5, units = "cm", res = 600)
  print(p)
  dev.off()
} else {
  p
}

#           END OF SCRIPT
#####################################