#----------------
#-- Study area --
#----------------

# Author: Daniel Ottmann
# Created on: February 2021

###################################################################################################
#       Readme

# Paper title: Spawning site selection of a migratory tuna reduces jellyfish predation on early life stages
# This script is designed to create the maps used in Figure 1

###################################################################################################


########################
# Set working directory:
setwd("...")

# Load R libraries
library(tidyverse)
library(maps)
library(maptools)
library(raster)


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



#################################################################
# Load lat lon data of the sampling stations:
df <- read.delim('station_positions.txt', sep = '\t', header = T, stringsAsFactors = F)


###########################################
# Download map data from GADM:
coastline <-  getData("GADM", country = "ESP", level = 0)
morocco <-  getData("GADM",country = "MAR",level = 0)
algeria <-  getData("GADM",country = "DZA",level = 0)
france <-  getData("GADM",country = "FRA",level = 0)
portugal <-  getData("GADM",country = "PRT",level = 0)
tunisia <-  getData("GADM",country = "TUN",level = 0)
italy <-  getData("GADM",country = "ITA",level = 0)
germany <-  getData("GADM",country = "DEU",level = 0)
swizerland <-  getData("GADM",country = "CHE",level = 0)
libya <-  getData("GADM",country = "LBY",level = 0)
poland <-  getData("GADM",country = "POL",level = 0)
austria <-  getData("GADM",country = "AUT",level = 0)
belgium <-  getData("GADM",country = "BEL",level = 0)
luxembourg <-  getData("GADM",country = "LUX",level = 0)
uk <-  getData("GADM",country = "GBR",level = 0)
slovenia <-  getData("GADM",country = "svn",level = 0)
croatia <-  getData("GADM",country = "HRV",level = 0)
netherlands <-  getData("GADM",country = "NLD",level = 0)
andorra <-  getData("GADM",country = "AND",level = 0)
denmark <-  getData("GADM",country = "DNK",level = 0)
sweeden <-  getData("GADM",country = "SWE",level = 0) 
czeck <-  getData("GADM",country = "CZE",level = 0)
slovakia <-  getData("GADM",country = "SVK",level = 0)
bosnia <-  getData("GADM",country = "BIH",level = 0)
albania <-  getData("GADM",country = "ALB",level = 0)
greece <-  getData("GADM",country = "GRC",level = 0)
serbia <-  getData("GADM",country = "SRB",level = 0)
montenegro <-  getData("GADM",country = "MNE",level = 0)
hungary <-  getData("GADM",country = "HUN",level = 0)
ireland <-  getData("GADM",country = "IRL",level = 0)



##############################################################
# Plot sampling Sites:

# Set x, y limits:
xlim1 <- c(0.7, 4.65)
ylim1 <- c(37.8, 40.4)


p <- ggplot() +
  geom_point(data = df, aes(lon, lat), size = 1.5) +
  geom_polygon(data = coastline, aes(x = long, y = lat, group = group), fill = "light grey", colour = "dark grey") +
  coord_equal(xlim = xlim1, ylim = ylim1) +
  labs(x = "Longitude", y = "Latitude") +
  
  scaleBar(lon = 0.7, lat = 40.2,
           distanceLon = 50, distanceLat = 10,
           distanceLegend = 20, dist.unit = "km",
           orientation = FALSE, legend.size = 4 ) +
  
  theme_bw() +
  theme(panel.grid = element_blank())

p



##############################################################
# Plot western Mediterranean:

# Set x, y limits:
xlim2 <- c(-8, 17.5)
ylim2 <- c(35, 50)



p <- ggplot() +
  
  geom_polygon(data = coastline, aes(x = long, y = lat, group = group), fill = "light grey", colour = "dark grey", size = .2) +
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

p

#           END OF SCRIPT
#####################################