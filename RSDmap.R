#Program to work with map files & stuff
#--------------------------------------#

#This scrip will specifically make map of Bahamas & RSD & add reef sites to map
#but can be modified to run with other shapefile files
#it uses a function to add scale bar and North Arrow to a map

# housekeeping
rm(list=ls())

#required packages
#I really do not remember if you will need all of them...
library(geosphere)
library(mapdata)
library(rgeos) 
library(maptools)
library(raster)
library (sp) #
library(ggplot2) 
library(ggmap) 
library(maps) 
library(rgdal) 
library(plyr)  
library(grid)  
library(GISTools)

#Call scalebar (+arrow) functions
#To customize these go to BarArrowFunction, file must be on same folder or properly directed
source("BarArrowFunction.R")

##Call data from RSD points and transform from DM to decimal 
##this is only to add some points to the map afterwards
mydata <- read.table("CEI.txt",sep="|", "\t",header=T, as.is=T,
                     stringsAsFactors=F, comment.char="", na.strings = "NA")
attach(mydata)

#do the actual transformation
mydatab <- within(mydata, {
  latdms <- do.call(rbind, strsplit(as.character(lat1), " "))
  latdec <- as.numeric(latdms[,1]) +
    (as.numeric(latdms[,2])/60)
  longdms <- do.call(rbind, strsplit(as.character(long1), " "))
  longdec <- as.numeric(longdms[,1]) -
    (as.numeric(longdms[,2])/60)
  rm(longdms)
  rm(latdms)
})
attach(mydatab)

#Load a shapefile file####
#call shapefile of Bahamas map
#all these calls help us to analyze the type of file
bhms=readShapeSpatial("bhs-_bahama_shapefiles.shp")
proj4string(bhms) <-"+proj=moll +ellps=WGS84" #equal area projection
summary(bhms)
bbox(bhms)
proj4string(bhms)
head(bhms@data)

#Plot the whole Bahamas####
#first prepare data file
#given that the file contains different polygons, we join them in a "single" file to plot them together
bhms@data$id = rownames(bhms@data)
bhms.points = fortify(bhms, region="id")
bhms.df = join(bhms.points, bhms@data, by="id")

#To export image as png (better to use as pdf)
#png("BHS.png", units="px", width=5000, height=5000, res=300)

BHS <- ggplot() + geom_polygon(data = bhms.df, aes(x = long, y = lat, group = group)) + coord_map()+
  labs(title = "The Bahamas") + theme_bw() + # Omit default grey backdrop
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank()) + #Omit default backdrop outline and inner grey gridlines
  theme(axis.text=element_text(size=40),
        axis.title=element_text(size=40,face="bold")) +
  theme(legend.text=element_text(size=35))

#Add scale bar
#Must first run code below to produce scale bar functions
#Map is very detailed so give if a few minutes to run code
#changing "orientation" to TRUE or FALSE adds or not the "North Arrow" 
BHS + scaleBar(lon = -80, lat = 21, distanceLon = 100, distanceLat = 20, distanceLegend = 30, dist.unit = "km", orientation = TRUE)

#tell export command where to stop
#dev.off()

############
#Add box surrounding South Eleuthera!!!!!!!
############

#subset the database to select & plot just South Eleuthera
RSD.South.df<-subset(bhms, bhms$NAME1 == "South Eleuthera")
ggplot(RSD.South.df, aes(long, lat))+
  labs(title = "South Eleuthera")

#Provide the latitude and longitudes from your bbox to zoom into a specific region
bbox(RSD.South.df)

#To export image as png
#png("RSD.png", units="px", width=3100, height=3100, res=300)

RSD <-ggplot(RSD.South.df, aes(long, lat))+geom_polygon(aes(group=group)) + 
  coord_map(xlim = c(-76.39514, -76.13347),ylim = c(24.75, 24.95)) + 
  labs(title = "Rock Sound, South Eleuthera") + theme_bw() + # Omit default grey backdrop
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank())+ #Omit default backdrop outline and inner grey gridlines
  theme(axis.text=element_text(size=15),
        axis.title=element_text(size=14,face="bold"))

#add CEI reef sites needed
RSD.sites <- RSD + geom_point(data = mydatab,aes(mydatab$longdec, mydatab$latdec, color=treatment))
RSD.sites + scaleBar(lon = -76.38, lat = 24.76, distanceLon = 2, distanceLat = .6, distanceLegend = 1, dist.unit = "km", orientation = TRUE)

#tell export command where to stop
#dev.off()
