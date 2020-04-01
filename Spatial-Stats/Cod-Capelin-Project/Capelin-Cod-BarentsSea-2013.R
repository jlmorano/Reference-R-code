# Capelin and Cod Abundance in Barents Sea in 2013

# Data are from acoustic surveys by IMR
# Here, using only 2013 data for Spatial Stats course and testing it all out
######################################################################

##########
# Setup
##########
# Working directory
setwd("/Users/janellemorano/Git/Reference-R-scripts/Spatial-Stats/Cod-Capelin-Project")

# Libraries
library(sp)
library(rgdal)
library(gstat)
library(geoR)
library(RColorBrewer)
library(classInt)

#All of the data from 2007-2013
data = read.csv("AllData.csv", head = TRUE)
head(data)
dim(data)
#123056     22

#make each year a unique number for reference
iyear = unique(data$year)
#assign the lat and lon
data$Xloc = data$lon
data$Yloc = data$lat
coordinates(data)=c("Xloc","Yloc")

##############################
# Plot: Visualize Survey Data
##############################
# Projection: NEEDS FIXING

# proj4string(data) <- NA_character_ # remove CRS information from lnd
# proj4string(data) <- CRS("+init=epsg:27700") # assign a new CRS
# EPSG <- make_EPSG() # create data frame of available EPSG codes
# EPSG[grepl("WGS 84$", EPSG$note), ]
# lnd84 <- spTransform(data, CRS("+init=epsg:4326")) # reproject
# saveRDS(object = lnd84, file = "data/lnd84.Rds")

# make the UTM cols spatial (X/Easting/lon, Y/Northing/lat)
# use sf package
# library(sf)
# data.SP <- st_as_sf(data, coords = c("Xloc","Yloc"), crs = 4326)
# ## Plot survey locations
# plot(lat~lon,data=data.SP)
# Projection doesn't look different

# What about this way?
# # Read in boundary layers
# library(rgdal)
# 
# ogrInfo(dsn=".",layer="iho")
# barents = readOGR(dsn=".",layer="iho")
# proj4string(NENY)
# plot(barents,add=T,lwd=2)


## Read in boundary layers
ogrInfo(dsn=".",layer="/Barents Sea Shp/iho")
barents = readOGR(dsn=".",layer="iho")
proj4string(NENY)
plot(barents,add=T,lwd=2)

## Plot survey locations
plot(lat~lon,data=data)

#Make a dataset for just 2013 to simply things.
#######
yy = 2013
datai = data[data$year==yy,]
dim(datai)
#15571    22

# Plot relative capelin densities for 2013
#####
# Set colors
pal = brewer.pal(5,"Blues")
q5 = classIntervals(datai$capelin, n=5, style="quantile")
q5Colors = findColours(q5,pal)

# Plot relative capelin densities for 2013
plot(c(min(datai$Xloc),max(datai$Xloc)),
     c(min(datai$Yloc),max(datai$Yloc)),
     xlab="Longitude",ylab="Latitude",type="n")
points(datai,col=q5Colors,pch=1,add=T, cex = 2*(datai$capelin/max(datai$capelin))) #look at q5$brks
# points(datai,col=q5Colors2,pch=1,add=T, cex = 2*(datai$cod/max(datai$cod)))
legend("bottomright",fill=attr(q5Colors,"palette"),	legend = names(attr(q5Colors,"table")),bty="n")
title(paste("Capelin Abundance",yy))

# Plot relative cod densities for 2013
######
# Set colors
pal2 = brewer.pal(5,"Oranges")
q52 = classIntervals(datai$cod, n=5, style="quantile")
q5Colors2 = findColours(q52,pal2)
plot(c(min(datai$Xloc),max(datai$Xloc)),
     c(min(datai$Yloc),max(datai$Yloc)),
     xlab="Longitude",ylab="Latitude",type="n")
#plot(datai,col=q5Colors2,pch=19,add=T)
points(datai,col=q5Colors2,pch=1,add=T, cex = 2*(datai$cod/max(datai$cod)))
legend("bottomright",fill=attr(q5Colors2,"palette"), legend = names(attr(q5Colors2,"table")),bty="n")
title(paste("Cod Abundance",yy))

#######################
# Capelin: Empirical Variogram
########################

# Calculate the empirical variogram for Capelin
capelin.vario = variogram(log(capelin+1)~1,datai,cutoff=20)
#
# Plot the empirical variogram
plot(gamma~dist,capelin.vario,
     ylim=c(0,max(gamma)),type='n',
     xlab="Distance",ylab="Semivariance",
     main=paste("Capelin Variogram",yy))
points(gamma~dist,capelin.vario,cex=2*np/max(np),pch=16,col="lightblue")


#######################
# Capelin: Fit Model By-Eye
########################

my.range = 8.8
my.nugget = 0.8
my.psill = 5.2-my.nugget
#
capelin.eye = vgm(model="Sph",psill=my.psill,range=my.range,nugget=my.nugget)
plot(gamma~dist,capelin.vario,
     ylim=c(0,max(gamma)),type='n',
     xlab="Distance",ylab="Semivariance",
     main=paste("Capelin Variogram",yy))
points(gamma~dist,capelin.vario,cex=2*np/max(np),pch=16,col="lightblue")
vgmline = variogramLine(capelin.eye,max(capelin.vario$dist))
lines(gamma~dist,vgmline,lwd=2)

#######################
# Capelin: Fit Actual Model
########################
capelin.fit=fit.variogram(capelin.vario,
                          vgm(model="Sph",psill=my.psill,range=my.range,nugget=my.nugget),
                          fit.method=1)

# Look at estimates
capelin.fit
capelin.psill=capelin.fit$psill[2]
capelin.range=capelin.fit$range[2]
capelin.nugget=capelin.fit$psill[1]

# Plot the data, model and parameter estimates
#####
plot(gamma~dist,capelin.vario,
     ylim=c(0,max(gamma)),type='n',
     xlab="Distance",ylab="Semivariance",
     main=paste("Capelin Variogram",yy))
points(gamma~dist,capelin.vario,cex=2*np/max(np),pch=16,col="lightblue")
vgmline = variogramLine(capelin.fit,max(capelin.vario$dist))
lines(gamma~dist,vgmline,lwd=2)
#
legend("bottomright",legend = c(
  paste("Psill  =",round(capelin.psill,2)),
  paste("Range  =",round(capelin.range,2)),
  paste("Nugget = ",round(capelin.nugget,2))),
  bty="n")

#######################
# Capelin: Predict: Kriging
########################
# Kriging isn't working or is taking a long time. Need to break this down.

# Let's make some predictions about the data, given the fitted model.
# Create a grid of points to predict over
capelin.grid = expand.grid(
  Xloc=seq(min(datai$Xloc),max(datai$Xloc),length=10),
  Yloc=seq(min(datai$Yloc),max(datai$Yloc),length=10))
names(capelin.grid)=c("Xloc","Yloc")
coordinates(capelin.grid)=c("Xloc","Yloc")
capelin.grid = as(capelin.grid, "SpatialPixels")

# Now plot the data and overlay the prediction grid
plot(Yloc~Xloc,capelin.grid,cex=1.2,pch='+',col="green")
points(Yloc~Xloc,datai,pch=".")	

# Predict the value at all the points in the domain
date()	
capelin.ok = krige(log(capelin+1)~1, datai, capelin.grid, capelin.fit)	
date()

# Plot the prediction
#
#plot(c(min(datai$Xloc),max(datai$Xloc)),
#     c(min(datai$Yloc),max(datai$Yloc)),
#     type="n",xlab="Longitude",ylab="Latitude")
plot(barents)
image(capelin.ok["var1.pred"],col=rev(heat.colors(4)),add=T)
#contour(capelin.ok["var1.pred"],add=T)
title(paste("Predicted Log(Capelin+1)",yy))
legend("bottomright",legend=c(0,1,2,3),fill=rev(heat.colors(4)),
       bty="n",title="log(Capelin+1)")
plot(barents,add=T)
summary(capelin.ok["var1.pred"])
