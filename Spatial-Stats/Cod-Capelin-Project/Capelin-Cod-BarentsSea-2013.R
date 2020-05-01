# Capelin and Cod Abundance in Barents Sea in 2013

# Data are from Fall et al. 2018
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
library(ggplot2)

#All of the data from Fall et al. 2018
data = read.table("Norwegian part of ecosystem survey cod-capelin 2004-2015.txt", sep = " ",header = T)
head(data)
dim(data)
#2455   42

#make each year a unique number for reference
iyear = unique(data$year)
#assign the lat and lon
data$Xloc = jitter(data$lon)
data$Yloc = jitter(data$lat)
coordinates(data)=c("Xloc","Yloc")

#Make a dataset for just 2013 to simply things.
#######
yy = 2013
datai = data[data$year==yy,]
dim(datai)
#217  42

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

# found this but function basemap not working
# library(PlotSvalbard)
# basemap("svalbard")
# basemap("panarctic", limits = 60, bathymetry = TRUE)

## Plot survey locations
plot(lat~lon,data=datai)
## Plot with ggplot2
# fortify.datai <-fortify(as.data.frame(datai))
# names(fortify.datai)
# dim(fortify.datai)
# fx<-fortify.datai$coords.x1
# fy<-fortify.datai$coords.x2
# survey = ggplot(data=fortify.datai, aes(fx, fy,colour=factor(capelinA)) + #aes(x variable, y variable)
#   geom_point()
# survey

# Read in boundary layers
library(rgdal)
setwd("/Users/janellemorano/Git/Reference-R-scripts/Spatial-Stats/Cod-Capelin-Project/Barents Sea Shp")

## Read in boundary layers
ogrInfo(dsn=".",layer="iho")
barents = readOGR(dsn=".",layer="iho")

# proj4string the slot which contains the projection information
proj4string(data)
#[1] NA
plot(barents,add=T,lwd=2)

setwd("/Users/janellemorano/Git/Reference-R-scripts/Spatial-Stats/Cod-Capelin-Project")

# Plot capelin and cod relative densities, separately
###############################################################################
## Plot relative capelin densities for 2013
# Set colors
pal = brewer.pal(5,"Blues")
q5 = classIntervals(datai$capelinA, n=5, style="quantile")
q5Colors = findColours(q5,pal)

# Plot
plot(c(min(datai$Xloc),max(datai$Xloc)),
     c(min(datai$Yloc),max(datai$Yloc)),
     xlab="Longitude",ylab="Latitude",type="n")
points(datai,col=q5Colors,pch=1,add=T, cex = 2*(datai$capelinA/max(datai$capelinA)))
legend("bottomleft",fill=attr(q5Colors,"palette"),	legend = names(attr(q5Colors,"table")),bty="n")
title(paste("Capelin Abundance",yy))

## Plot relative immature cod densities for 2013
# Set colors
pal2 = brewer.pal(5,"Oranges")
q52 = classIntervals(datai$cod.imm, n=5, style="quantile")
q5Colors2 = findColours(q52,pal2)
plot(c(min(datai$Xloc),max(datai$Xloc)),
     c(min(datai$Yloc),max(datai$Yloc)),
     xlab="Longitude",ylab="Latitude",type="n")
#plot(datai,col=q5Colors2,pch=19,add=T)
points(datai,col=q5Colors2,pch=1,add=T, cex = 2*(datai$cod.imm/max(datai$cod.imm)))
legend("bottomleft",fill=attr(q5Colors2,"palette"), legend = names(attr(q5Colors2,"table")),bty="n")
title(paste("Immature Cod Abundance",yy))

## Plot relative mature cod densities for 2013
# Set colors
pal2.2 = brewer.pal(5,"Greens")
q52.2 = classIntervals(datai$cod.mat, n=5, style="quantile")
q5Colors2.2 = findColours(q52.2,pal2.2)
plot(c(min(datai$Xloc),max(datai$Xloc)),
     c(min(datai$Yloc),max(datai$Yloc)),
     xlab="Longitude",ylab="Latitude",type="n")
#plot(datai,col=q5Colors2,pch=19,add=T)
points(datai,col=q5Colors2.2,pch=1,add=T, cex = 2*(datai$cod.mat/max(datai$cod.mat)))
legend("bottomleft",fill=attr(q5Colors2.2,"palette"), legend = names(attr(q5Colors2.2,"table")),bty="n")
title(paste("Mature Cod Abundance",yy))

#####
## Plot relative capelin, immature and mature cod (TOGETHER) densities for 2013
###############################################################################
# And makes the color intervals the same

# Capelin
capelin.pal = brewer.pal(5,"Blues")
capelin.q5 = classIntervals(datai$capelinA, n=5, style="quantile")
capelin.q5Colors = findColours(capelin.q5,capelin.pal)

# Immature Cod
cod.imm.pal = brewer.pal(5,"Oranges")
cod.imm.q5 = classIntervals(datai$capelinA, n=5, style="quantile") #relative to capelin
cod.imm.q5Colors = findColours(cod.imm.q5,cod.imm.pal)

# Mature Cod
cod.mat.pal = brewer.pal(5,"Greens")
cod.mat.q5 = classIntervals(datai$capelinA, n=5, style="quantile") #relative to capelin
cod.mat.q5Colors = findColours(cod.mat.q5,cod.mat.pal)

plot(c(min(datai$Xloc),max(datai$Xloc)),
     c(min(datai$Yloc),max(datai$Yloc)),
     xlab="Longitude",ylab="Latitude",type="n")
#add capelin
points(datai,col=capelin.q5Colors,pch=1, cex = 2*(datai$capelinA/max(datai$capelinA))) 
#add immature cod
points(datai,col=cod.imm.q5Colors,pch=1, cex = 2*(datai$cod.imm/max(datai$cod.imm)))
#add mature cod
points(datai,col=cod.mat.q5Colors,pch=1, cex = 2*(datai$cod.mat/max(datai$cod.mat)))

#add legend: this is terrible looking
legend("topright",fill=attr(capelin.q5Colors,"palette"),	legend = names(attr(capelin.q5Colors,"table")),bty="n")
legend("bottomright",fill=attr(cod.imm.q5Colors,"palette"),	legend = names(attr(cod.imm.q5Colors,"table")),bty="n")
legend("bottomleft",fill=attr(cod.mat.q5Colors,"palette"),	legend = names(attr(cod.mat.q5Colors,"table")),bty="n")
title(paste("Capelin (blue), Immature Cod (orange), Mature Cod (green)  Abundance",yy))

######################
#Try with ggplot2
######################
# abundplot <- ggplot(datai, aes(lon, lat)) + 
#   geom_point()
# 
# print(abundplot)

##########################################################################
# Summary of Species and Environmental Factor Patterns and Relationships
##########################################################################

# Summary of capelin
# Histogram of capelin
hist(datai$capelinA)
# Log of capelin
datai$logcapelinA = log(datai$capelinA)
hist(datai$logcapelinA)
summary(datai$logcapelinA)
# Add 1
datai$capelin1 = (datai$capelinA) + 1
summary(datai$capelin1)
# Log of (capelin + 1)
datai$logcapelin1 = log(datai$capelin1)
summary(datai$logcapelin1)
hist(datai$logcapelin1)


# Summary of immature cod
# Histogram of cod
hist(datai$cod.imm)
# Log of immature cod
datai$log.cod.imm = log(datai$cod.imm)
hist(datai$log.cod.imm)
summary(datai$log.cod.imm)
# Add 1
datai$cod.imm1 = (datai$cod.imm) + 1
summary(datai$cod.imm1)
# Log of (capelin + 1)
datai$logcod.imm1 = log(datai$cod.imm1)
summary(datai$logcod.imm1)
hist(datai$logcod.imm1)

# Summary of mature cod
# Histogram of cod
hist(datai$cod.mat)
# Log of mature cod
datai$log.cod.mat = log(datai$cod.mat)
hist(datai$log.cod.mat)
summary(datai$log.cod.mat)
# Add 1
datai$cod.mat1 = (datai$cod.mat) + 1
summary(datai$cod.mat1)
# Log of (capelin + 1)
datai$logcod.mat1 = log(datai$cod.mat1)
summary(datai$logcod.mat1)
hist(datai$logcod.mat1)

##########################################################################
# Data relationships
##########################################################################

library(PerformanceAnalytics)
chart.Correlation(data.frame(datai)[,c("b_depth" ,"b_temp","p_temp","capelinA", "cod.imm", "cod.mat")])

#Response: Cod.mat
#Predictor: b_depth and p_temp (since b_temp is highly correlated with p_temp)
cod.mat.lm = lm(log(cod.mat+1)~b_depth+p_temp,data=datai)
summary(cod.mat.lm)
#
#create dataframe with varying pelagic temp but keep depth the same (mean)
new1=data.frame(p_temp=seq(0,500,length=20),
                #b_temp=rep(mean(datai$p_temp),20),
                b_depth=rep(mean(datai$p_temp),20))
pred1=predict(cod.mat.lm,newdata=new1, se.fit = TRUE)
plot(log(cod.mat+1)~p_temp,data=datai)
lines(new1$p_temp,pred1$fit,lwd=2,col=2)
lines(new1$p_temp,pred1$fit + 2*pred1$se.fit,lwd=2,col=3)
lines(new1$p_temp,pred1$fit - 2*pred1$se.fit,lwd=2,col=3)
#

#Response: Cod.imm
#Predictor: b_depth, p_temp, b_temp (but b_temp is highly correlated with p_temp)
cod.imm.lm = lm(log(cod.imm+1)~b_depth+b_temp+p_temp,data=datai)
summary(cod.imm.lm)
#
#create dataframe with varying depth but keep temp the same (mean)
new2=data.frame(b_depth=seq(0,500,length=20),
                b_temp=rep(mean(datai$b_temp),20),
                p_temp=rep(mean(datai$p_temp),20))
pred2=predict(cod.imm.lm,newdata=new2, se.fit = TRUE)
plot(log(cod.imm+1)~b_depth,data=datai)
lines(new2$b_depth,pred2$fit,lwd=2,col=2)
lines(new2$b_depth,pred2$fit + 2*pred2$se.fit,lwd=2,col=3)
lines(new2$b_depth,pred2$fit - 2*pred2$se.fit,lwd=2,col=3)
#
##########################################################################
#CAPELIN
##########################################################################

#######################
# Capelin: Empirical Variogram
########################

# Calculate the empirical variogram for Capelin
capelin.vario = variogram(log(capelinA+1)~1,datai,cutoff=20)
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

my.range = 6.8
my.nugget = 0.1
my.psill = 6-my.nugget
#
capelin.eye = vgm(model="Sph",psill=my.psill,range=my.range,nugget=my.nugget)
plot(gamma~dist,capelin.vario,
     ylim=c(0,max(gamma)),type='n',
     xlab="Distance",ylab="Semivariance",
     main=paste("Capelin Variogram By-Eye",yy))
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
     main=paste("Capelin Variogram Fit",yy))
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

# Given the fitted model, let's make some predictions about the data.

# Create a grid of points to predict over
capelin.grid = expand.grid(
  Xloc=seq(min(datai$Xloc),max(datai$Xloc),length=100),
  Yloc=seq(min(datai$Yloc),max(datai$Yloc),length=100))
names(capelin.grid)=c("Xloc","Yloc")
coordinates(capelin.grid)=c("Xloc","Yloc")
capelin.grid = as(capelin.grid, "SpatialPixels")

# Now plot the data and overlay the prediction grid
plot(Yloc~Xloc,capelin.grid,cex=1.2,pch='+',col="green")
points(Yloc~Xloc,datai,pch=".")	

# Predict the value at all the points in the domain
date()	
capelin.ok = krige(log(capelinA+1)~1, datai, capelin.grid, m=capelin.fit)	
date()

# Plot the prediction
plot(barents)
image(capelin.ok["var1.pred"],col=rev(heat.colors(4)),add=T)
#contour(capelin.ok["var1.pred"],add=T)
title(paste("Predicted Log(Capelin+1)",yy))
legend("bottomright",legend=c(0,1,2,3, 4, 5),fill=rev(heat.colors(6)),
       bty="n",title="log(Capelin+1)")
plot(barents,add=T)
summary(capelin.ok["var1.pred"])

##########################################################################
#IMMATURE COD
##########################################################################
#######################
# Immature Cod: Empirical Variogram
########################

# Calculate the empirical variogram for Immature Cod
cod.imm.vario = variogram(log(cod.imm+1)~1,datai,cutoff=20)
#
# Plot the empirical variogram
plot(gamma~dist,cod.imm.vario,
     ylim=c(0,max(gamma)),type='n',
     xlab="Distance",ylab="Semivariance",
     main=paste("Immature Cod Variogram",yy))
points(gamma~dist,cod.imm.vario,cex=2*np/max(np),pch=16,col="lightblue")


#######################
# Immature Cod: Fit Model By-Eye
########################

my.range = 4
my.nugget = 4
my.psill = 8.3-my.nugget
#
cod.imm.eye = vgm(model="Sph",psill=my.psill,range=my.range,nugget=my.nugget)
plot(gamma~dist,cod.imm.vario,
     ylim=c(0,max(gamma)),type='n',
     xlab="Distance",ylab="Semivariance",
     main=paste("Immature Cod Variogram By-Eye",yy))
points(gamma~dist,cod.imm.vario,cex=2*np/max(np),pch=16,col="lightblue")
vgmline = variogramLine(cod.imm.eye,max(cod.imm.vario$dist))
lines(gamma~dist,vgmline,lwd=2)

#######################
# Immature Cod: Fit Actual Model
########################
cod.imm.fit=fit.variogram(cod.imm.vario,
                          vgm(model="Sph",psill=my.psill,range=my.range,nugget=my.nugget),
                          fit.method=1)

# Look at estimates
cod.imm.fit
cod.imm.psill=cod.imm.fit$psill[2]
cod.imm.range=cod.imm.fit$range[2]
cod.imm.nugget=cod.imm.fit$psill[1]

# Plot the data, model and parameter estimates
#####
plot(gamma~dist,cod.imm.vario,
     ylim=c(0,max(gamma)),type='n',
     xlab="Distance",ylab="Semivariance",
     main=paste("Immature Cod Variogram Fit",yy))
points(gamma~dist,cod.imm.vario,cex=2*np/max(np),pch=16,col="lightblue")
vgmline = variogramLine(cod.imm.fit,max(cod.imm.vario$dist))
lines(gamma~dist,vgmline,lwd=2)
#
legend("bottomright",legend = c(
  paste("Psill  =",round(cod.imm.psill,2)),
  paste("Range  =",round(cod.imm.range,2)),
  paste("Nugget = ",round(cod.imm.nugget,2))),
  bty="n")

#######################
# Immature Cod: Predict: Kriging
########################

# Given the fitted model, let's make some predictions about the data.

# Create a grid of points to predict over
cod.imm.grid = expand.grid(
  Xloc=seq(min(datai$Xloc),max(datai$Xloc),length=100),
  Yloc=seq(min(datai$Yloc),max(datai$Yloc),length=100))
names(cod.imm.grid)=c("Xloc","Yloc")
coordinates(cod.imm.grid)=c("Xloc","Yloc")
cod.imm.grid = as(cod.imm.grid, "SpatialPixels")

# Now plot the data and overlay the prediction grid
plot(Yloc~Xloc,cod.imm.grid,cex=1.2,pch='+',col="green")
points(Yloc~Xloc,datai,pch=".")	

# Predict the value at all the points in the domain
date()	
cod.imm.ok = krige(log(cod.imm+1)~1, datai, cod.imm.grid, m=cod.imm.fit)	
date()


# Plot the prediction
plot(barents)
image(cod.imm.ok["var1.pred"],col=rev(heat.colors(4)),add=T)
#contour(capelin.ok["var1.pred"],add=T)
title(paste("Predicted Log(Immature Cod+1)",yy))
legend("bottomright",legend=c(0,1,2,3,4,5,6,7),fill=rev(heat.colors(8)),
       bty="n",title="log(Immature Cod+1)")
plot(barents,add=T)
summary(cod.imm.ok["var1.pred"])

##########################################################################
#MATURE COD
##########################################################################
#######################
# Mature Cod: Empirical Variogram
########################

# Calculate the empirical variogram for Mature Cod
cod.mat.vario = variogram(log(cod.mat+1)~1,datai,cutoff=20)
#
# Plot the empirical variogram
plot(gamma~dist,cod.mat.vario,
     ylim=c(0,max(gamma)),type='n',
     xlab="Distance",ylab="Semivariance",
     main=paste("Mature Cod Variogram",yy))
points(gamma~dist,cod.mat.vario,cex=2*np/max(np),pch=16,col="lightblue")


#######################
# Mature Cod: Fit Model By-Eye
########################

my.range = 4
my.nugget = 4
my.psill = 8.3-my.nugget
#
cod.mat.eye = vgm(model="Sph",psill=my.psill,range=my.range,nugget=my.nugget)
plot(gamma~dist,cod.imm.vario,
     ylim=c(0,max(gamma)),type='n',
     xlab="Distance",ylab="Semivariance",
     main=paste("Mature Cod Variogram By-Eye",yy))
points(gamma~dist,cod.mat.vario,cex=2*np/max(np),pch=16,col="lightblue")
vgmline = variogramLine(cod.mat.eye,max(cod.mat.vario$dist))
lines(gamma~dist,vgmline,lwd=2)

#######################
# Mature Cod: Fit Actual Model
########################
cod.mat.fit=fit.variogram(cod.mat.vario,
                          vgm(model="Sph",psill=my.psill,range=my.range,nugget=my.nugget),
                          fit.method=1)

# Look at estimates
cod.mat.fit
cod.mat.psill=cod.mat.fit$psill[2]
cod.mat.range=cod.mat.fit$range[2]
cod.mat.nugget=cod.mat.fit$psill[1]

# Plot the data, model and parameter estimates
#####
plot(gamma~dist,cod.imm.vario,
     ylim=c(0,max(gamma)),type='n',
     xlab="Distance",ylab="Semivariance",
     main=paste("Mature Cod Variogram Fit",yy))
points(gamma~dist,cod.mat.vario,cex=2*np/max(np),pch=16,col="lightblue")
vgmline = variogramLine(cod.mat.fit,max(cod.mat.vario$dist))
lines(gamma~dist,vgmline,lwd=2)
#
legend("bottomright",legend = c(
  paste("Psill  =",round(cod.mat.psill,2)),
  paste("Range  =",round(cod.mat.range,2)),
  paste("Nugget = ",round(cod.mat.nugget,2))),
  bty="n")

#######################
# Mature Cod: Predict: Kriging
########################

# Given the fitted model, let's make some predictions about the data.

# Create a grid of points to predict over
cod.mat.grid = expand.grid(
  Xloc=seq(min(datai$Xloc),max(datai$Xloc),length=100),
  Yloc=seq(min(datai$Yloc),max(datai$Yloc),length=100))
names(cod.mat.grid)=c("Xloc","Yloc")
coordinates(cod.mat.grid)=c("Xloc","Yloc")
cod.mat.grid = as(cod.mat.grid, "SpatialPixels")

# Now plot the data and overlay the prediction grid
plot(Yloc~Xloc,cod.mat.grid,cex=1.2,pch='+',col="green")
points(Yloc~Xloc,datai,pch=".")	

# Predict the value at all the points in the domain
date()	
cod.mat.ok = krige(log(cod.mat+1)~1, datai, cod.mat.grid, m=cod.mat.fit)	
date()


# Plot the prediction
plot(barents)
image(cod.mat.ok["var1.pred"],col=rev(heat.colors(4)),add=T)
#contour(capelin.ok["var1.pred"],add=T)
title(paste("Predicted Log(Mature Cod+1)",yy))
legend("bottomright",legend=c(0,1,2,3),fill=rev(heat.colors(4)),
       bty="n",title="log(Mature Cod+1)")
plot(barents,add=T)
summary(cod.mat.ok["var1.pred"])


##########################################################################
#PLOTTING IN GGPLOT
#######

library(dplyr)
library(ggplot2)
require(maps)
require(viridis)

theme_set(
  theme_void()
)

world_map <- map_data("world")
ggplot(world_map, aes(x = long, y = lat, group = group)) +
  geom_polygon(fill="lightgray", colour = "white")


# Now, Scandinavian arctic
arctic <- c(
  "Norway", "Sweden", "Finland", "Russia", "Greenland", "Iceland"
)
# Retrievethe map data
arctic <- map_data("world", region = arctic)

# Compute the centroid as the mean longitude and lattitude
# Used as label coordinate for country's names
region.lab.data <- arctic %>%
  group_by(region) %>%
  summarise(long = mean(long), lat = mean(lat))
ggplot(arctic, aes(x = long, y = lat)) +
  coord_fixed(3, xlim = c(0, 60), ylim = c(65, 81)) +
  geom_polygon(aes(group = group, fill = region))+
  geom_text(aes(label = region), data = region.lab.data,  size = 3, hjust = 0.5) +
  #viridis is a new color palette with 32bit color
  scale_fill_viridis_d()+
  theme_void()+
  theme(legend.position = "none")


world <- map_data("world")
ggplot() +
  geom_polygon(data = world, aes(x = long, y = lat, group = group), fill = "#dddddd")
#now use coord_fixed() to make sure the axes have the same scale. You may want to pass a different value to this function if you are mapping areas close to the poles
ggplot() +
  geom_polygon(data = world, aes(x = long, y = lat, group = group), fill = "#dddddd") +
  coord_fixed(3, xlim = c(0, 60), ylim = c(65, 81))

