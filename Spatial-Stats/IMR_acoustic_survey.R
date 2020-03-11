#
# Capelin Acoustic Transect Analysis
# Original author: Pat Sullivan (AllData.R)
# Modified by Janelle Morano


## Working directory: Github repository "BarentsSea"
#
#setwd(""/Users/janellemorano/Documents/Github/BarentsSea")


## Read in data
data = read.csv("AllData.csv", head = TRUE)
head(data)
dim(data) 
(iyear = unique(data$year))
#zero-group fish are immature capelin

library(dplyr)
library(ggplot2)

hist(data$capelin)
hist(data$cod)
plot(data$capelin, data$cod)
qqnorm(data$capelin)
qqnorm(data$cod)
#plot(byyear$year, byyear$capelin)

## AVERAGE species by year
#average COD by year
#tapply(cod, year, mean)
ave.cod<-tapply(data$cod, data$year, mean)

#average CAPELIN by year
ave.capelin<-tapply(data$capelin, data$year, mean)

#average POLAR COD by year
#tapply(polar_cod, year, mean)
ave.polar_cod<-tapply(data$polar_cod, data$year, mean)

#average HERRING by year
ave.herring<-tapply(data$herring, data$year, mean)

#average SAITHE by year
ave.saithe<-tapply(data$saithe, data$year, mean)

#average REDFISH by year
ave.redfish<-tapply(data$redfish, data$year, mean)

#average HADDOCK_COD by year
ave.haddock_cod<-tapply(data$haddock_cod, data$year, mean)

#average HADDOCK by year
ave.haddock<-tapply(data$haddock, data$year, mean)

#average BLUE WHITING by year
ave.blue_whiting<-tapply(data$blue_whiting, data$year, mean)

#average NORWAY POUT by year
ave.norway_pout<-tapply(data$Norway_pout, data$year, mean)

#average PLANKTON by year
#no data for plankton
#ave.plankton<-tapply(data$plankton, data$year, mean)

#average ZERO GROUP FISH by year
ave.zero_group_fish<-tapply(data$zero_group_fish, data$year, mean)


## Simple graph of all of the species
plot(iyear, ave.capelin,type = "o", lty = 1, col = "blue")
lines(iyear, ave.cod, type = "o", lty = 1, col = "darkgreen")
lines(iyear, ave.herring, type = "o", lty = 1, col = "lightblue")
lines(iyear, ave.haddock, type = "o", lty = 1, col = "green")
#lines(iyear, ave.redfish, type = "o", lty = 1, col = "red")
lines(iyear, ave.polar_cod, type = "o", lty = 1, col = "purple")
#lines(iyear, ave.saithe, type = "o", lty = 1, col = "red")
#lines(iyear, ave.haddock_cod, type = "o", lty = 1, col = "red")
#lines(iyear, ave.blue_whiting, type = "o", lty = 1, col = "red")
#lines(iyear, ave.norway_pout, type = "o", lty = 1, col = "red")
lines(iyear, ave.zero_group_fish, type = "o", lty = 1, col = "red")
legend("topleft", legend=c("capelin", "cod", "herring", "haddock", "polar cod", 'zero group fish'),
       col=c("blue", "darkgreen", "lightblue", "green", "purple", "red"), lty=1, cex=1)



## Plot with ggplot2
## Not working...
#
# data %>%
#   group_by(year, month) %>%
#   summarise(year, month)
# #Average catch of capelin and cod
# byyear <- data %>% 
#   group_by(year) %>% 
#   summarise(
#     capelin = mean(capelin, na.rm = TRUE),
#     cod = mean(cod, na.rm = TRUE),
#   )
# 
# theme_set(theme_classic())
# ggplot(byyear, aes(year, cod))
# 
# select(data, year, capelin, cod)
# group_by(data, year)
# summarise(year, delay = mean(dep_delay, na.rm = TRUE))


#ggplot(capelin, aes(year, avg)+geom_bar)

#ggplot(data=capelin, aes(x=year, y=avg) + theme_bw()
#  geom_bar()

## Plot survey locations
#
plot(lat~lon,data=data)
#
# Plot relative capelin densities
#
yy = 2013
for(yy in iyear){
  datai = data[data$year==yy,]
  plot(lat~lon,data=datai,cex=4*datai$capelin/max(datai$capelin),lwd=2)
  title(paste("Capelin Abundance",yy))
  locator(1)
  }
#

# Plot relative cod densities
#
yy = 2013
for(yy in iyear){
  datai2 = data[data$year==yy,]
  plot(lat~lon,data=datai2,cex=4*datai$cod/max(datai$cod),lwd=2)
  title(paste("Cod Abundance",yy))
  locator(1)
}
#
##############################################################
#
# Geostatistics
#
#
library(sp)
#
# Read the data into R
#
data$Xloc = data$lon
data$Yloc = data$lat
coordinates(data)=c("Xloc","Yloc")
#
# Do some simple exploratory analysis of the data
#
hist(data$capelin)
stem(data$capelin)
#
# Plot the sample locations
#
par(pty="s")
plot(Yloc~Xloc,data=data)
title("Capelin Locations")
#
#
library(RColorBrewer)
library(classInt)
#
## Examine the capelin concentrations over the area
#
# Press ESC to move through the years.
# Crosshairs allow you to select a point, but not sure why
#   (The color of the point corresponds to the
#   Concentration of cadmium, as specified in the q5Colors object)
#
yy = 2013
for(yy in iyear){
  datai = data[data$year==yy,]
  pal = brewer.pal(5,"Blues")
  q5 = classIntervals(datai$capelin, n=5, style="quantile")
  q5Colors = findColours(q5,pal)
  plot(c(min(datai$Xloc),max(datai$Xloc)),
       c(min(datai$Yloc),max(datai$Yloc)),
       xlab="Longitude",ylab="Latitude",type="n")
  plot(datai,col=q5Colors,pch=19,add=T)
  legend("bottomright",fill=attr(q5Colors,"palette"),
	legend = names(attr(q5Colors,"table")),bty="n")
  title(paste("Capelin Abundance Over Area",yy))
  locator(1)
  }
#

##Examine the cod concentrations over the area
yy = 2013
for(yy in iyear){
  datai2 = data[data$year==yy,]
  pal2 = brewer.pal(5,"Greens")
  q52 = classIntervals(datai$cod, n=5, style="quantile")
  q5Colors2 = findColours(q5,pal2)
  plot(c(min(datai2$Xloc),max(datai2$Xloc)),
       c(min(datai2$Yloc),max(datai2$Yloc)),
       xlab="Longitude",ylab="Latitude",type="n")
  plot(datai2,col=q5Colors2,pch=19,add=T)
  legend("bottomright",fill=attr(q5Colors2,"palette"),
         legend = names(attr(q5Colors2,"table")),bty="n")
  title(paste("Cod Abundance Over Area",yy))
  locator(1)
}
#


##Examine the cod and capeline concentrations next to each other over the area
#
par(mfrow=c(1,2))
plot(c(min(datai$Xloc),max(datai$Xloc)),
     c(min(datai$Yloc),max(datai$Yloc)),
     xlab="Longitude",ylab="Latitude",type="n")
plot(datai,col=q5Colors,pch=19,add=T)
title(paste("Capelin Abundance Over Area",yy))
plot(c(min(datai2$Xloc),max(datai2$Xloc)),
     c(min(datai2$Yloc),max(datai2$Yloc)),
     xlab="Longitude",ylab="Latitude",type="n")
plot(datai2,col=q5Colors2,pch=19,add=T)
title(paste("Cod Abundance Over Area",yy))

# Option:
# layout(matrix(c(1,1,2,3), 2, 2, byrow = TRUE))

yy = 2012
  datai2 = data[data$year==yy,]
  pal = brewer.pal(5,"Greens")
  q5 = classIntervals(datai$cod, n=5, style="quantile")
  q5Colors = findColours(q5,pal)
  plot(c(min(datai2$Xloc),max(datai2$Xloc)),
       c(min(datai2$Yloc),max(datai2$Yloc)),
       xlab="Longitude",ylab="Latitude",type="n")
  plot(datai2,col=q5Colors,pch=19,add=T)
  legend("bottomright",fill=attr(q5Colors,"palette"),
         legend = names(attr(q5Colors,"table")),bty="n")
  title(paste("Cod Abundance Over Area",yy))
  locator(1)
}
################################################################################
#
# Kriging Capeline
#
library(gstat)
#
# Choose Year ##########################
#
yy = 2004
datai = data.frame(data[data$year==yy,])
datai$Xloc = jitter(datai$Xloc)
datai$Yloc = jitter(datai$Yloc)
coordinates(datai) = c("Xloc","Yloc")
#
par(mfrow=c(1,2))
hist(datai$capelin,main="Capelin",col="dodgerblue")
hist(log(datai$capelin+1),main="Log(Capelin+1)",col="dodgerblue")
par(mfrow=c(1,1))
#
#
# Plot relative abundances
#
  plot(lat~lon,data=datai,cex=4*datai$capelin/max(datai$capelin),lwd=2)
  title(paste("Capelin Abundance",yy))
#
#
# Plot relative abundances using color
#
library(RColorBrewer)
library(classInt)
#
# Plot a point at a location
# The color of the point corresponds to the
# Concentration of cadmium, as specified in the q5Colors object
#
  pal = brewer.pal(5,"Blues")
  q5 = classIntervals(datai$capelin, n=5, style="quantile")
  q5Colors = findColours(q5,pal)
  plot(c(min(datai$Xloc),max(datai$Xloc)),
       c(min(datai$Yloc),max(datai$Yloc)),
       xlab="Longitude",ylab="Latitude",type="n")
  plot(datai,col=q5Colors,pch=19,add=T)
  legend("bottomright",fill=attr(q5Colors,"palette"),
	legend = names(attr(q5Colors,"table")),bty="n")
  title(paste("Capelin Abundance Over Area",yy))
#
# Calculate the empirical variogram
#
capelin.vario = variogram(log(capelin+1)~1,datai,cutoff=20)
#
# Plot the empirical variogram
#
plot(gamma~dist,capelin.vario,
  ylim=c(0,max(gamma)),type='n',
  xlab="Distance",ylab="Semivariance",
  main=paste("Capelin Variogram",yy))
points(gamma~dist,capelin.vario,cex=2*np/max(np),pch=16,col="lightblue")
#
# Fit the model first by eye
#
my.range = locator(1)$x
my.nugget = locator(1)$y
my.psill = locator(1)$y-my.nugget
#
capelin.eye = vgm(model="Sph",psill=my.psill,range=my.range,nugget=my.nugget)
plot(gamma~dist,capelin.vario,
  ylim=c(0,max(gamma)),type='n',
  xlab="Distance",ylab="Semivariance",
  main=paste("Capelin Variogram",yy))
points(gamma~dist,capelin.vario,cex=2*np/max(np),pch=16,col="lightblue")
vgmline = variogramLine(capelin.eye,max(capelin.vario$dist))
lines(gamma~dist,vgmline,lwd=2)
#
# Now use eye parameters to start fit of model
#
#
capelin.fit=fit.variogram(capelin.vario,
	vgm(model="Sph",psill=my.psill,range=my.range,nugget=my.nugget),
	fit.method=1)
#
# Look at estimates
#
capelin.fit
capelin.psill=capelin.fit$psill[2]
capelin.range=capelin.fit$range[2]
capelin.nugget=capelin.fit$psill[1]
#
# Plot data, model and parameter estimates
#
#
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
#
#
# Create a grid of points to predict over
#
library(geoR)
#
capelin.grid = expand.grid(
      Xloc=seq(min(datai$Xloc),max(datai$Xloc),length=50),
      Yloc=seq(min(datai$Yloc),max(datai$Yloc),length=50))
names(capelin.grid)=c("Xloc","Yloc")
coordinates(capelin.grid)=c("Xloc","Yloc")
capelin.grid = as(capelin.grid, "SpatialPixels")
#
# Now plot the data and overlay the prediction grid
#
plot(Yloc~Xloc,capelin.grid,cex=1.2,pch='+',col="green")
points(Yloc~Xloc,datai,pch=".")	
#
#
# Predict the value at all the points in the domain
#
date()	
capelin.ok = krige(log(capelin+1)~1, datai, capelin.grid, capelin.fit)	
date()
#
# Plot the prediction
#
plot(c(min(datai$Xloc),max(datai$Xloc)),
     c(min(datai$Yloc),max(datai$Yloc)),
     type="n",xlab="Longitude",ylab="Latitude")
image(capelin.ok["var1.pred"],col=heat.colors(4),add=T)
#contour(capelin.ok["var1.pred"],add=T)
title(paste("Predicted Log(Capelin+1)",yy))
legend("bottomright",legend=c(0,1,2,3),fill=heat.colors(4),
	bty="n",title="log(Capelin+1)")
#
# Plot variance
#
plot(c(min(datai$Xloc),max(datai$Xloc)),
     c(min(datai$Yloc),max(datai$Yloc)),
     type="n",xlab="Longitude",ylab="Latitude")
image(capelin.ok["var1.var"],col=heat.colors(4),add=T)
points(datai$Xloc,datai$Yloc,pch=".")
legend("bottomright",legend=round(seq(.3,.5,length=4),2),fill=heat.colors(4),
	bty="n",title="Variance")
# points(data.frame(capelin.grid)$Xloc,data.frame(capelin.grid)$Yloc,pch="+")
title(paste("Variance in Predictions",yy))
#
#
# Overlay Cod on Capelin Predictions
#
plot(c(min(datai$Xloc),max(datai$Xloc)),
     c(min(datai$Yloc),max(datai$Yloc)),
     type="n",xlab="Longitude",ylab="Latitude")
image(capelin.ok["var1.pred"],col=heat.colors(4),add=T)
#contour(capelin.ok["var1.pred"],add=T)
title(paste("Predicted Log(Capelin+1) \n Overlayed with Cod",yy))
legend("bottomright",legend=c(0,1,2,3),fill=heat.colors(4),
	bty="n",title="log(Capelin+1)")
datai0 = datai[datai$cod>2,]
points(Yloc~Xloc,data=datai0,cex=4*datai0$cod/max(datai0$cod),lwd=1)


