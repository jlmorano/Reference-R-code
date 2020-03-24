#
# Capelin Acoustic Transect Analysis
#
# Identify working directory
#
setwd("C:\\Users\\pjs31\\Documents\\Cornell\\Institute of Marine Research\\capeline acoustics")
#
# Read in data
#
data = read.csv("AllData_new.csv")
head(data)
dim(data) 
(iyear = unique(data$year))
table(data$year)
#
setwd("C:\\Users\\pjs31\\Documents\\Cornell\\Institute of Marine Research\\capeline acoustics\\Barents Sea Shp")
# 
library(rgdal)
# 
# Read in boundary layers
#
ogrInfo(dsn=".",layer="iho")
barents = readOGR(dsn=".",layer="iho")
proj4string(NENY)
plot(barents,add=T,lwd=2)
#
setwd("C:\\Users\\pjs31\\Documents\\Cornell\\Institute of Marine Research\\capeline acoustics")
#
# Plot survey locations
#
plot(barents,lwd=2,axes=T)
points(lat~lon,data=data,col=2)

#
# Plot relative densities
#
yy = 2013
par(mfrow=c(4,3))
for(yy in iyear){
  plot(lat~lon,data=data,type="n")
  datai = data[data$year==yy,]
  points(lat~lon,data=datai,cex=4*datai$capelin/max(datai$capelin),
    lwd=2,col="blue")
  #title(paste("Capelin Abundance",yy))
  title(paste(yy))
  plot(barents,lwd=2,add=T)
  #locator(1)
  }
par(mfrow=c(1,1))
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
# Examine the capelin concentrations over the area
#
library(RColorBrewer)
library(classInt)
#
# Plot a point at a location
# The color of the point corresponds to the
# Concentration of cadmium, as specified in the q5Colors object
#
yy = 2014
  plot(barents)
  datai = data[data$year==yy,]
  pal = brewer.pal(5,"Blues")
  q5 = classIntervals(datai$capelin, n=5, style="quantile")
  q5Colors = findColours(q5,pal)
  plot(datai,col=q5Colors,pch=19,add=T)
  legend("bottomright",fill=attr(q5Colors,"palette"),
	legend = names(attr(q5Colors,"table")),bty="n")
  title(paste("Capelin Abundance Over Area",yy))
#
#
yy = 2004
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
################################################################################
#
# Kriging Capeline
#
library(gstat)
#
# Choose Year ##########################
#
yy = 2007
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
#
#
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


