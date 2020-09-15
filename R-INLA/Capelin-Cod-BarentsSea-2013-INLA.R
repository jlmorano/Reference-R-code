# Capelin and Cod Abundance in Barents Sea in 2013

# Data are from Fall et al. 2018
# Building on Spatial Stats course project code and using INLA code from Pat Sullivan for redback salamders
# Excluded data exploration from original project code, instead jumps into INLA
# Started with Mature Cod as focus for INLA
######################################################################

##########
# Setup
##########
# Working directory
setwd("/Users/janellemorano/Git/Reference-R-scripts/R-INLA")

####################################################################################
#
# Explore using INLA-R 
# Spatial Modeling with Stochastic Partial Differential Equations
# Krainski et al. 2019 Chapman Hall
# https://haakonbakka.bitbucket.io/btopic126.html
#
# https://inla.r-inla-download.org/r-inla.org/tutorials/spde/html/spde-tutorialch1.html
#
# install.packages("INLA", repos=c(getOption("repos"), INLA="https://inla.r-inla-download.org/R/testing"), dep=TRUE)
#
# https://haakonbakka.bitbucket.io/btopic108.html # this was useful too
#
# INLA-related libraries
library(INLA)
library(fields)
library(abind)
library(lattice)

# Other Libraries
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
#217 sites

##############################
# Plot: Visualize Survey Data
##############################
# Projection: Not projected correctly

## Plot survey locations
plot(lat~lon,data=datai)

## Plot with ggplot2
fortify.datai <-fortify(as.data.frame(datai))
names(fortify.datai)
dim(fortify.datai)
flon<-fortify.datai$lon
flat<-fortify.datai$lat
ggplot(data=fortify.datai, aes(flon, flat))+
  geom_point() +
  theme_classic() +
  ggtitle("Acoustic Survey Sites in Barents Sea") +
  xlab("longitude") + 
  ylab("latitude")

#####
## Plot relative capelin, immature and mature cod (TOGETHER) densities for 2013
###############################################################################
# With ggplot2
ggplot(data=fortify.datai, aes(flon, flat))+
  geom_point(aes(size = cod.mat, color = cod.mat)) +
  scale_color_continuous(type = "viridis") 

# ggplot(data=fortify.datai, aes(flon, flat))+
#   geom_point(aes(size = cod.imm, color = cod.imm)) +
#   scale_color_continuous(type = "viridis") 
# 
# ggplot(data=fortify.datai, aes(flon, flat))+
#   geom_point(aes(size = capelinA, color = capelinA)) +
#   scale_color_continuous(type = "viridis")



################################################
#MATURE COD
################################################
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
# Use Matern model
# Original code used Spherical
cod.mat.eye = vgm(model="Mat",psill=my.psill,range=my.range,nugget=my.nugget)
plot(gamma~dist,cod.mat.vario,
     ylim=c(0,max(gamma)),type='n',
     xlab="Distance",ylab="Semivariance",
     main=paste("Mature Cod Variogram By-Eye",yy))
points(gamma~dist,cod.mat.vario,cex=2*np/max(np),pch=16,col="lightblue")
vgmline = variogramLine(cod.mat.eye,max(cod.mat.vario$dist))
lines(gamma~dist,vgmline,lwd=2)


#######################
# Mature Cod: Fit Actual Model
########################
#Matern
cod.mat.fit=fit.variogram(cod.mat.vario,
                          vgm(model="Mat",psill=my.psill,range=my.range,nugget=my.nugget),
                          fit.method=1)

# Look at Mat estimates
cod.mat.fit
cod.mat.psill=cod.mat.fit$psill[2]
cod.mat.range=cod.mat.fit$range[2]
cod.mat.nugget=cod.mat.fit$psill[1]

# Plot the data, model and parameter estimates
#####
plot(gamma~dist,cod.mat.vario,
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
# Mature Cod: INLA
########################
##############
# Find the convex hull surrounding the data
#############
x = datai$x
y = datai$y
cod.mat.chull = chull(x,y)
#
# Plot the resulting polygon
#
plot(x,y)
lines(x[cod.mat.chull],y[cod.mat.chull])
#
# Create a grid of points to predict over
#
# install.packages("geoR")
#
library(geoR)
cod.mat.grid = polygrid(
  xgrid=seq(min(x),max(x),length=100),
  ygrid=seq(min(y),max(y),length=100),
  cbind(
    x[cod.mat.chull],
    y[cod.mat.chull]))
names(cod.mat.grid)=c("x","y")
coordinates(cod.mat.grid)=c("x","y")
cod.mat.grid = as(cod.mat.grid, "SpatialPixels")
#
# Now plot the data and overlay the prediction grid
#
plot(y~x,cex=1.2,pch=20,col=2)
points(data.frame(cod.mat.grid)$x,data.frame(cod.mat.grid)$y,pch="+")	
#
# Predict the value at all the points in the domain
#
cod.mat.ok = krige(log(cod.mat+1)~1, datai, cod.mat.grid, m=cod.mat.fit)	#Matern

# Plot the prediction
#
range(cod.mat.ok$var1.pred)
image(cod.mat.ok["var1.pred"],col=rev(heat.colors(10)),axes=T)
mtext(side=1,line=2,"x")
mtext(side=2,line=2,"y")
title("Predicted Mature Cod Abundances")
legend("bottomright",legend=round(seq(0,3,length=10),2),fill=rev(heat.colors(10)),
       bty="n",title="Log(Count+1)")

### Why different???

# Plot the prediction
plot(barents)
image(cod.mat.ok["var1.pred"],col=rev(heat.colors(4)),add=T)
title(paste("Predicted Log(Mature Cod+1)",yy))
legend("bottomright",legend=c(0,1,2,3, 4, 5),fill=rev(heat.colors(6)),
       bty="n",title="log(Mature Cod+1)")
plot(barents,add=T)
summary(capelin.ok["var1.pred"])

# Plot variance
#
range(cod.mat.ok$var1.var)
image(cod.mat.ok["var1.var"],col=rev(heat.colors(4)),axes=T)
mtext(side=1,line=2,"x")
mtext(side=2,line=2,"y")
points(x,y,pch="+")
legend("topleft",legend=round(seq(.3,.5,length=4),2),fill=rev(heat.colors(4)),
       bty="n",title="Variance")
title("Variance in Predictions")

### NOW INLA

# Create mesh over which kriged values will be estimated and
# upon which finer scale interpolation can happen at 
# low computational cost
#
max.edge = 5
#boundary for convex hull so that...still not sure how the resolution translates
#max.edge, inner and out edge ratio
mesh = inla.mesh.2d(
  loc=cbind(coordinates$x,coordinates$y),
  boundary=list(inla.nonconvex.hull(
    cbind(coordinates$x,coordinates$y), 20)),
  cutoff=max.edge/5,
  max.edge=c(max.edge,max.edge*3))
plot(mesh,asp=1)
points(coordinates$x,coordinates$y, col='red')
axis(1);axis(2)
box()
#


