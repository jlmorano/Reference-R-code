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

# For sp, coordinates must be set
# Dataset has lat/lon and Polar projection in x,y columns
# Here, sticking with x,y
# y = northing (latitude)
# x = easting (longitude)

#assign the x, y to Xloc and Yloc for reference
data$Xloc = jitter(data$x)
data$Yloc = jitter(data$y)
#Set coordinates using x,y
# define coordinates, specifically, so they can be referred to again
# Bivens specifying that these are the coordinates in the sp database
coordinates(data)=c("Xloc","Yloc")
head(data)
#plot(coordinates) #not working
#dim(coordinates) #not working

#Make a dataset for 2013 to start.
#######
yy = 2013 #For reference later in graphs and future loops
datai = data[data$year==2013,]
# For multiple years
# yy = '2013-2015' #For reference later in graphs and future loops
# datai = data[data$year>2012,]
dim(datai)
#471  42

#Previous work showed that the species distributions needed to be transformed with +1, log
# Transform mature cod
# Add 1
datai$cod.mat1 = (datai$cod.mat) + 1
summary(datai$cod.mat1)
# Log of (capelin + 1)
datai$logcod.mat1 = log(datai$cod.mat1)
head(datai)

# Transform immature cod
# Add 1
datai$cod.imm1 = (datai$cod.imm) + 1
summary(datai$cod.imm1)
# Log of (capelin + 1)
datai$logcod.imm1 = log(datai$cod.imm1)
head(datai)

##############################
# Plot: Visualize Survey Data
##############################
# Projection: Not projected correctly

## Plot survey locations
plot(y~x,data=datai)
# This works the same way
#plot(Yloc~Xloc, data=datai)

## Plot with ggplot2
fortify.datai <-fortify(as.data.frame(datai))
names(fortify.datai)
dim(fortify.datai)
flon<-fortify.datai$x
flat<-fortify.datai$y
ggplot(data=fortify.datai, aes(flon, flat))+
  geom_point() +
  theme_classic() +
  ggtitle("Acoustic Survey Sites in Barents Sea") +
  xlab("Easting") + 
  ylab("Northing")

#####
## Plot relative capelin, immature and mature cod densities for 2013
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
cod.mat.vario = variogram(log(cod.mat+1)~1,datai,cutoff=400)
#
# Plot the empirical variogram
plot(gamma~dist,cod.mat.vario,
     ylim=c(0,max(gamma)),type='n',
     xlab="Distance",ylab="Semivariance",
     main=paste("Mature Cod Variogram,", yy))
points(gamma~dist,cod.mat.vario,cex=2*np/max(np),pch=16,col="lightblue")


#######################
# Mature Cod: Fit Model By-Eye
########################
my.range = 125
my.nugget = 4.5
my.psill = 8.5-my.nugget
#
# Use Matern model
# Original code used Spherical
cod.mat.eye = vgm(model="Mat",psill=my.psill,range=my.range,nugget=my.nugget)
plot(gamma~dist,cod.mat.vario,
     ylim=c(0,max(gamma)),type='n',
     xlab="Distance",ylab="Semivariance",
     main=paste("Mature Cod Variogram By-Eye,",yy))
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
# Mature Cod: Kriging
########################
##############
# Find the convex hull surrounding the data
#############
cod.mat.chull = chull(datai$x,datai$y)
#
# Plot the resulting polygon
#
plot(y~x, datai)
y = datai$Yloc #same as "y"
x = datai$Xloc #same as "x"
lines(x[cod.mat.chull],y[cod.mat.chull])

# Create a grid of points to predict over

#library(geoR) #installed above
cod.mat.grid = polygrid(
  xgrid=seq(min(x),max(x),length=50), #when this is changed to 50 from 100, the axis is correct. Why?
  ygrid=seq(min(y),max(y),length=100),
  cbind(
    x[cod.mat.chull],
    y[cod.mat.chull]))
names(cod.mat.grid)=c("x","y")
coordinates(cod.mat.grid)=c("x","y")
cod.mat.grid = as(cod.mat.grid, "SpatialPixels")
#
# Now plot the data and overlay the prediction grid
plot(y~x,cex=1.2,pch=20,col=2)
points(data.frame(cod.mat.grid)$x,data.frame(cod.mat.grid)$y,pch="+")	
#
# Predict the value at all the points in the domain
cod.mat.ok = krige(log(cod.mat+1)~1, datai, cod.mat.grid, m=cod.mat.fit)	#Matern

# Plot the prediction
range(cod.mat.ok$var1.pred)
image(cod.mat.ok["var1.pred"],col=rev(heat.colors(6)),axes=T)
#######################################################################
#The x axis is larger and distorts the image. Why is this happening?
range(datai$x)
#[1] -330.5501  109.6791
#######################################################################
mtext(side=1,line=2,"x")
mtext(side=2,line=2,"y")
title("Predicted Mature Cod Abundances")
legend("bottomright",legend=round(seq(0,3,length=10),2),fill=rev(heat.colors(10)),
       bty="n",title="Log(Count+1)")

# Plot variance
range(cod.mat.ok$var1.var)
image(cod.mat.ok["var1.var"],col=rev(heat.colors(4)),axes=T)
mtext(side=1,line=2,"x")
mtext(side=2,line=2,"y")
points(x,y,pch="+")
legend("bottomright",legend=round(seq(.3,.5,length=5),2),fill=rev(heat.colors(4)),
       bty="n",title="Variance")
title("Variance in Predictions")

######################
### Mature Cod: INLA, without time
######################

# one year, no time component
# What's the distribution of mature cod as a function of immature cod?
# formula = mature cod ~ immature cod + (immature cod)^2 + f(field,model=spde)


# Data set-up
# t  = sal$YEAR #time; not used now
w  = datai$logcod.imm1 #value being tested against z
w2 = datai$logcod.imm1*datai$log.cod.imm1 #why squared value?
x = datai$x #easting, assigned above, but here for reference
y = datai$y #northing, assigned above, but here for reference
z  = datai$logcod.mat1 #cod.mat values
# logz  = sal$logREDBACK

# define coordinates, specifically, from the sp (Bivens) formatted dataset: datai
coordinates = datai[c("x","y")]
plot(coordinates)
dim(coordinates)

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
# Set up model object for Matern model
# spde= stochastic partial differential equation
# Using matern model
# mesh created above
# alpha means what?
spde = inla.spde2.matern(mesh=mesh, alpha=2)
#
field = mesh$idx$loc
# Amend the hypothesized model to test across the mesh
formula = z ~ w + f(field,model=spde)
#
result = inla(formula,data=data.frame(datai),family="gaussian",verbose=F)
summary(result)
#
result.field = inla.spde2.result(result, "field", spde)
names(result.field)
#
(inla.kappa = inla.emarginal(function(x) x, result.field$marginals.kappa[[1]]))
(inla.psill = inla.emarginal(function(x) x, result.field$marginals.variance.nominal[[1]]))
(inla.range = inla.emarginal(function(x) x, result.field$marginals.range.nominal[[1]]))
(inla.tau   = 1/(sqrt(4*pi)*inla.kappa*inla.psill))
(inla.nugget = 1/summary(result)$hyperpar$mean[1]) # 1/precision
#
plot(result.field$marginals.range.nominal$range.nominal.1,type='l')
range.mode = result.field$marginals.range.nominal$range.nominal.1[
  result.field$marginals.range.nominal$range.nominal.1[,"y"]==
    max(result.field$marginals.range.nominal$range.nominal.1[,"y"]),"x"]
abline(v=range.mode,col=2)
abline(v=inla.range,col=3)
#
plot(result.field$marginals.variance.nominal$variance.nominal.1,type='l')
var.mode = result.field$marginals.variance.nominal$variance.nominal.1[
  result.field$marginals.variance.nominal$variance.nominal.1[,"y"]==
    max(result.field$marginals.variance.nominal$variance.nominal.1[,"y"]),"x"]
abline(v=var.mode,col=2)
abline(v=inla.psill,col=3)
#
cod.mat.inla = vgm(model="Mat",psill=inla.psill,range=inla.range,nugget=inla.nugget)
plot(cod.mat.vario,cod.mat.eye,pch=20) #,cex=cod.mat.vario$np/50000)

# Great, it works up until here. I have a variogram fit.
# But now I need to build the plot the prediction...


#################
# MULTIPLE YEARS
#################

# Create mesh over which kriged values will be estimated and
# upon which finer scale interpolation can happen at 
# low computational cost

# Set up mature cod data following redback code
#convert board info to???
#board = sal$BOARD
#boardID  = sal$Board_ID

#ignore elevation
#w  = sal$elevation
#w2 = sal$elevation*sal$elevation

x  = jitter(sal$POINT_X-min(sal$POINT_X))/1000 # scale to KM
y  = jitter(sal$POINT_Y-min(sal$POINT_Y))/1000
z  = sal$REDBACK
logz  = sal$logREDBACK
year = sal$YEAR
#
data = data.frame(board=board,boardID=boardID,x=x,y=y,w=w,w2=w2,z=z,logz=logz,year=year)
data = data[!is.na(data$w*data$x*data$y*data$z*data$logz),]
dim(data)
#
# Unique station coordinates
#
coordinates = data[data$year==2012,c("x","y")]
plot(coordinates)
dim(coordinates)
#
n.stations = dim(coordinates)[1]
n.data     = dim(data)[1]
n.years    = n.data/n.stations
#
data$time = rep(1:n.years,each = n.stations)
#
#

# define coordinates, specifically
coordinates = datai[c("x","y")]
plot(coordinates)
dim(coordinates)

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
#######################################################################
#------> PAT  <-------------
#####Why is this a different shape now?
#######################################################################
points(coordinates$x,coordinates$y, col='red')
axis(1);axis(2)
box()
#
# Set up model object for Matern model
# spde= stochastic partial differential equation
spde = inla.spde2.matern(mesh=mesh, alpha=2)
#
# Alternatively, setup model object using priors based on earlier analysis
# ----------- Note this uses inla.spde2.pcmatern as opposed to inla.spde2.matern as above
#
# priors should be a vector of length 2 'c(prior0,tailprob)' 
# or a fixed prior specified with 'c(prior,NA)'.
#
spdePC <- inla.spde2.pcmatern(
  mesh=mesh, alpha=2, ### mesh and smoothness parameter
  prior.range=c(cod.mat.range,0.1), 
  prior.sigma=c(cod.mat.psill,0.1))
#
##--- Observation structure for estimation data
#
# Setting up data to set up in multivariate way, for multiple years
# Creating an array, stacking by time
A.est = inla.spde.make.A(
  mesh=mesh,
  loc=as.matrix(data.frame(x=data$x,y=data$y)),
  group=data$time,
  n.group=n.years)
dim(A.est)
#
A.pred = inla.spde.make.A(
  mesh=mesh, 
  group=2,             # group=1,2,3 -> years 2012,2013,2014
  n.group=n.years)
dim(A.pred)
#
#indicies allow us to step through the space and time
field.indices = inla.spde.make.index("field",n.spde = spde$n.spde, n.group=n.years)
names(field.indices)
#
#
#sets up the stack to be analyzed
stack.est = inla.stack(data=list(logz=data$logz), 
                       A=list(A.est,1), 
                       effects=
                         list(c(field.indices,
                                list(Intercept=1)),
                              list(data[,3:6])),
                       tag="est")
#
#Not sure why this part is needed
scaled.mesh.loc = list(x=(rep(scale(mesh$loc[,1],
                                    mean(data$x),
                                    sd(data$x)),
                              n.years)),
                       y=(rep(scale(mesh$loc[,2],
                                    mean(data$y),
                                    mean(data$y)),
                              n.years)))
#
stack.pred = inla.stack(data=list(logz=NA),
                        A=list(A.pred),
                        effects=
                          list(c(field.indices,
                                 scaled.mesh.loc,
                                 list(Intercept=1)
                          )),
                        tag="pred")
#
stack = inla.stack(stack.est, stack.pred)
#
# Regression
formula = (logz ~ #-1 + Intercept + w + w2 + 
             f(field, model=spde, group=field.group,control.group=list(model="ar1")))
#
formula = (logz ~ # w + w2 + 
             f(field, model=spde))#, group=field.group,control.group=list(model="ar1")))
#
date()
#field = mesh$idx$loc #corresponds to line: data=data.frame(data)[data$year==2013,],
result1 = inla(formula,
               data=inla.stack.data(stack.est,spde=spde), #comment out if running next line
               #data=data.frame(data)[data$year==2013,], 
               family="gaussian",
               control.predictor=list(A=inla.stack.A(stack.est),compute=TRUE),
               control.compute=list(cpo=FALSE),
               control.inla = list(reordering = "metis"),
               keep=FALSE, verbose=FALSE)
date()
#
print(summary(result1))
#see that the 95%CI includes zero so elevation doesn't play a roll
#
date()
resultPC = inla(formula,
                data=inla.stack.data(stack,spde=spdePC), 
                family="gaussian",
                control.predictor=list(A=inla.stack.A(stack),compute=TRUE),
                control.compute=list(cpo=FALSE),
                control.inla = list(reordering = "metis"),
                keep=FALSE, verbose=FALSE)
date()
#
print(summary(resultPC))
#
#
result.field = inla.spde2.result(result1, "field", spde)
### or ###
result.field = inla.spde2.result(resultPC, "field", spdePC)
#
#
pred_mean <- result1$summary.fitted.values[inla.stack.index(stack,"pred")$data, "mean"]
#
proj_grid = inla.mesh.projector(mesh, 
                                xlim=c(0,60), ylim=c(0,150), zlim = c(0,2), 
                                dims=c(300, 300))
grid_latent_mean = exp(inla.mesh.project(proj_grid, field_pred_mean))
#
levelplot(x=grid_latent_mean,
          row.values=proj_grid$x,
          column.values=proj_grid$y,
          #col.regions=brewer.pal(n = 8, name = "Reds"),
          col.regions=rev(heat.colors(20)),
          xlim=c(0,60), ylim=c(0,150), 
          aspect="iso",
          contour=TRUE, cuts=20, labels=FALSE, pretty=TRUE,
          xlab="Easting",ylab="Northing",
          main=as.character(paste("Salamander Count",2014)))
#
# Another try
#
trellis.par.set(regions=list(col=rev(terrain.colors(20))))
levelplot(x=grid_latent_mean,
          row.values=proj_grid$x,
          column.values=proj_grid$y,
          xlim=c(0,60), ylim=c(0,150), 
          aspect="iso",
          contour=TRUE, cuts=20, labels=FALSE, pretty=TRUE,
          xlab="Easting",ylab="Northing",
          main=as.character(paste("Salamander Count",2014)))
#
# Export Parameter Estimates
#
result.field = inla.spde2.result(result1, "field", spde)
### or ###
#result.field = inla.spde2.result(resultPC, "field", spdePC)
#
names(result.field)
#
(inla.kappa = inla.emarginal(function(x) x, result.field$marginals.kappa[[1]]))
(inla.psill = inla.emarginal(function(x) x, result.field$marginals.variance.nominal[[1]]))
(inla.range = inla.emarginal(function(x) x, result.field$marginals.range.nominal[[1]]))
(inla.tau   = 1/(sqrt(4*pi)*inla.kappa*inla.psill))
(inla.nugget = 1/summary(result)$hyperpar$mean[1]) # 1/precision
#
plot(result.field$marginals.range.nominal$range.nominal.1,type='l')
range.mode = result.field$marginals.range.nominal$range.nominal.1[
  result.field$marginals.range.nominal$range.nominal.1[,"y"]==
    max(result.field$marginals.range.nominal$range.nominal.1[,"y"]),"x"]
abline(v=range.mode,col=2)
abline(v=inla.range,col=3)
#
plot(result.field$marginals.variance.nominal$variance.nominal.1,type='l')
var.mode = result.field$marginals.variance.nominal$variance.nominal.1[
  result.field$marginals.variance.nominal$variance.nominal.1[,"y"]==
    max(result.field$marginals.variance.nominal$variance.nominal.1[,"y"]),"x"]
abline(v=var.mode,col=2)
abline(v=inla.psill,col=3)
#
# Compare to empirical variogram from gstat()
#
### in the INLA package the smoothness parameter is not estimated. 
### It is fixed when defining the spde model. 
### When alpha=2, the smoothness parameter is 1, as alpha = \nu + d/2.
### the kappa parameter in inla corresponds to 1/\phi (range parameter in gstat).
#
red.inla = vgm(model="Mat",kappa=1,psill=inla.psill,range=inla.range,nugget=inla.nugget)
plot(red.vario,red.inla,pch=20,cex=vario.red$np/50000,ylim=c(0,5))
#
red.mode = vgm(model="Mat",kappa=1,psill=var.mode,range=range.mode,nugget=0.3)
plot(red.vario,red.mode,pch=20,cex=vario.red$np/50000)
#
### comparing with maximum likelihood implemented in geoR package
###  likelihood model based approach 
#
library(geoR)
lik = likfit(data=juradata$Cd, coords=cbind(juradata$Xloc, juradata$Yloc), ini.cov.pars=c(1,.1))
lik
Cd.lik = vgm(model='Mat', kappa=1, psill=lik$sigmasq, range=lik$phi, nugget=lik$nugget)
plot(Cd.vario, Cd.lik, pch=20, cex=1.5)
#
#
mytable=data.frame(Likelihood=c(lik$phi,lik$sigmasq,lik$nugget),INLA = c(inla.range,inla.psill,inla.nugget))
row.names(mytable)=c("Range","Psill","Nugget")
mytable
#
##############################################################################
#
library(ggplot2)
#
dpm = data.frame(
  x = proj_grid$x, y = proj_grid$y, value = grid_latent_mean)
#
ggplot(dpm) + geom_tile(aes(x, y, grid_latent_mean)) +
  facet_wrap(~variable, nrow = 1) +
  coord_fixed(ratio = 1) +
  scale_fill_gradient(
    name = "log(Count+1)",
    low = "blue", high = "orange"
  ) +
  theme_bw()
