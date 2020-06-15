# by Pat Sullivan
# shared with me on May 13, 2020

# Geostats on salamanders
#
setwd("/Users/janellemorano/Git/Reference-R-scripts/R-INLA")
#
sal = read.csv("salabundance_892019_lr.csv")
head(sal)
tail(sal)
names(sal)
#
####################################################################################
#
# Simple Exploratory
#
# Number of redback salamanders seen by year
#
table(sal$YEAR,sal$REDBACK)
#
# Locations of sample points
#
plot(POINT_Y~POINT_X,data=sal)
#
# Removing extreme location
#
sal = sal[sal$POINT_X>=560000,]
plot(POINT_Y~POINT_X,data=sal)
#
# Explore correlation between covariates
#
# install.packages("PerformanceAnalytics")
library("PerformanceAnalytics")
chart.Correlation(sal[,c(15,26,30,6)],histogram=TRUE, pch=19)
#
# Add log of counts of REDBACK
#
sal$logREDBACK = log(sal$REDBACK+1)
chart.Correlation(sal[,c(15,26,30,32)],histogram=TRUE, pch=19)
#
# Conduct a simple linera model to examine strength of predictor signal
#
sal.glm = glm(logREDBACK~elevation+I(elevation^2),data=sal,family='gaussian')
summary(sal.glm)
#
salP.glm = glm(REDBACK~elevation+I(elevation^2),data=sal,family='poisson')
summary(sal.glm)
#
plot(REDBACK~elevation,data=sal)
new.data = data.frame(elevation=seq(350))
lines(exp(predict(sal.glm,newdata=new.data,type="response"))~new.data$elevation,
	data=sal,lwd=2,col=2)
lines(predict(salP.glm,newdata=new.data,type="response")~new.data$elevation,
	data=sal,lwd=2,col=3)
legend("topleft",legend=c("Gaussian","Poisson"),col=c(2,3),lty=1,bty='n')
title("Expected Number of Redback Salamanders
Alternative Distributional Assumptions")
#
plot(logREDBACK~elevation,data=sal)
new.data = data.frame(elevation=seq(350))
lines(predict(sal.glm,newdata=new.data,type="response")~new.data$elevation,
	data=sal,lwd=2,col=2)
lines(predict(salP.glm,newdata=new.data,type="link")~new.data$elevation,
	data=sal,lwd=2,col=3)
legend("topleft",legend=c("Gaussian","Poisson"),col=c(2,3),lty=1,bty='n')
title("Expected Log of Number of Redback Salamanders
Alternative Distributional Assumptions")
#
#######################################################################################
#
# Simple Geostatistical Estimation -- Bivand
#
library(gstat)
library(sp)
#
t  = sal$YEAR
w  = sal$elevation
w2 = sal$elevation*sal$elevation
x  = (sal$POINT_X-min(sal$POINT_X))/1000 # scale to KM
y  = (sal$POINT_Y-min(sal$POINT_Y))/1000
z  = sal$REDBACK
logz  = sal$logREDBACK
#
data = data.frame(t=t,w=w,w2=w2,x=x,y=y,logz=logz)
# --------- remove missing values
data = data[!is.na(data$t*data$w*data$x*data$y*data$logz),]
# --------- jitter data so there is no overlap in coordinates
data$x = jitter(data$x,100)
data$y = jitter(data$y,100)
coordinates(data)=c("x","y")
#
hist(data$logz,col="dodgerblue")
# Poisson maybe -- mean should equal variance
mean(data$logz)
var(data$logz)
#
# Plot abundance by location
#
library(RColorBrewer)
library(classInt)
#
pal = brewer.pal(5,"Blues")
q5 = classIntervals(data$logz, n=5, style="quantile")
q5Colors = findColours(q5,pal)
plot(data,col=q5Colors,pch=19,axes=T)
legend("topleft",fill=attr(q5Colors,"palette"),
	legend = names(attr(q5Colors,"table")),bty="n")
title("Redback Levels Over Area")
#
# Construct a variogram to explore spatial correlation
#
sort(unique(data$t))
red.vario = variogram(logz~1,data=data,cressie=F)
plot(red.vario,pch=20,cex=vario.red$np/50000) #vario.red missing
#
# Fit first by eye
# 
my.sill   = 0.38
my.nugget = 0.32
my.psill  = my.sill - my.nugget
my.range  = 20
#
red.eye = vgm(model="Sph",psill=my.psill,range=my.range,nugget=my.nugget)
plot(red.vario,red.eye,pch=20,cex=vario.red$np/50000) #vario.red missing
#
# Now use eye parameters to start fit of model
#
red.fit=fit.variogram(red.vario,
	vgm(model="Sph",psill=my.psill,range=my.range,nugget=my.nugget),
	fit.method=1)
plot(red.vario,red.fit,pch=20,cex=vario.red$np/50000) #vario.red missing
#
# Look at estimates
#
red.fit
(red.psill  = red.fit$psill[2])
(red.range  = red.fit$range[2])
(red.nugget = red.fit$psill[1])
(red.sill = red.nugget + red.psill)
#
# Try using a Matern variogram
#
redM.fit=fit.variogram(red.vario,
	vgm(model="Mat",psill=my.psill,range=my.range,nugget=my.nugget),
	fit.method=1)
plot(red.vario,redM.fit,pch=20,cex=vario.red$np/50000)
#
redM.fit
(redM.psill  = redM.fit$psill[2])
(redM.range  = redM.fit$range[2])
(redM.nugget = redM.fit$psill[1])
(redM.sill = redM.nugget + redM.psill)
#
#
# Find the convex hull surrounding the data
#
x = data$x
y = data$y
red.chull = chull(x,y)
#
# Plot the resulting polygon
#
plot(x,y)
lines(x[red.chull],y[red.chull])
#
# Create a grid of points to predict over
#
# install.packages("geoR")
#
library(geoR)
red.grid = polygrid(
      xgrid=seq(min(x),max(x),length=100),
      ygrid=seq(min(y),max(y),length=100),
      cbind(
	x[red.chull],
	y[red.chull]))
names(red.grid)=c("x","y")
coordinates(red.grid)=c("x","y")
red.grid = as(red.grid, "SpatialPixels")
#
# Now plot the data and overlay the prediction grid
#
plot(y~x,cex=1.2,pch=20,col=2)
points(data.frame(red.grid)$x,data.frame(red.grid)$y,pch="+")	
#
#
# Predict the value at all the points in the domain
#
red.ok = krige(logz~1, data, red.grid, red.fit) # Spherical	
#
red.ok = krige(logz~1, data, red.grid, redM.fit) # Matern
#
# red.ok = krige(z~w+w2, data, red.grid, redM.fit)# Need elevations
#
# Plot the prediction
#
range(red.ok$var1.pred)
image(red.ok["var1.pred"],col=rev(heat.colors(10)),axes=T)
mtext(side=1,line=2,"x")
mtext(side=2,line=2,"y")
title("Predicted Red Salamander Concentrations")
legend("topleft",legend=round(seq(0,3,length=10),2),fill=rev(heat.colors(10)),
	bty="n",title="Log(Count+1)")
# text(x,y,round(data$logz),cex=.5)
#
# Plot variance
#
range(red.ok$var1.var)
image(red.ok["var1.var"],col=rev(heat.colors(4)),axes=T)
mtext(side=1,line=2,"x")
mtext(side=2,line=2,"y")
points(x,y,pch="+")
legend("topleft",legend=round(seq(.3,.5,length=4),2),fill=rev(heat.colors(4)),
	bty="n",title="Variance")
title("Variance in Predictions")
#
#
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
library(INLA)
library(fields)
library(abind)
library(lattice)


#Convert this to make sense for cod data
# Salamander Data
#
#
board = sal$BOARD
boardID  = sal$Board_ID
w  = sal$elevation
w2 = sal$elevation*sal$elevation
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
                       cbind(coordinates$x,coordinates$y), 200)),
          cutoff=max.edge/5,
          max.edge=c(max.edge,max.edge*3))
plot(mesh,asp=1)
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
     prior.range=c(redM.range,0.1), 
     prior.sigma=c(redM.sill,0.1))
#
##--- Observation structure for estimation data
#
#
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
