####################################################################################################
# This code is for 1) simulating a landscape/seascape of autocorrelated noise and 2) simulating
# a population that is correlated to that underlying environment
# Author: Madison B Hall 
# Shared with Janelle Morano in Jan 2021
####################################################################################################
library(raster)
library(gstat)
library(faux)
library(tidyr)
#####################################################################################################

#importing most recent GOA sampling grid
grid<-shapefile("/Users/janellemorano/Git/Reference-R-scripts/Simulations/Madison GOA sim/GOA_GRID_2019/grid2019.shp")
# 23530 rows, 9 columns

########################
# below from http://santiago.begueria.es/2010/10/generating-spatially-correlated-random-fields-with-r/
# also see https://www.r-bloggers.com/2019/07/exploring-spatial-autocorrelation-in-r/

#create imaginary 10K cell grid seascape
xy <- expand.grid(1:100, 1:100)
names(xy) <- c('x','y')
g.dummy <- gstat(formula=z~1, locations=~x+y, dummy=T, beta=1, model=vgm(psill=0.025, range=5, model='Exp'), nmax=20)
gyy <- predict(g.dummy, newdata=xy, nsim=4)
gridded(gyy) = ~x+y
spplot(obj=gyy[1])

#coarser autocorrelation pattern
g.dummy1 <- gstat(formula=z~1, locations=~x+y, dummy=T, beta=1, model=vgm(psill=0.025, range=20, model='Exp'), nmax=20)
gyy1 <- predict(g.dummy1, newdata=xy, nsim=4)
gridded(gyy1) = ~x+y
spplot(obj=gyy1[1])


# ID descibes the grid cell ID # for every cell in the GOA sampling grid
# formatted as "column - row". The separate() fxn splits the ID at "-". I am trying to use the column
# and row numbers in the same way that we used x and y for the spatial pixels above
idlist<-grid@data[["ID"]]
idlist<-as.data.frame(idlist)
idlists<-separate(idlist, idlist[1], sep = "-", into = c("x", "y"))
idlists$x<-as.numeric(idlists$x)
idlists$y<-as.numeric(idlists$y)
#create df of grid id with no NAs or fxn will fail
idnona_x<-idlists$x[!is.na(idlists$x)]
idnona_y<-idlists$y[!is.na(idlists$y)]
id_nona_df<-as.data.frame(cbind(idnona_x, idnona_y))
#rename columns to match gstat formula above
names(id_nona_df)<- c("x","y")
yy <- predict(g.dummy, newdata=id_nona_df, nsim=4)
gridded(yy) = ~x+y
spplot(obj=yy)
#SUCCESS 

#each layer of yy is a different simulated seascape
spr1<-raster(yy, layer=1, values=T)
spr2<-raster(yy, layer=2, values=T)
spr3<-raster(yy, layer=3, values=T)
spr4<-raster(yy, layer=4, values=T)


df1<-as.data.frame(yy[1])
df2<-as.data.frame(yy[2])
df3<-as.data.frame(yy[3])
df4<-as.data.frame(yy[4])


#these are the mean and stdev values for first simulated enviro
mu1<-mean(df1$sim1)
std1<-sd(df1$sim1)
# working with first sim enviro for now...
data1<-df1$sim1


# Pulling in data from Zack's abundance models, will use 
# the mean and stdev in order to simulate the imaginary pop

ZO_estimates<-read.csv("ZO_estimates.csv")
mu2013<-mean(ZO_estimates$polyspinis_2013)
sd2013<-sd(ZO_estimates$polyspinis_2013)

# the data is not normal so simulating population generates negative
# abundance numbers in some cells. This problem might be fixable by using a diff distribution fxn, e.g. lognormal
# and there is the rlnorm fxn for simulating lognormal data, but it doesn't work here bcs this fxn
# doesn't generate (pop) data correlated to the 
# simulated environmental data, so sticking with rnrom_pre and
# just replacing negative abundance cells with 0 for now.. will have 
# to deal with this later

#below code generates a vector with defined mean and stdev, correlated at value r to dataset data1
v1 <- rnorm_pre(data1, mu = mu2013, sd = sd2013, r = 0.5)
v1[v1<0]<- 0
v2 <- rnorm_pre(data1, mu = mu2013, sd = sd2013, r = 0.75)
v2[v2<0]<- 0
v3 <- rnorm_pre(data1, mu = mu2013, sd =sd2013, r = 0.90)
v3[v3<0]<- 0

#pull generated population data into df with sim enviro, label columns with corr value
df1$pop50<-v1
df1$pop75<-v2
df1$pop90<-v3

gridded(df1) = ~x+y
spplot(df1[1]) #plot simulated environment
spplot(df1[2:4]) #plot 3 simulated populations corr to that environment