####################################################################################################
# This code is for 
# 1) simulating a landscape/seascape of autocorrelated noise and 
# 2) simulating a population that is correlated to that underlying environment
# for the US east coast Northeast Shelf (NES)
# Author: Janelle L. Morano
# Based on code by Madison B. Hall 
#
# Last updated March 16, 2021
####################################################################################################
library(raster)
library(gstat)
library(faux)
library(tidyr)
#####################################################################################################
# General plan
# 1. Create a spatially autocorrelated grid of a population distribution
# 2. Crop it to fit within NES strata


#Northeast Fisheries Science Center strata
NESgrid<-shapefile("/Users/janellemorano/DATA/NEFSC strata/BTS_Strata.shp")
# 185 rows, 9 columns
plot(NESgrid)
summary(NESgrid)
getClass(NESgrid)

########################
# below from http://santiago.begueria.es/2010/10/generating-spatially-correlated-random-fields-with-r/
# also see https://www.r-bloggers.com/2019/07/exploring-spatial-autocorrelation-in-r/

#create imaginary 10K cell grid seascape
# 100x100 grid converted into data frame (xyz structure) by taking all possible combinnations of the x and y coordinates
xy <- expand.grid(1:100, 1:100)
# name the xy variables
names(xy) <- c('x','y')
# define the spatial model (simple kriging) as a gstat object
g.dummy <- gstat(formula=z~1, locations=~x+y, dummy=T, beta=1, model=vgm(psill=0.025, range=5, model='Exp'), nmax=20)
# make 4 simulations from the gstat object (g.dummy)
gyy <- predict(g.dummy, newdata=xy, nsim=1)
# 1 realization of the simulations (of 4)
gridded(gyy) = ~x+y
spplot(obj=gyy[1])



# I need to take simulated space above and do this within the boundaries of the STRATA   
# STRATA identifies each sampling strata by numerical code.

library(stars)
library(sf)
library(ggplot2)
boundary <- st_read("/Users/janellemorano/DATA/NEFSC strata/BTS_Strata.shp")
grid <- read_stars(spplot(obj=gyy[1]))
simpop <- st_crop(gyy[1], NESgrid)
#nope!


fortify.shape <- function(x){
  x@data$id <- rownames(x@data)
  x.f <- fortify(x, region = "id")
  x.join <- inner_join(x.f, x@data, by = "id")
}

NESidlist<-NESgrid@data[["STRATA"]]
NESidlist<-as.data.frame(NESidlist)
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

