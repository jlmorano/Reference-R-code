####################################################
# Script to fit zero-altered negative binomial GAMM
#   with a spatial field
# Example code from Rob LaTour
# Embellished and edited by J. Morano
###########################

sessionInfo()
# R version 3.6.3 (2020-02-29)
# Platform: x86_64-apple-darwin15.6.0 (64-bit)
# Running under: macOS Mojave 10.14.6

# Some libraries that may be needed

library('mgcv')
library('car')
#install.packages('INLA',repos = c(getOption('repos'),INLA  ='https://inla.r-inla-download.org/R/stable'), dep = TRUE)
library('INLA')
library('autoimage')
library('gstat')

#library('devtools')
#install_github('timcdlucas/INLAutils')
library('INLAutils')

library('ggplot2')
library('fields')
library('rgdal')


#library('remotes')
#remotes::install_github('inbo/inlatools')
library('inlatools')

#if(!require(ggregplot)) devtools::install_github('gfalbery/ggregplot')
library('ggregplot')


############
# Functions
############

#convert lat/long to UTMs
LLtoUTM = function(df){
  df2 = df
  utms = project(as.matrix(df[,c('X','Y')]), '+proj=utm +zone=18 ellps=WGS84') #zone 18 is approx CT-NC
  df2$X = utms[,1]
  df2$Y = utms[,2]
  df2$Xkm = df2$X/1000
  df2$Ykm = df2$Y/1000
  assign(deparse(substitute(df)), df2, envir = parent.frame())
} 


#Visualize the mesh and all of it's triangles:
plotmesh = function(mesh, locs){
  par(mfrow = c(1,1), mar = c(1,1,1,1))
  plot(mesh, asp = 1)
  points(locs, col = 'red', pch = 16, cex = 0.7)
}


#a function from Zuur's book that is used to visualize the spatial field 
PlotField <- function(field, mesh, ContourMap, xlim, ylim, Add = FALSE,...){
  stopifnot(length(field) == mesh$n)
  # Plotting region to be the same as the study area polygon
  if (missing(xlim)) xlim <- ContourMap@bbox[1, ] 
  if (missing(ylim)) ylim <- ContourMap@bbox[2, ]
  
  # inla.mesh.projector: it creates a lattice using the mesh and specified ranges. 
  proj <- inla.mesh.projector(mesh, 
                              xlim = xlim, 
                              ylim = ylim, 
                              dims = c(300, 300))
  # The function inla.mesh.project can then 
  # be used to project the w's on this grid.
  field.proj <- inla.mesh.project(proj, field)
  
  # And plot the whole thing
  image.plot(list(x = proj$x, 
                  y = proj$y,
                  z = field.proj), 
             xlim = xlim, 
             ylim = ylim,
             asp = 1,
             add = Add,
             ...)  
  title(xlab = 'Easting', line = 2.5, cex.lab = 1.1)
  title(ylab = 'Northing', line = 2.5, cex.lab = 1.1)
}


#######
# Data
#######

#assume survey data stored in data frame dat
#relate counts to covariates variables temp, depth

#will need presence/absence and positive counts in separate columns in data frame
# dat$PA = ifelse(dat$count > 0, 1, 0)
# dat$Pos = ifelse(dat$count > 0, dat$count, NA)

# NAs cannot be present in any columns being used in analysis, or you'll get errors.

###############################################
# Using test data: survdat_3_2020.RData from Kevin Friedland
load("/Users/janellemorano/DATA/Survdat_3_2020.RData")
# Subsetting out menhaden; 36=menhaden
menhaden <- subset(survdat, SVSPP %in% c(36))
# add species name for reference
menhaden$species <- c("menhaden")

# Assess which envt'l variables to look at
library("PerformanceAnalytics")
colnames(menhaden)
chart.Correlation(menhaden[,c("DEPTH","SURFTEMP","SURFSALIN","BOTTEMP","BOTSALIN","BIOMASS", "ABUNDANCE")],histogram=TRUE, pch=19)
# BOTTEMP, SURFTEMP are highly correlated with Biomass
# For purposes here, going to choose BOTTEMP and DEPTH, both to follow the original script, but also because I would expect BOTTEMP to be significant and DEPTH to not be significant, so I can see how these interact in the model.

# Make menhaden a dataframe "dat" with BOTTEMP and DEPTH for ease of testing code
library(dplyr)
dat <- select(menhaden, "species", "STATION", "STRATUM", "YEAR", "SEASON", "LAT", "LON", "EST_TOWDATE", "DEPTH", "BOTTEMP", "ABUNDANCE", "BIOMASS")

# change column names to fit Rob's code below
# rename abundance to count (Pos)
names(dat)[names(dat) == 'ABUNDANCE'] <- 'Pos'
# add Presence/Absence
dat$PA <- ifelse(dat$Pos >0, 1, 0)
# duplicate LON LAT and make X Y
dat$X <- dat$LON
dat$Y <- dat$LAT
# rename if there's a mess up
# names(dat)[names(dat) == "X"] <- "Y"

# Here, I'm going to sub BOTTEMP for temp and DEPTH for depth
# if get errors, look for NAs
sum(is.na(dat$BOTTEMP))
# There are NAs, so remove them, but the tidy way isn't working
# library(tidyr)
# dat %>% drop_na(BOTTEMP, SURFTEMP)
# dat %>% drop_na()
# dat %>% filter(!is.na(BOTTEMP))
dat <- na.omit(dat)

# Write data to file to share with Pat
write.csv(dat, "/Users/janellemorano/Git/Reference-R-scripts/R-INLA/dat.csv")

############## VISUALIZE DATA
# Plot menhaden sample locations
library(tidyverse)
library(sp)
library (rnaturalearth)
library (rnaturalearthdata)
library(viridis)
world <- ne_countries(scale = "medium", returnclass = "sf")  
ggplot(data = world) +
  geom_sf() +
  coord_sf (xlim = c(-80,-65), ylim = c (30,45), expand = FALSE ) +
  geom_point(data = menhaden,
             aes (x = LON, y = LAT, color = YEAR)) + #remove , size=BIOMASS for survey loc
  scale_color_continuous(type = "viridis") +
  scale_fill_viridis() +
  theme_bw() +
  theme (axis.text = element_blank()) +
  facet_wrap(~SVSPP) +
  xlab("longitude") + 
  ylab("latitude")


####### Bottom TEMPERATURE
# Annual bottom temp
bottemp <- dat %>%
  group_by(YEAR) %>%
  summarise(avg = mean(BOTTEMP))

ggplot(data = bottemp, aes(YEAR, avg)) +
  geom_line() +
  theme_bw() +
  ggtitle("Bottom Temperature") +
  xlab("") + 
  ylab("Average Temp")

# Annual surface temp
surftemp <- dat %>%
  group_by(YEAR) %>%
  summarise(avg = mean(SURFTEMP))

ggplot(data = surftemp, aes(YEAR, avg)) +
  geom_line() +
  theme_bw() +
  ggtitle("Surface Temperature") +
  xlab("") + 
  ylab("Average Temp")

# Sea surface and bottom temp combined
ggplot() +
  geom_line(data = surftemp, aes(YEAR, avg), color = "red") +
  geom_line(data = bottemp, aes(YEAR, avg), color = "blue") +
  theme_bw() +
  theme(legend.position="bottom") +
  ggtitle("Surface and Bottom Temperature") +
  xlab("") + 
  ylab("Average Temp")




###########
# Modeling
###########

###### Begin Step 1: Spatial partial differential equation #####

#Need to make a mesh
#Use function to convert to UTM 
LLtoUTM(dat)

#X and Y are UTMS
# locs = as.matrix(cbind(dat$X, dat$Y)) 
# but Xkm and Ykm is actually the UTM conversion?? I'm confused, but this works better
locs = as.matrix(cbind(dat$Xkm, dat$Ykm)) 

# Look at the semivariogram
#To visualize the spatial dependency 
d = dist(locs)
# distance is likely in km, so convert to meter?
h = hist(d/1000, breaks = 50, freq = T, main = '', xlab = 'Distance Between Sites (km)', ylab = 'Frequency')
plot(sort(d), (1:length(d))/length(d), type = 'l', xlab = 'Distance Between Sites', ylab = 'Cumulative Proportion')  

# Find the range in the semivariogram
# Spatial dependency is probably around 8 km based on near peak of histogram
# This will be a judgement specific to each data set
# rangeguess = 8*1000
# times 1000 to make it in meters
rangeguess = 800
# 600
# 800

#recommended settings
# maxedge = rangeguess/5
# convhull = inla.nonconvex.hull(locs, 10*1000)
# mesh.dat = inla.mesh.2d(boundary = convhull, max.edge = c(1,5)*maxedge, cutoff = maxedge/5)
# mesh.dat$n  

# it's about 1/2 or 1/3, he's using 1/5
maxedge = rangeguess/5
convhull = inla.nonconvex.hull(locs, 1000) #I don't know where the 10*1000 comes from
mesh.dat = inla.mesh.2d(boundary = convhull, 
                        max.edge = c(1,5)*maxedge, 
                        cutoff = maxedge/5)
mesh.dat$n
# rangeedge = 800, n=892
# rangeedge = 1000, n=595
# rangeedge = 800, rangeguess/3, n=365
# rangeguess/2, n=171

#Visualize spatial field
plotmesh(mesh.dat, locs)

#define the spde
spde = inla.spde2.pcmatern(mesh.dat, prior.range = c(15000, 0.05), prior.sigma = c(2.0, 0.05))
#the prior for range says P(range < range0) = 0.05, basically forcing the range to be larger than 15km
#the prior for sigma says P(sigma > sigma0) = 0.05, basically forcing the sigma to be smaller than 2.0
spde$n.spde

#spatial weights
w.index = inla.spde.make.index(name = 'w', n.spde = spde$n.spde)

###### End Step 1: Spatial partial differential equation #####


##### Begin model M1: 5 basis functions all smoothers, spatial model with Year iid ######

#create basis functions and linear combinations
#value for k is a guess at this point, but should be explored more thoroughly to find appropriate smoother complexity

BasisTemp = smoothCon(s(BOTTEMP, k = 5, fx = T), data = dat, knots = NULL, absorb.cons = T)[[1]]$X
BasisDepth = smoothCon(s(DEPTH, k = 5, fx = T), data = dat, knots = NULL, absorb.cons = T)[[1]]$X


#k-1 number of knots
colnames(BasisTemp) = paste('Temp', 1:4, sep = '')
colnames(BasisDepth) = paste('Depth', 1:4, sep = '')

#need separate linear combinations for each model component.  Number elements corresponds to number of knots
#positive model
lcs.tempPos = inla.make.lincombs(Temp1Pos = BasisTemp[,'Temp1'], Temp2Pos = BasisTemp[,'Temp2'], Temp3Pos = BasisTemp[,'Temp3'], Temp4Pos = BasisTemp[,'Temp4'])
lcs.depthPos = inla.make.lincombs(Depth1Pos = BasisDepth[,'Depth1'], Depth2Pos = BasisDepth[,'Depth2'], Depth3Pos = BasisDepth[,'Depth3'], Depth4Pos = BasisDepth[,'Depth4'])

#presence/absence model
lcs.tempPA = inla.make.lincombs(Temp1PA = BasisTemp[,'Temp1'], Temp2PA = BasisTemp[,'Temp2'], Temp3PA = BasisTemp[,'Temp3'], Temp4PA = BasisTemp[,'Temp4'])
lcs.depthPA = inla.make.lincombs(Depth1PA = BasisDepth[,'Depth1'], Depth2PA = BasisDepth[,'Depth2'], Depth3PA = BasisDepth[,'Depth3'], Depth4PA = BasisDepth[,'Depth4'])


#need to give each linear combination a unique name to avoide an error from INLA
names(lcs.tempPos) = paste(names(lcs.tempPos), 'TempPos', sep = '')
names(lcs.depthPos) = paste(names(lcs.depthPos), 'DepthPos', sep = '')

names(lcs.tempPA) = paste(names(lcs.tempPA), 'TempPA', sep = '')
names(lcs.depthPA) = paste(names(lcs.depthPA), 'DepthPA', sep = '')


#define the weighting factors (projector matrix)
A1 = inla.spde.make.A(mesh.dat, loc = locs)

#define positive and PA data frames with all covariates
#include year (categorical) since it will be a random effect
#include effort as an offset #didn't do this with my data because don't have effort
XPos = data.frame(InterceptPos = rep(1, nrow(dat)), 
                  Temp1Pos = BasisTemp[,'Temp1'], Temp2Pos = BasisTemp[,'Temp2'], Temp3Pos = BasisTemp[,'Temp3'], Temp4Pos = BasisTemp[,'Temp4'],
                  # Depth1Pos = BasisDepth[,'Depth1'], Depth2Pos = BasisDepth[,'Depth2'], Depth3Pos = BasisDepth[,'Depth3'], Depth4Pos = BasisDepth[,'Depth4'], 
                  YearPos = dat$YEAR) #, 
                  #EffortPos = dat$effort)


X01 = data.frame(InterceptPA = rep(1, nrow(dat)), 
                 Temp1PA = BasisTemp[,'Temp1'], Temp2PA = BasisTemp[,'Temp2'], Temp3PA = BasisTemp[,'Temp3'], Temp4PA = BasisTemp[,'Temp4'],
                 Depth1PA = BasisDepth[,'Depth1'], Depth2PA = BasisDepth[,'Depth2'], Depth3PA = BasisDepth[,'Depth3'], Depth4PA = BasisDepth[,'Depth4'],
                 YearPA = dat$YEAR) #, 
                 #EffortPA = dat$effort)

#create the stacks 
StackPos = inla.stack(tag = 'FitPos', data = list(AllY = cbind(dat$Pos, NA)), A = list(1, A1), effects = list(XPos = XPos, wPos = wPos.index1))
Stack01 = inla.stack(tag = 'FitPA', data = list(AllY = cbind(NA, dat$PA)), A = list(1, A1), effects = list(X01 = X01, w01 = w01.index1))
StackAll = inla.stack(StackPos, Stack01)

#combine all linear combinations
lcs.all = c(lcs.tempPos, lcs.depthPos, lcs.tempPA, lcs.depthPA, lcs.juvPA)


fZA.gam = formula(AllY ~ -1 + InterceptPos +  #-1 removes intercept since manually defined in XPos and X01
                    Temp1Pos + Temp2Pos + Temp3Pos + Temp4Pos + 
                    Depth1Pos + Depth2Pos + Depth3Pos + Depth4Pos +
                    offset(EffortPos) + 
                    f(YearPos, model = 'iid', hyper = list(prec = list(prior = 'loggamma', param = c(1, 0.5)))) +  #year random effect for Pos data, loggamma prior with changed precision
                    f(wPos, model = spde) + #spatial field for Pos data
                    InterceptPA + 
                    Temp1PA + Temp2PA + Temp3PA + Temp4PA +
                    Depth1PA + Depth2PA + Depth3PA + Depth4PA +
                    offset(EffortPA) + 
                    f(YearPA, model = 'iid', hyper = list(prec = list(prior = 'loggamma', param = c(1, 0.5)))) + 
                    f(w01, model = spde))

#prior to force the conditional model (Pos) to be postive only since no actual zero-truncated distribution
HyperZANB = list(theta = list(initial = -10, fixed = T)) 

M1 <- inla(fZA.gam, family = c('zeroinflatednbinomial0','binomial'), data = inla.stack.data(StackAll), 
           control.family = list(list(hyper = HyperZANB),list()),
           control.compute = list(config = TRUE, dic = TRUE, waic = TRUE), lincomb = lcs.all,
           control.predictor = list(link = 1, A = inla.stack.A(StackAll), compute = T), 
           control.inla = list(strategy = 'gaussian'), verbose = T) 


#extract smoothers - back transform out of link space (if desired)
#need to keep track of row numbers
#Pos
Pos.temp = exp(M1$summary.lincomb.derived[1:nrow(dat) + 0*nrow(dat), 'mean'])
Pos.depth = exp(M1$summary.lincomb.derived[1:nrow(dat) + 1*nrow(dat), 'mean'])

#PA
PA.temp = plogis(M1$summary.lincomb.derived[1:nrow(dat) + 2*nrow(dat), 'mean'])
PA.depth = plogis(M1$summary.lincomb.derived[1:nrow(dat) + 3*nrow(dat), 'mean'])


#Multiply
temp.sm = Pos.temp * PA.temp
depth.sm = Pos.depth * PA.depth


#Need to order the covariates to avoid spaghetti plots 
OTemp = order(dat$temp)
ODepth = order(dat$depth)

#plot neonate
smoothers = data.frame(mu = c(temp.sm[OTemp], neo.depth.sm[ODepth]),
                       Xaxis = c(sort(dat$temp), sort(dat$depth)),
                       ID = factor(rep(c('Temp', 'Depth'), 
                       each = nrow(dat))))

p = ggplot() + xlab('Covariate') + ylab('Smoother') + geom_line(data = smoothers, aes(x = Xaxis, y = mu)) +
  facet_wrap(~ID, scales = 'free', ncol = 2)
p

#Note: can extract sd (e.g., M1$summary.lincomb.derived[1:nrow(dat) + 0*nrow(dat), 'sd'] if want uncertainty estimates of smoothers on link scale
#      need to apply delta approximation method (or some other procedure) if want SEs on original scale given nonlinear anti-link functions (log, plogis)
#      also need to apply another SE approximation method (e.g., Goodman 1960) for product of random variables if want 
#          SE of product of two model components
#      Above only plots back transformed multiplied smoothers and not associated uncertainty

#fixed effects
M1$summary.fixed

#Effects plot
Efxplot(list(M1))

##### End model M1: 5 basis functions all smoothers, spatial model with Year iid ######
