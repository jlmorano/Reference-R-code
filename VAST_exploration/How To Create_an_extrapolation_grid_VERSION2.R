### An example of how to create user-defined extrapolation
### regions (extents) for VAST.

### Cecilia O'Leary and Cole Monnahan | December 2020
# https://github.com/James-Thorson-NOAA/VAST/wiki/Creating-an-extrapolation-grid

### modified by Janelle as of May 18, 2021

### The extrapolation region defines the extent over which the
### model predictions are integrated. Any density outside of this
### region will not be included in estimates of the index. It is
### not used in model fitting. It comes with many built-in
### regions but often a user needs to define their own. Here, we
### demonstrate two ways of doing this: (1) From a set of points
### representing the outer extent of the region; (2) from an
### existing shape file.

library(sp) # 1.4.4
packageVersion('sp') # I have 1.4.5
library(sf) # 0.9.6
# Linking to GEOS 3.8.1, GDAL 3.1.4, PROJ 6.3.1
packageVersion('sf') # I have 0.9.6

# I have these data that I need to create an extrapolation grid for
### Use NEFSC bottom trawl survey data to try here
dat <- read.csv("/Users/janellemorano/Git/Reference-R-scripts/VAST_exploration/dat.csv", header = TRUE)
# View it
plot(dat$LON, dat$LAT)

# This is just Method 2, from an existing shapefile
### ------------------------------------------------------
library(rgdal) # '1.5.18'
# I have 1.5-23
# read in shapefile
shp <- readOGR("/Users/janellemorano/DATA/strata/finstr_nad83.shp", layer="finstr_nad83")
# plot it to look at it
plot(shp)
# transform the projection
sps <- spTransform(shp, CRS("+proj=longlat +lat_0=90 +lon_0=180 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0 "))
lon <- sum(bbox(sps)[1,])/2
## convert decimal degrees to utm zone for average longitude, use
## for new CRS
utmzone <- floor((lon + 180)/6)+1
### End method 2
### --------------------------------------------------

### --------------------------------------------------
### Create the VAST extroplation grid
## Convert the final in polygon to UTM
crs_UTM <- CRS(paste0("+proj=utm +zone=",utmzone," +ellps=WGS84 +datum=WGS84 +units=m +no_defs "))
region_polygon <- spTransform(sps, crs_UTM)
### Construct the extroplation grid for VAST using sf package
## Size of grid **in meters** (since working in UTM). Controls
## the resolution of the grid.
cell_size <- 2000
## Create a grid over the geometry of the region_polygon object
## This step is slow at high resolutions
region_grid <- st_make_grid(region_polygon, cellsize = cell_size, what = "centers")
## Convert region_grid to Spatial Points to SpatialPointsDataFrame
region_grid <- as(region_grid, "Spatial")
region_grid_sp <- as(region_grid, "SpatialPointsDataFrame")
## combine shapefile data (region_polygon) with Spatial Points
## (region_grid_spatial) & place in SpatialPointsDataFrame data
## (this provides you with your strata identifier (here called
## Id) in your data frame))
head (shp@data)
# AREA PERIMETER FINSTR_G_ FINSTR_G_I STRATA   A2
# 0 0.164     6.122         2       3840   3820 1712
# 1 0.036     2.681         3       3088   3880  375
# In this data, it's STRATA, so replace Id with STRATA
region_grid_sp@data <- over(region_grid, region_polygon)

#### ADDED not sure if will help
crs_LL <- CRS('+proj=longlat +datum=WGS84 +no_defs') #good for PROJ6
sps@proj4string <- crs_LL
#####

## Convert back to lon/lat coordinates as that is what VAST uses
region_grid_LL <- as.data.frame(spTransform(region_grid_sp, crs_LL))
region_df <- with(region_grid_LL,
                  data.frame(Lon=coords.x1,
                             Lat=coords.x2, STRATA,
                             Area_km2=( (cell_size/1000^2)),
                             row=1:nrow(region_grid_LL)))
## Filter out the grid that does not overlap (outside extent)
region <- subset(region_df, !is.na(STRATA))
## This is the final file needed.
str(region)
# 'data.frame':	80406 obs. of  5 variables:
#   $ Lon     : num  -77.8 -77.8 -77.9 -77.8 -77.8 ...
# $ Lat     : num  32.5 32.5 32.5 32.5 32.5 ...
# $ STRATA  : int  8770 8770 8770 8770 8770 8770 8770 8770 8770 8770 ...
# $ Area_km2: num  0.002 0.002 0.002 0.002 0.002 0.002 0.002 0.002 0.002 0.002 ...
# $ row     : int  62 63 661 662 663 1260 1261 1262 1263 1264 ...

### Save it to be read in and passed to VAST later.
saveRDS(region, file = "user_region.rds")
### End of creating user extrapolation region object
### --------------------------------------------------

### Quick plots of the process for method 1
png('user_region.png', width=7, height=7, units='in', res=200)
par(mfrow=c(2,2))
with(region_extent, plot(long, lat, main='Extent in points in LL'))
plot(region_polygon, main='Polygon in UTM', axes=TRUE)
plot(region_grid, col=ifelse(is.na(region_df$Id), 'red', 'black'),
     axes=TRUE, main='Extrapolation area UTM')
with(region, plot(Lon, Lat, main='Extrapolation region in LL', pch='.'))
dev.off()


### Show how to run it in VAST
library(VAST)
dat <- load_example(data_set='EBS_pollock')$sampling_data
dat <- subset(dat, Year==2000)

settings <- make_settings(n_x=200, Region='User',
                          purpose="index2", bias.correct=FALSE,
                          knot_method='grid')
settings$FieldConfig[2,] <- 0 ## turn off temporal components
user_region <- readRDS('user_region.rds')
fit <- fit_model(settings=settings,
                 Lat_i=dat$Lat, Lon_i=dat$Lon,
                 t_i=dat$Year, b_i=dat$Catch_KG,
                 a_i=dat$AreaSwept_km2,
                 input_grid=user_region)
plot_results(fit, plot_set=3)