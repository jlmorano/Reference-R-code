# remote sensing tutorial
# by Kevin Friedland

## Goals:
# Retrieve data from the web
# Read Network Common Data Form (NetCDF) files of
# Chlorophyll concentration data
# Convert to raster
# Correct orientation and geo-reference
# Save raster objects and PDF maps
# Extract point data using two methods
# Extract data using a shape
# Look for seasonal trends
# Compare bloom time series
# Extract area estimates given chlorophyll conditions

#  required libraries
require(ncdf4)
require(raster)
require(maps)
require(marmap)
require(chron)
require(maptools)
require(Kendall)

# --------------------------- first part read in data from ncdf files and write as R rasters

# set working directory to where ncdf file are:
setwd("/Users/janellemorano/Git/Reference-R-scripts/Remote sensing")
wd=getwd()

# OPTION to record graphs in pdf file
# put this before plot commands
# pdf("methods_output.pdf")
# and this after:
# dev.off()
# which closes the pdf file
# the will make plots data for each year and out them in a pdf directory

#  text used to form output file names
index.name.1 <- "CHL1"
index.name.2 <- "MNTH"
index.name.3 <- "GB"
fmb="RAST_NESREG"

# get bathy data to add isobath to maps later on
getNOAA.bathy(lon1 = -80, lon2 = -60, lat1 = 32, lat2 = 48,
              resolution = 10) -> nesbath

# make of list of the files to be analyzed, use the GSM method only (files have "GSM" in them)
files = list.files(pattern="GSM")

# open one of the files to take a look
raster.ncdf <- nc_open(files[15])

# get the lon and lat extracted
lons = ncvar_get(raster.ncdf,varid = "lon")
lats = ncvar_get(raster.ncdf,varid = "lat")

# define bounding box of the extent matching the data in the nc files
bb = extent(min(lons),max(lons),min(lats),max(lats))

# clean up
remove(lons)
remove(lats)

# list data variables in file
names(raster.ncdf$var)

# read data from file, make into matrix
raster.full <- as.matrix(ncvar_get(raster.ncdf,varid = "CHL1_mean")) 

# convert the matrix to a raster
masked.raster<- raster(raster.full)

# take a look at the raster
plot(log(masked.raster))

# does not look right, need to transpose
masked.raster=t(masked.raster)

# take another look
plot(log(masked.raster))

# set the lon lat extent of the raster
extent(masked.raster) <- bb

# take another look
plot(log(masked.raster))

# add spatial projection so areas calculations are in real units
crs(masked.raster) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0" 

# add map and 200m isobath to the map
map("world",add=T, fill=T, col="grey")
plot(nesbath,deep=-200, shallow=-200, step=1,add=T,lwd=1,col="gray50",lty=2)


# create dir to put rasters
dir.create(file.path(wd,"rasters"), showWarnings = FALSE)
# create dir to put pdfs
dir.create(file.path(wd,"pdfs"), showWarnings = FALSE)


for ( i in 1:length(files)){
  print(i)

# from above get data and form raster
raster.ncdf <- nc_open(files[i])
raster.full <- as.matrix(ncvar_get(raster.ncdf,varid = "CHL1_mean")) 
masked.raster<- raster(raster.full)
masked.raster=t(masked.raster)
extent(masked.raster) <- bb
crs(masked.raster) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0" 

# Extract data from the file name to make a raster filename
year.label   <- as.numeric(substr(as.character(files[i]),5,8))
day.label    <- as.numeric(substr(as.character(files[i]),11,12))
month.label  <- as.numeric(substr(as.character(files[i]),9,10))
jul.code  <- julian(month.label,day.label,year.label, c(1,0,1800))
jul.code = sprintf("%09.0f",jul.code)
date.code <- paste(sprintf("%04.0f",year.label),
                   sprintf("%02.0f",month.label),
                   sprintf("%02.0f",day.label),sep = ".")
file.name.rast=paste(fmb,date.code,index.name.3,index.name.1,index.name.2,jul.code,"RData",sep = ".")
substr(file.name.rast,12,13)="_"

# save the data as a raster for further analysis
save(masked.raster, file=paste(wd,"/rasters/",file.name.rast, sep=""))

# also save it as a pdf file to take a look
pdf(paste(wd,"/pdfs/chl_map_",date.code,".pdf",sep=""))
plot(log(masked.raster))
map("world",add=T, fill=T, col="grey")
plot(nesbath,deep=-200, shallow=-200, step=1,add=T,lwd=1,col="gray50",lty=2)
mtext(file.name.rast,side=3)
dev.off()

} # end the file loop

# clean up
remove(bb)
remove(index.name.1)
remove(index.name.2)
remove(index.name.3)
remove(fmb)
remove(file.name.rast)
remove(jul.code)
remove(day.label)
remove(month.label)
remove(year.label)
remove(raster.full)
remove(date.code)
nc_close(raster.ncdf)
remove(raster.ncdf)

#---------------------------------------------------------------------------------------
# read data rasters for a point location

# set working directory to where raster files are located
setwd("/Users/janellemorano/Git/Reference-R-scripts/Remote sensing/rasters")
wd=getwd()

# make of list of the files to be analyzed
files = list.files(pattern="RAST")

# a point location for data extraction
xy <- cbind(-67, 40.75)

# loat a file to make a map and see the location
load(files[15])
plot(log(masked.raster))
map("world",add=T, fill=T, col="grey")
plot(nesbath,deep=-200, shallow=-200, step=1,add=T,lwd=1,col="gray50",lty=2)
points(xy,pch=19,col="red")

# make a data.frame to recieve data extraction
chl.data = data.frame(array(data=NA, dim=c(length(files),7)))
colnames(chl.data) <- c("x","y","year", "month", "chl_simple","chl_bilin","jday")

# for each file
for ( i in 1:length(files)){
  print(i)
  load(files[i])
  
  # extact data used two methods
  chl_simple = extract(masked.raster, xy, method='simple')
  chl_bilin = extract(masked.raster, xy, method='bilinear')
  
  year = substr(files[i],13,16)
  month = substr(files[i],18,19)
  jday =  substr(files[i],41,45)
  
  chl.data[i,1]=xy[1]
  chl.data[i,2]=xy[2]
  chl.data[i,3]=year
  chl.data[i,4]=month
  chl.data[i,5]=chl_simple
  chl.data[i,6]=chl_bilin
  chl.data[i,7]=jday
  
  
} # end the file loop

# contrast extraction methods, plot simple v bilinear
plot(chl.data$chl_simple,chl.data$chl_bilin,
     ylab = expression("Chlorophyll bilinear " ~italic(a) ~ "mg" ~ m^{-3}),
     xlab = expression("Chlorophyll simple " ~italic(a) ~ "mg" ~ m^{-3}))

# plot 1:1 line
abline(0,1,col="red")

# add regression line
reg1 <- lm(chl.data$chl_bilin~chl.data$chl_simple)
summary(reg1)
abline(reg1, col="purple")

# time series of all simple data, is there a trend, Kendall package
plot(chl.data$jday,chl.data$chl_simple,
     ylab = expression("Chlorophyll simple " ~italic(a) ~ "mg" ~ m^{-3}),
     xlab = "Julian day")

# add regression
reg1 <- lm(chl.data$chl_simple~as.numeric(chl.data$jday))
summary(reg1)
abline(reg1, col="red")

# trend test
MannKendall(chl.data$chl_simple)

# time series of all blinear data, is there a trend, Kendall package
plot(chl.data$jday,chl.data$chl_bilin,
     ylab = expression("Chlorophyll bilinear " ~italic(a) ~ "mg" ~ m^{-3}),
     xlab = "Julian day")

# add regression
reg1 <- lm(chl.data$chl_bilin~as.numeric(chl.data$jday))
summary(reg1)
abline(reg1, col="blue")

# trend test
MannKendall(chl.data$chl_bilin)

# what month has the highest average chl , from these data how many blooms likely occur
plot(aggregate(chl.data$chl_simple~chl.data$month, FUN = "mean"), ylim=c(.5,1.4),col="red",
     ylab = expression("Chlorophyll" ~italic(a) ~ "mg" ~ m^{-3}),
     xlab = "Month")
lines(aggregate(chl.data$chl_simple~chl.data$month, FUN = "mean"),col="red")

points(aggregate(chl.data$chl_bilin~chl.data$month, FUN = "mean"),col="blue")
lines(aggregate(chl.data$chl_bilin~chl.data$month, FUN = "mean"),col="blue")
mtext("simple",side=3,col="red", at=1)
mtext("bilinear",side=3,col="blue", at=3)

# has the main bloom gotten larger or smaller over time at point location
# put years in x
x=chl.data$year[chl.data$month=="04"]

# plot the April data for the two methods
par(mar=c(5,5,3,4))
plot(x,  chl.data$chl_simple[chl.data$month=="04"],
     ylim=c(.5,3.5), col="red",
     ylab = expression("Chlorophyll" ~italic(a) ~ "mg" ~ m^{-3}),
     xlab = "Year")
points(x,  chl.data$chl_bilin[chl.data$month=="04"] ,col="blue" )
lines(x,  chl.data$chl_simple[chl.data$month=="04"], col="red"  )
lines(x,  chl.data$chl_bilin[chl.data$month=="04"]   ,col="blue" )
mtext("simple",side=3,col="red", at=2004)
mtext("bilinear",side=3,col="blue", at=2007)

#---------------------------------------------------------------------------------------
# read data rasters for a shaped area

# is the point location representative of Georges Bank

# read in shape to represent Georges Bank
gb_mask = readShapePoly("/Users/janellemorano/Git/Reference-R-scripts/Remote sensing/shape file")

# plot last raster used, add map, point location and the GB shape
load(files[15])
plot(log(masked.raster))
map("world",add=T, fill=T, col="grey")
plot(nesbath,deep=-200, shallow=-200, step=1,add=T,lwd=1,col="gray50",lty=2)
points(xy,pch=19,col="red")
plot(gb_mask,add=T)

# make an index raster with locations numbered
index.raster <- masked.raster
index.raster[] <- 1:ncell(index.raster)
plot(index.raster)

# extract grid locations for GB mask
grid.indices <- extract(index.raster,gb_mask)

# extact data from last raster using GB grid locations only
mean(extract(masked.raster,unlist(grid.indices)),na.rm = T)

# add column to data.frame to recieve chl data used shape extraction
chl.data$chl_shape=NA

# extact the data for each raster
for ( i in 1:length(files)){
  print(i)
  load(files[i])
  chl.data[i,8] = mean(extract(masked.raster,unlist(grid.indices)),na.rm = T)
} # end the file loop

# has the main bloom gotten large or small over time at point location
# put years in x
x=chl.data$year[chl.data$month=="04"]

# what month has the highest average chl , from these data how many blooms likely occur
plot(aggregate(chl.data$chl_simple~chl.data$month, FUN = "mean"), ylim=c(.5,2.2),col="red",
     ylab = expression("Chlorophyll" ~italic(a) ~ "mg" ~ m^{-3}),
     xlab = "Month")
lines(aggregate(chl.data$chl_simple~chl.data$month, FUN = "mean"),col="red")
points(aggregate(chl.data$chl_bilin~chl.data$month, FUN = "mean"),col="blue")
lines(aggregate(chl.data$chl_bilin~chl.data$month, FUN = "mean"),col="blue")
points(aggregate(chl.data$chl_shape~chl.data$month, FUN = "mean"),col="green")
lines(aggregate(chl.data$chl_shape~chl.data$month, FUN = "mean"),col="green")
mtext("simple",side=3,col="red", at=1)
mtext("bilinear",side=3,col="blue", at=3)
mtext("shape",side=3,col="green", at=5)


# add the shape data to the plot, 
par(mar=c(5,5,3,4))
plot(x,  chl.data$chl_simple[chl.data$month=="04"],
     ylim=c(.5,4.5), col="red",
     ylab = expression("Chlorophyll" ~italic(a) ~ "mg" ~ m^{-3}),
     xlab = "Year")
points(x,  chl.data$chl_bilin[chl.data$month=="04"] ,col="blue" )
points(x,  chl.data$chl_shape[chl.data$month=="04"] ,col="green" )
lines(x,  chl.data$chl_simple[chl.data$month=="04"], col="red"  )
lines(x,  chl.data$chl_bilin[chl.data$month=="04"]   ,col="blue" )
lines(x,  chl.data$chl_shape[chl.data$month=="04"]  ,col="green"  )
mtext("simple",side=3,col="red", at=2004)
mtext("bilinear",side=3,col="blue", at=2007)
mtext("shape",side=3,col="green", at=2010)

# add regressions
reg1 <- lm(chl.data$chl_simple[chl.data$month=="04"]~as.numeric(x))
summary(reg1)
abline(reg1, col="red")
reg2 <- lm(chl.data$chl_bilin[chl.data$month=="04"]~as.numeric(x))
summary(reg2)
abline(reg2, col="blue")
reg3 <- lm(chl.data$chl_shape[chl.data$month=="04"]~as.numeric(x))
summary(reg3)
abline(reg3, col="green")

# trend tests
MannKendall(chl.data$chl_simple[chl.data$month=="04"])
MannKendall(chl.data$chl_bilin[chl.data$month=="04"])
MannKendall(chl.data$chl_shape[chl.data$month=="04"])

# add loess curves
simple.loess <- loess(chl.data$chl_simple[chl.data$month=="04"] ~ as.numeric(x), span=0.5)
simple.predict <- predict(simple.loess, as.numeric(x))
lines(as.numeric(x), simple.predict,col ="red",lwd=3)

bilin.loess <- loess(chl.data$chl_bilin[chl.data$month=="04"] ~ as.numeric(x), span=0.5)
bilin.predict <- predict(bilin.loess, as.numeric(x))
lines(as.numeric(x), bilin.predict,col ="blue",lwd=3)

shape.loess <- loess(chl.data$chl_shape[chl.data$month=="04"] ~ as.numeric(x), span=0.5)
shape.predict <- predict(shape.loess, as.numeric(x))
lines(as.numeric(x), shape.predict,col ="green",lwd=3)

#---------------------------------------------------------------------------------------
# determine areas given chlorophyll threshold

# get a file to serve as template for mask
load(files[1])

# for index raster with numbered cells
index.raster <- masked.raster
index.raster[] <- 1:ncell(index.raster)

# take a look
plot(index.raster)

# get list of indecies for GB
grid.indices <- extract(index.raster,gb_mask)

# set to NA for GB and then reverse, index raster is blanking raster for GB
index.raster[unlist(grid.indices)]=NA
index.raster[index.raster>0]=2
index.raster[is.na(index.raster)]=1
index.raster[index.raster==2]=NA

# take a look
plot(index.raster)

# make a data.frame to receive data
chl.data = data.frame(array(data=NA, dim=c(length(files),6)))
colnames(chl.data) <- c("year", "month", "area_.5","area_1","area_2","jday")


# extract the nummber of square km above chl threholds
for ( i in 1:length(files)){
  print(i)
  load(files[i])
  
  masked.raster[masked.raster<=.5]=NA
  area.masked.raster = area(masked.raster*index.raster, na.rm=TRUE, weights=FALSE)
  area_.5 = cellStats(area.masked.raster, stat='sum', na.rm=TRUE)
  
  masked.raster[masked.raster<=1]=NA
  area.masked.raster = area(masked.raster*index.raster, na.rm=TRUE, weights=FALSE)
  area_1 = cellStats(area.masked.raster, stat='sum', na.rm=TRUE)
  
  masked.raster[masked.raster<=2]=NA
  area.masked.raster = area(masked.raster*index.raster, na.rm=TRUE, weights=FALSE)
  area_2 = cellStats(area.masked.raster, stat='sum', na.rm=TRUE)
  
  year = substr(files[i],13,16)
  month = substr(files[i],18,19)
  jday =  substr(files[i],41,45)
  
  chl.data[i,1]=year
  chl.data[i,2]=month
  chl.data[i,3]=area_.5
  chl.data[i,4]=area_1
  chl.data[i,5]=area_2
  chl.data[i,6]=jday
  
} # end the file loop


# time series of all data, is there a trend, Kendall package
plot(chl.data$jday,chl.data$area_.5, xlab="Julian day", ylab=expression("Area 0.5 " ~ km^{2}))
plot(chl.data$jday,chl.data$area_1, xlab="Julian day", ylab=expression("Area 1.0 " ~ km^{2}))
plot(chl.data$jday,chl.data$area_2, xlab="Julian day", ylab=expression("Area 2.0 " ~ km^{2}))

# is there a monthly trend in the area data
plot(aggregate(chl.data$area_.5~chl.data$month, FUN = "mean"),
     ylab = expression("Area 0.5 " ~ km^{2}),
     xlab = "Month")
lines(aggregate(chl.data$area_.5~chl.data$month, FUN = "mean"))

plot(aggregate(chl.data$area_1~chl.data$month, FUN = "mean"),
     ylab = expression("Area 1.0 " ~ km^{2}),
     xlab = "Month")
lines(aggregate(chl.data$area_1~chl.data$month, FUN = "mean"))

plot(aggregate(chl.data$area_2~chl.data$month, FUN = "mean"),
     ylab = expression("Area 2.0 " ~ km^{2}),
     xlab = "Month")
lines(aggregate(chl.data$area_2~chl.data$month, FUN = "mean"))

# has there been a time series trend in area for April
x=chl.data$year[chl.data$month=="04"]

par(mar=c(5,5,3,4))

plot(x,  chl.data$area_.5[chl.data$month=="04"], col="red",
     ylab = expression("Area 0.5 " ~ km^{2}),
     xlab = "Year")
lines(x,  chl.data$area_.5[chl.data$month=="04"], col="red"  )
reg1 <- lm(chl.data$area_.5[chl.data$month=="04"]~as.numeric(x))
summary(reg1)
abline(reg1)
MannKendall(chl.data$area_.5[chl.data$month=="04"])

plot(x,  chl.data$area_1[chl.data$month=="04"], col="red",
     ylab = expression("Area 1.0 " ~ km^{2}),
     xlab = "Year")
lines(x,  chl.data$area_1[chl.data$month=="04"], col="red"  )
reg1 <- lm(chl.data$area_1[chl.data$month=="04"]~as.numeric(x))
summary(reg1)
abline(reg1)
MannKendall(chl.data$area_1[chl.data$month=="04"])

plot(x,  chl.data$area_2[chl.data$month=="04"], col="red",
     ylab = expression("Area 2.0 " ~ km^{2}),
     xlab = "Year")
lines(x,  chl.data$area_2[chl.data$month=="04"], col="red"  )
reg1 <- lm(chl.data$area_2[chl.data$month=="04"]~as.numeric(x))
summary(reg1)
abline(reg1)
MannKendall(chl.data$area_2[chl.data$month=="04"])

# area extract area using another technique
# first as done above
load(files[1])
plot(masked.raster)
masked.raster[masked.raster<=1]=NA
plot(masked.raster)
area.masked.raster = area(masked.raster*index.raster, na.rm=TRUE, weights=FALSE)
plot(area.masked.raster)
area_first = cellStats(area.masked.raster, stat='sum', na.rm=TRUE)

# second the other was
load(files[1])
plot(masked.raster)
area.raster = masked.raster*index.raster
plot(area.raster)
thres=.5
area.raster[area.raster < 1] <- NA
plot(area.raster)
cell_size<-area(area.raster, na.rm=TRUE, weights=FALSE)
plot(cell_size)
area_second = cellStats(cell_size, sum)

print(c(area_first,area_second))



