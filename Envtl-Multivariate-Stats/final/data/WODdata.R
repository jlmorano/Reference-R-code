# Taking World Ocean Database data and then re-format it into a dataframe in R, keeping only some of the variables
# From Kurt Heim's R code via Janet Nye

#High resolution CTD/XCTD (CTD)

# CTD of the east coast ocldb1651955704.9526_CTD.nc
# Western edge = -80
# Eastern edge = -68
# Northern edge = 42
# Southern edge = 33 


library(ncdf4)#the packge for .nc files
setwd("/Users/janellemorano/DATA/WOD")

# Files from WODselect website
file1<-("ocldb1651955704.9526_CTD.nc") #east coast CTC from WODselect download site
file2<-("ocldb1652117625.10157_OSD.nc") #east coast Bio from WODselect download site

con1 <- ncdf4::nc_open(file1)#the file connection

# con2<-ncdf4::nc_open(file2)#the file connection #THIS LINE ISN"T WORKING
ncpath <- "/Users/janellemorano/DATA/WOD/"
ncname <- "ocldb1652117625.10157_OSD"  
ncfname <- paste(ncpath, ncname, ".nc", sep="")
# dname <- "tmp"  # note: tmp means temperature (not temporary)
ncin <- nc_open(ncfname)
print(ncin)

###heres just a look at the dataset, a map with points colored according to botom depth
cfunk<-colorRampPalette(c("red","blue"))
col = cfunk(100)[as.numeric(cut(as.vector(ncdf4::ncvar_get(con1, "Bottom_Depth")), breaks = 100))]
plot(as.vector(ncdf4::ncvar_get(con1, "lon")),as.vector(ncdf4::ncvar_get(con1, "lat")),
     col = col, pch = 19, cex = .1)#locations

###here is the second dataset
# col = cfunk(100)[as.numeric(cut(as.vector(ncdf4::ncvar_get(con2, "Bottom_Depth")), breaks = 100))]
# plot(as.vector(ncdf4::ncvar_get(con2, "lon")),as.vector(ncdf4::ncvar_get(con2, "lat")),
     # col = col, pch = 19, cex = .1)#locations
###IF THIS PLOT DOESNT SHOW WHAT YOU EXPECTED THEN YOU PROBABLY
###DID NOT DOWNLOAD THE DATA CORRECTLY

##
# Part 1
###############
# use the netCDF connections (con1 and con2) to create a 'list' of data.frames, each storing a 'typical' CTD cast (i.e., first column is depth, next is temp, salinity, etc.) that are easy to work with by indexing.

make_casts<-function(connection){
  ddd<-data.frame(platform = as.vector(ncdf4::ncvar_get(connection, "Platform")),
                  originators_cruise_identifier = as.vector(ncdf4::ncvar_get(connection, "originators_cruise_identifier")),
                  wod_unique_cast = as.vector(ncdf4::ncvar_get(connection, "wod_unique_cast")),
                  Institute = as.vector(ncdf4::ncvar_get(connection, "Institute")),
                  z_row_size = as.vector(ncdf4::ncvar_get(connection, "z_row_size")),
                  Temperature_row_size = as.vector(ncdf4::ncvar_get(connection, "Temperature_row_size")),
                  Salinity_row_size = as.vector(ncdf4::ncvar_get(connection, "Salinity_row_size")),
                  lat = as.vector(ncdf4::ncvar_get(connection, "lat")),
                  lon = as.vector(ncdf4::ncvar_get(connection, "lon")),
                  date = as.Date(as.character(as.vector(ncdf4::ncvar_get(connection, "date"))), format = "%Y%m%d"),
                  bot_depth = as.vector(ncdf4::ncvar_get(connection, "Bottom_Depth")),
                  Pressure_row_size = as.vector(ncdf4::ncvar_get(connection, "Pressure_row_size"))
                  #barometric_pressure = as.vector(ncdf4::ncvar_get(connection, "Barometric_Pres"))
  )
  ddd[is.na(ddd$z_row_size),"z_row_size"]<-0#needs to be 0 instead of na or cumsum fails below
  ddd[is.na(ddd$Salinity_row_size),"Salinity_row_size"]<-0
  ddd[is.na(ddd$Temperature_row_size),"Temperature_row_size"]<-0
  ddd[is.na(ddd$Pressure_row_size),"Pressure_row_size"]<-0
  
  
  ##Add necessary indexes for generating individual CTD cast dataframes
  ##This says..for the cast in this row, which section of the vector do I go get my data from...
  ddd$end_indexZ<-cumsum(ddd$z_row_size)#ending index of z
  ddd$start_indexZ<-ddd$end_indexZ-ddd$z_row_size+1#start index of z
  
  ddd$end_indexT<-cumsum(ddd$Temperature_row_size)#ending index of Temperature
  ddd$start_indexT<-ddd$end_indexT-ddd$Temperature_row_size+1#start index of z
  
  ddd$end_indexS<-cumsum(ddd$Salinity_row_size)#ending index of salinity
  ddd$start_indexS<-ddd$end_indexS-ddd$Salinity_row_size+1#start index salinity
  
  ddd$end_indexP<-cumsum(ddd$Pressure_row_size)#ending index of salinity
  ddd$start_indexP<-ddd$end_indexP-ddd$Pressure_row_size+1#start index salinity
  
  
  return(ddd)
}


# Next we need a function that can create a 'ctd' dataset using the information in in the netcdfs (con1 and con2); for this function it uses ddd and the input is only a single number, telling you what cast in the cast info dataframe (calling it ddd and it MUST be called ddd for function to work)

create.ctd<-function(castN){
  #castN=1
  depth = z[ddd[castN,"start_indexZ"]:ddd[castN,"end_indexZ"]]#adds a depth column
  
  if(ddd$Temperature_row_size[castN] >0){
    temp = t[ddd[castN,"start_indexT"]:ddd[castN,"end_indexT"]]}else{
      temp = NA
    }
  
  if(ddd$Salinity_row_size[castN] >0){
    salinity = s[ddd[castN,"start_indexS"]:ddd[castN,"end_indexS"]]} else{
      salinity = NA}
  
  if(ddd$Pressure_row_size[castN] >0){
    pressure = p[ddd[castN,"start_indexP"]:ddd[castN,"end_indexP"]]} else{
      pressure = NA}
  
  tempd<-data.frame(depth,temp,salinity, pressure)
  density<-rep(NA,nrow(tempd))
  
  
  for(m in 1:nrow(tempd)){
    if(rowSums(is.na(tempd[1:3]))[m] == 0){
      density[m]<-swRho(tempd[m,3],tempd[m,2], tempd[m,1]) #swRho from package[oce]
    }
  }
  tempd$density<-density
  return(tempd)
}

# Now use these functions to create a bunch of 'ctd' data frames from the connections.This chunk will take a bit of time to run and is probably a very inefficient way to do this task, but whatever. It works and you only need to do it once.

# WILL TAKE ABOUT FIVE MINUTES TO RUN. 
#####Prepare All data for con1
ddd<-make_casts(con1)
ctds1<-list()#this is the list that will contain individual CTD casts
z<-as.vector(ncdf4::ncvar_get(con1, "z"))#get vectors of variables
t<-as.vector(ncdf4::ncvar_get(con1, "Temperature"))#get vectors of variables
s<-as.vector(ncdf4::ncvar_get(con1, "Salinity"))#get vectors of variables
p<-as.vector(ncdf4::ncvar_get(con1, "Pressure"))#get vectors of variables

require(oce)
for(i in 1:nrow(ddd)){
  ctds1[[i]]<-create.ctd(i)
  #print(i) #dont do this, was only using so I could troubleshoot
}
ddd_con1<-ddd#resave ddd as ddd1 
###Now you have ctds1 (has a list of ctd data) and ddd_con1 (has the cast info)

# ddd<-make_casts(con2)#make cast dataset
# z<-as.vector(ncdf4::ncvar_get(con2, "z"))#get vectors of variables
# t<-as.vector(ncdf4::ncvar_get(con2, "Temperature"))
# s<-as.vector(ncdf4::ncvar_get(con2, "Salinity"))
# p<-as.vector(ncdf4::ncvar_get(con2, "Pressure"))#get vectors of variables
# ctds2<-list()#empty list to store stuff
# 
# for(i in 1:nrow(ddd)){
#   ctds2[[i]]<-create.ctd(i)
#   #print(i) dont do this, was only using so I could troubleshoot
# }
# ddd_con2<-ddd
###Now you have ctds2 (has a list of ctd data) and ddd_con2 (has the cast info)

# Plot some CTD data to make sure the stuff looks right.
i=100#which one to do
plot(ctds1[[i]]$temp, ctds1[[i]]$depth, ylim =c(rev(range(ctds1[[i]]$depth))),
     type = "b", col = "red", axes = FALSE, xlab = "", ylab = "")
axis(1);axis(2,las = 2)
par(new = TRUE)
plot(ctds1[[i]]$density, ctds1[[i]]$depth, ylim =c(rev(range(ctds1[[i]]$depth))),
     type = "b", col = "black", axes = FALSE, ylab = "", xlab = "")
axis(3)

####
# Part 2
##################
# Now you can take these and put them into a single dataframe (one cast per row) and include only the data you want for calculating indicators.

nrow(ddd_con1)#
length(ctds1)#good same length
ddd_con1$last_row_depth<-NA#depth of last row in cast i
ddd_con1$bot_temp<-NA#the temperature of the last row in cast i
ddd_con1$bot_sal<-NA#the salinity of the last row in cast i
ddd_con1$bot_den<-NA#the density of the last row in cast i
ddd_con1$first_row_depth<-NA#depth of first row in cast i
ddd_con1$surf_temp<-NA#temp of first row in cast i
ddd_con1$surf_sal<-NA#sal of first row in cast i
ddd_con1$surf_den<-NA#density of first row in cast i

ddd_con1$c50_depth<-NA#depth of obs closest to 50 m
ddd_con1$c50_temp<-NA#temp of obs closst to 50 m
ddd_con1$c50_sal<-NA#salinity of obs closest to 50 m
ddd_con1$c50_den<-NA#density of obs closest to 50 m

#thecast<-ctds1[[1]]


for(i in 1:nrow(ddd_con1)){
  thecast<-ctds1[[i]]#the cast data
  c50row<-which.min(abs(thecast$depth - 50))#row index of obs clsoest to 50 m
  
  ddd_con1[i,"last_row_depth"]<-thecast[nrow(thecast),"depth"]
  ddd_con1[i,"bot_temp"]<-thecast[nrow(thecast),"temp"]
  ddd_con1[i,"bot_sal"]<-thecast[nrow(thecast),"salinity"]
  ddd_con1[i,"bot_den"]<-thecast[nrow(thecast),"density"]
  ddd_con1[i,"first_row_depth"]<-thecast[1,"depth"]
  ddd_con1[i,"surf_temp"]<-thecast[1,"temp"]
  ddd_con1[i,"surf_sal"]<-thecast[1,"salinity"]
  ddd_con1[i,"surf_den"]<-thecast[1,"density"]
  
  ddd_con1[i, "c50_depth"]<-thecast[c50row, "depth"]
  ddd_con1[i, "c50_temp"]<-thecast[c50row, "temp"]
  ddd_con1[i, "c50_sal"]<-thecast[c50row, "salinity"]
  ddd_con1[i, "c50_den"]<-thecast[c50row, "density"]
  
  #print(i)
}
#Now ddd_con1 has what you want

# #do it for con 2 data
# ddd_con2$last_row_depth<-NA
# ddd_con2$bot_temp<-NA
# ddd_con2$bot_sal<-NA
# ddd_con2$bot_den<-NA
# ddd_con2$first_row_depth
# ddd_con2$surf_temp<-NA
# ddd_con2$surf_sal<-NA
# ddd_con2$surf_den<-NA
# 
# 
# ddd_con2$c50_depth<-NA#depth of obs closest to 50 m
# ddd_con2$c50_temp<-NA#temp of obs closst to 50 m
# ddd_con2$c50_sal<-NA#salinity of obs closest to 50 m
# ddd_con2$c50_den<-NA#density of obs closest to 50 m
# 
# for(i in 1:nrow(ddd_con2)){
#   thecast<-ctds2[[i]]
#   c50row<-which.min(abs(thecast$depth - 50))#row index of obs clsoest to 50 m
#   
#   ddd_con2[i,"last_row_depth"]<-thecast[nrow(thecast),"depth"]
#   ddd_con2[i,"bot_temp"]<-thecast[nrow(thecast),"temp"]
#   ddd_con2[i,"bot_sal"]<-thecast[nrow(thecast),"salinity"]
#   ddd_con2[i,"bot_den"]<-thecast[nrow(thecast),"density"]
#   ddd_con2[i,"first_row_depth"]<-thecast[1,"depth"]
#   ddd_con2[i,"surf_temp"]<-thecast[1,"temp"]
#   ddd_con2[i,"surf_sal"]<-thecast[1,"salinity"]
#   ddd_con2[i,"surf_den"]<-thecast[1,"density"]
#   #print(i)
#   
#   ddd_con2[i, "c50_depth"]<-thecast[c50row, "depth"]
#   ddd_con2[i, "c50_temp"]<-thecast[c50row, "temp"]
#   ddd_con2[i, "c50_sal"]<-thecast[c50row, "salinity"]
#   ddd_con2[i, "c50_den"]<-thecast[c50row, "density"]
# }

# Write to disk
write.csv(ddd_con1,"WOD_CTD_format_D50_eastcoast.csv")
# write.csv(ddd_con2,".csv")
rm(list = ls(all.names = TRUE))#will remove everything in workspace and hidden objects

## If you wanted to bind these files, follow next steps
#finalDDD<-rbind(ddd_con1,ddd_con2)

# Write to disk
#setwd("~/Desktop/NYB Indicators/CalculateIndicators/WaterTemperature/Data")
# write.csv(finalDDD,"WOD_CTD_format_D50_2007-2019.csv")
# rm(list = ls(all.names = TRUE))#will remove everything in workspace and hidden objects
# 
# write.csv(finalDDD,"WOD_CTD_format_D50_2007-2019.csv")
# rm(list = ls(all.names = TRUE))#will remove everything in workspace and hidden objects
# 
# temp <- read.csv("/Users/janellemorano/DATA/WOD/WOD_CTD_format_D50_2007-2019_NYBight.csv", header = TRUE)
