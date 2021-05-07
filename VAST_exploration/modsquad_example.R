# modsquad example from Madison

#make extrapolation grid
#a csv file with Lat, Long, and grid cell area. I assume the Lat/Long is for the grid cell centroid?
GOAgrid <- read.csv(file= paste0(getwd(),"/Extrapolation_Grids/GOAThorsonGrid_Less700m.csv"))

input_grid <- cbind(Lat = GOAgrid$Lat,
                    Lon = GOAgrid$Lon,
                    Area_km2 = GOAgrid$Shape_Area/1000000)  # Extrapolation grid area is in m^2 & is converted to km^2 with this line

# Settings for model
settings = make_settings( Version = "VAST_v12_0_0", #.cpp version, not software #e.g., "VAST_v12_0_0"
                          n_x = 100, #knots aka spatial resolution of our estimates
                          Region = "User", ###HERE #instead of Region = "gulf_of_alaska" , go to ?make_settings for other built in extrapolation grids
                          purpose = "index2", #changes default settings
                          ObsModel= c(2,1)#, this is the default gamma obs error, other two options: #c(1,1) #c(10,2)
) 


#and then you can call the input grid you just defined in fit_model()

fit <- fit_model( "settings"= settings, #all of the settings we set up above
                  "Lat_i"= POPdata[,'Lat'], #latitude of observation
                  "Lon_i"= POPdata[,'Lon'],  #longitude of observation
                  "t_i"= POPdata[,'Year'], #time for each observation
                  "b_i"= POPdata[,'Catch_KG'], #in kg, raw catch or in CPUE per tow
                  "a_i"= POPdata[,'AreaSwept_km2'], #sampled area for each observation
                  "v_i"= POPdata[,'Vessel'], #ok to leave in because it's all "missing" in data, so NO vessel effects
                  "input_grid"= input_grid, #RIGHT HERE**********
                  "optimize_args" =list("lower"=-Inf,"upper"=Inf), #TMB argument (?fit_tmb) that can be used if you're having optimization issues, shouldn't need to change
                  "working_dir" = paste0(getwd(),"/",Species,"/"))
## Plot results
plot( fit )
## ##
## save the VAST model
saveRDS(fit,file = paste0(getwd(),"/",Species,"/",Species,"VASTfit.RDS"))