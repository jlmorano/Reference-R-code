# Simple example
# from https://github.com/James-Thorson-NOAA/VAST/wiki/Simple-example

# Example of using VAST with high-level wrapper functions

# Download latest release number; its useful for reproducibility to use a specific release number
devtools::install_github("james-thorson/VAST")

# Set local working directory (change for your machine)
setwd("/Users/janellemorano/Git/Reference-R-scripts")

# Load package
library(VAST)

# load data set
# see `?load_example` for list of stocks with example data 
# that are installed automatically with `FishStatsUtils`. 
example = load_example( data_set="EBS_pollock" ) 
# columns: lat, lon, year, catch, spp, water temp C, tow ID
head(EBS_pollock_data)
# lat; long; year; catch; spp; waterTmpC; TowID

plot(EBS_pollock_data$catch ~ EBS_pollock_data$year)

unique(EBS_pollock_data$year)
# [1] 1982 1983 1984 1985 1986 1987 1988 1989 1990 1991 1992 1993 1994 1995 1996 1997 1998 1999 2000 2001
# [21] 2002 2003 2004 2005 2006 2007 2008 2010 2009 2011 2012 2013 2014

library(dplyr)
EBS_pollock_data %>%
  select(year, catch, waterTmpC) %>%
  #group_by(year) %>%
  summarise(EBS_pollock_data, median)

# Make settings (turning off bias.correct to save time for example)
settings = make_settings( n_x=100, #number of stations
                          Region=example$Region, #what are the assumptions? does it pre-define spatial area, etc?
                          purpose="index2", #originally set to index, but warning said The package author recommends using purpose=`index2` for updated defaults; purpose=`index` is retained for backwards compatibility but not recommended
                          strata.limits=example$strata.limits, #defined by data, but why...
                          bias.correct=FALSE ) #non-linear transformation of a random effect will result in a biased estimator (retransformation bias). Bias correct any derived quantity, esp when there are large differences in standard error (e.g., survey samples different prop of spatial domain in diff years)

# Run model
fit = fit_model( settings=settings, 
                 Lat_i=example$sampling_data[,'Lat'], 
                 Lon_i=example$sampling_data[,'Lon'], 
                 t_i=example$sampling_data[,'Year'], #INDEX, time interval
                 c_i=rep(0,nrow(example$sampling_data)), #INDEX, category, filling as "0"
                 b_i=example$sampling_data[,'Catch_KG'], #DATA, sample response
                 a_i=example$sampling_data[,'AreaSwept_km2'], #DATA, area covered by each sample
                 v_i=example$sampling_data[,'Vessel'] ) #DATA, overdispersion level for each sample

# Plot results
plot( fit )

## Output
# see also documents
# Aniso.png = Distance at 10% correlation. Decorrelation (geometric anisotropy) distance for different directions. (related to the variogram?) HOW TO INTERPRET?

# center_of_gravity.png = detect shifts in distribution or range expansion/contraction. Center of gravity (COG) indicating shifts in distribution plus/minus 1 SE. 
# Data_and_knots.png = Spatial extent and location of knots
# Data_by_year.png = NOT SURE IT'S SHOWING ANYTHING Spatial distribution of catch-rate data
# Effective_Area.png = Interpret expansion/contraction of range. Effective area occupied indicating range expansion/contraction plus/minus 1 SE
# Index-Biomass.png = Is this: Index of abundance plus/minus 2 SE? The y-axis should be Abundance
# Kmeans-100.RData
# ln_density-predicted.png = Predicted density maps for each year
# parameter_estimates.RData
# parameter_estimates.txt
# quantile_residuals_on_map.png = Pearson residuals for positive catch rates by knot
# quantile_residuals.png = DHARMa residual diagnostics: Q-Q plot residuals; Residuals vs. predicted
# settings.txt = model settings
# Table_for_SS3.csv = ?? Columns: Year, Unit, Fleet, Estimate_metric_tons, SD_log; SD_mt

