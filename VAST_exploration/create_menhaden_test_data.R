### Test data for VAST exploration

# Using test data: survdat_3_2020.RData from Kevin Friedland
load("/Users/janellemorano/DATA/Survdat_3_2020.RData")
# Subsetting out menhaden; 36=menhaden
menhaden <- subset(survdat, SVSPP %in% c(36))
# add species name for reference
menhaden$species <- c("menhaden")

# Assess which envt'l variables to look at
# library("PerformanceAnalytics")
# colnames(menhaden)
# chart.Correlation(menhaden[,c("DEPTH","SURFTEMP","SURFSALIN","BOTTEMP","BOTSALIN","BIOMASS", "ABUNDANCE")],histogram=TRUE, pch=19)
# BOTTEMP, SURFTEMP are highly correlated with Biomass
# For purposes here, going to choose BOTTEMP and DEPTH, both to follow the original script, but also because I would expect BOTTEMP to be significant and DEPTH to not be significant, so I can see how these interact in the model.

# Make menhaden a dataframe "dat" with BOTTEMP and DEPTH for ease of testing code
library(dplyr)
dat <- select(menhaden, "species", "STATION", "STRATUM", "YEAR", "SEASON", "LAT", "LON", "EST_TOWDATE", "DEPTH", "BOTTEMP", "ABUNDANCE", "BIOMASS")
write.csv(dat, "/Users/janellemorano/Git/Reference-R-scripts/VAST_exploration/dat.csv")
