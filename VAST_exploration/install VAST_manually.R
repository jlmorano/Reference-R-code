##### Manual installation of VAST and dependencies
# This code was performed on a clean install of R 4.0.5 without any other packages installed, so some of these dependencies may not be necessary for others.
# by Janelle Morano, April 15, 2021 

sessionInfo()
# R version 4.0.5 (2021-03-31)
# Platform: x86_64-apple-darwin17.0 (64-bit)
# Running under: macOS Mojave 10.14.6

# Install and load devtools package
# install.packages("devtools")
library("devtools")

############ Standard recommended way to install #############
############ If doesn't work. Skip ahead ################
# # Install package
# install_github("james-thorson/VAST", INSTALL_opts="--no-staged-install")
# # Load package
# library(VAST)

############ Alternative option ####################
############ If doesn't work. Skip ahead ################
# Install TMB from CRAN
# install.packages("TMB")
# # Install INLA using currently recommended method
# install.packages("INLA", repos=c(getOption("repos"), INLA="https://inla.r-inla-download.org/R/stable"), dep=TRUE)
# # Install FishStatsUtils from CRAN
# devtools::install_github("james-thorson/FishStatsUtils", INSTALL_opts="--no-staged-install")
# #got errors 
# 
# #downloaded FishStatsUtil as zip
# devtools::install_local("dir/FishStatsUtils-main.zip")

########### THIS WORKED - Manual installation of VAST and dependencies ##################
# Under the VAST installation, point 2 and 6, regarding problems downloading from Github.
# And per #https://github.com/James-Thorson-NOAA/VAST/issues/280
# Matrix version should be 1.2.8
# TMB version 1.7.18 is needed. TMB 1.7.19 causes problems

# Download zips directly from Github (click "clone or download" -> "Download ZIP") and manually install. You may get an error that dependencies are missing, so you may need to install the dependencies listed below. In pathnames "dir" is the directory path of the zip

# FishStatsUtils-main.zip - download from https://github.com/James-Thorson-NOAA/FishStatsUtils
devtools::install_local(path="dir/FishStatsUtils-main.zip", dependencies=FALSE)
# ERROR: dependencies ‘mapproj’, ‘maptools’, ‘deldir’, ‘RANN’, ‘colorspace’, ‘RandomFields’, ‘RandomFieldsUtils’, ‘shape’, ‘mixtools’, ‘sp’, ‘plotKML’, ‘plotrix’, ‘TMB’, ‘MatrixModels’, ‘rgdal’, ‘TMBhelper’, ‘abind’, ‘corpcor’, ‘pander’, ‘rnaturalearth’, ‘rnaturalearthdata’, ‘formatR’, ‘splancs’, ‘DHARMa’, ‘viridisLite’ are not available for package ‘FishStatsUtils’
install.packages("mapproj")
install.packages("maptools")
install.packages("deldir")
install.packages("RANN")
install.packages("colorspace")
install.packages("RandomFields")
install.packages("RandomFieldsUtils")
install.packages("shape")
install.packages("mixtools")
install.packages("sp")
install.packages("plotKML")
install.packages("plotrix")
#####
install.packages("TMB")
#install_version("TMB", "1.7.18")  #Previous VAST version had conflict with TMB 1.7.19; TMB version 1.7.18 is needed. TMB 1.7.19 causes problems
install.packages("MatrixModels")
install.packages("rgdal")
install.packages("TMBhelper")
install.packages("abind")
install.packages("corpcor")
install.packages("pander")
install.packages("rnaturalearth")
install.packages("rnaturalearthdata")
install.packages("formatR")
install.packages("splancs")
install.packages("viridisLite")
install.packages("effects")
install.packages("INLA")
install.packages("PBSmapping")
install.packages("mapdata")
install.packages("DHARMa")

# TMB helper - download from https://github.com/kaskr/TMB_contrib_R and install just TMBhelper
devtools::install_local(path="dir/TMB_contrib_R-master/TMBhelper", dependencies=FALSE)
devtools::install_local(path="//Users/janellemorano/Downloads/TMB_contrib_R-master 3/TMBhelper", dependencies = FALSE)

# geostatistical_delta-GLMM-master.zip - download from https://github.com/nwfsc-assess/geostatistical_delta-GLMM
devtools::install_local(path="dir/geostatistical_delta-GLMM-master.zip", dependencies=FALSE)

# VAST 3.7.1
# VAST-main.zip - download from https://github.com/James-Thorson-NOAA/VAST/releases
devtools::install_local(path="dir/VAST-3.7.1.zip", dependencies=FALSE)
# where "dir" was the path of the zip
# Now RESTART R!!!!
library(VAST)
packageVersion("VAST") #3.7.1
# check Matrix and TMB versions
# There's a possibility that the installed Matrix version is 1.3.2, but it should be 1.2.8. If so, not sure when that version is installed.
# If necessary, fix it, but RESTART R again!!!!
install_version("Matrix", version = "1.2.8")
packageVersion("Matrix") #1.2.8
packageVersion("TMB") #1.7.18

library(VAST)
# Now test that this all works by running the index standardization model https://github.com/James-Thorson-NOAA/VAST/wiki/Index-standardization


####
# Updated VAST November 2021
#/Users/janellemorano/Downloads/VAST-3.8.1.zip
remotes::install_github("james-thorson/VAST", INSTALL_opts="--no-staged-install")

devtools::install_local(path="/Users/janellemorano/Downloads/VAST-3.8.1.zip", dependencies=FALSE, force = TRUE)
remotes::install_local("/Users/janellemorano/Downloads/VAST-3.8.1", force=T, dependencies=FALSE)


devtools::install_github("james-thorson/vast", INSTALL_opts=c("--no-multiarch --no-test-load"))

#errors with Matrix, install newer version
remotes::install_version("Matrix", version = "1.3.4")

#terra
devtools::install_local(path="/Users/janellemorano/Downloads/terra-master.zip", dependencies = FALSE)


### Updated VAST July 13, 2022
remotes::install_github("james-thorson/VAST", INSTALL_opts="--no-staged-install")
remove.packages("TMB")
remove.packages("Matrix")
install.packages("TMB")

# and then when there's the inevitable mismatch between versions, install by hand...(but uninstall first and restart R)
# Install VAST version downloaded as zip
devtools::install_local(path="/Users/janellemorano/Downloads/VAST-3.9.0 (1).zip", dependencies = FALSE)
install_version("TMB", version = "1.8.1")

# Installed sdmTMB and screwed up everything (August 2022)
# Working version of INLA and VAST disappeared and previous versions of INLA that are available are no longer compatible
# Got stuck upgrading to R 4.2 and nothing works

install.packages("INLA",repos=c(getOption("repos"),INLA="https://inla.r-inla-download.org/R/stable"), dep=TRUE)

# Reverted back to R 4.1.3, INLA 21.02.23, Matrix 1.4.0 and all good
#INLA_21.02.23
remotes::install_version("INLA", version="21.02.23",repos=c(getOption("repos"),INLA="https://inla.r-inla-download.org/R/testing"), dep=TRUE)


devtools::install_local(path="/Users/janellemorano/Downloads/VAST-3.9.1.zip", dependencies = TRUE)
