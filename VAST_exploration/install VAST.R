# last used Aug 19, 2020

# Install VAST
# Mac OS Mojave 10.14.6
# R 3.6
# RStudio 1.3.1056

# last run Mar 4, 2021
# Mac OS Mojave 10.14.6
# R 4.0.4
# RStudio 1.3.1056

install.packages("devtools")
require(devtools)
install.packages("Matrix")
require(Matrix)

## Uninstall Development Toolchain
# following https://thecoatlessprofessor.com/programming/r/uninstalling-the-r-development-toolchain-on-macos/
# Followed steps in Terminal, verifying clang7, ~/.R/Makevars, and~/.Renviron are completely removed

## Install Development Toolchain
# following https://thecoatlessprofessor.com/programming/cpp/r-compiler-tools-for-rcpp-on-macos/
# Install gfortran8.2
# Install Rcpp, RcppArmadillo
# -->>Compile test works, but with this error:
# >ld: warning: directory not found for option '-L/usr/local/gfortran/lib/gcc/x86_64-apple-darwin15/6.1.0'
# Option: install Xcode 13.
# -->>Cannot do this because not compatible with OS 10.14.6 and same issue with older version available
# https://mac.r-project.org/tools/

install.packages("TMB")
# select option 1 (update all packages), if prompted
require(TMB)

# Follow instructions https://github.com/NOAA-EDAB/Rpath/issues/1
# In terminal run:
#   /usr/bin/ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"
# In terminal run (g++ then installed in /usr/local/bin/g++ in default):
#   brew install gcc
# Make "Makevars" file in ~/.R/
#   Write in Makevars
# CC = gcc
# CXX = /usr/local/bin/g++
#   In R run:
#   devtools::install_github('NOAA-EDAB/Rpath', build_vignettes = TRUE)

# Replace ~/.R/Makevars contents
# In terminal
# >cd ~/.R
# >ls
# >open -e Makevars
# Makevars doesn't exist, so create it
# >touch Makevars
# add text below and save
# CC=gcc
# CXX=/usr/local/gfortran/bin/g++
#   CXX=/usr/local/bin/g++-9
#   CXX1X=/usr/local/clang7/bin/clang++
#   CXX98=/usr/local/clang7/bin/clang++
#   CXX11=/usr/local/clang7/bin/clang++
#   CXX14=/usr/local/clang7/bin/clang++
#   CXX17=/usr/local/clang7/bin/clang++
#   LDFLAGS=-L/usr/local/clang7/lib

# Test TMB following code from: https://github.com/nwfsc-assess/geostatistical_delta-GLMM/wiki/Steps-to-install-TMB.
Version = "linear_mixed_model"

# Simulate data for a linear mixed model with random intercepts:
set.seed(1)
Factor = rep(1:10, each=10)
Z = rnorm(length(unique(Factor)), mean=0, sd=1)

X0 = 0
Y = Z[Factor] + X0 + rnorm( length(Factor), mean=0, sd=1)

# Download CPP file:
setwd(tempdir())
download.file( url="https://raw.githubusercontent.com/James-Thorson/mixed-effects/master/linear_mixed_model/linear_mixed_model.cpp", destfile="linear_mixed_model.cpp", method="auto")
compile( paste0(Version,".cpp") )

# Generate inputs for TMB:
Data = list("n_data"=length(Y), "n_factors"=length(unique(Factor)), "Factor"=Factor-1, "Y"=Y)
Parameters = list("X0"=-10, "log_SD0"=2, "log_SDZ"=2, "Z"=rep(0,Data$n_factor))
Random = c("Z")

# Build TMB object:
dyn.load(dynlib(Version))
Obj = MakeADFun(data=Data, parameters=Parameters, random=Random)  #

# Check that TMB is working properly:
Obj$fn(Obj$par)
# This should return 313.4137.

# Install VAST dependency, FishStatsUtils:
install_github("james-thorson/FishStatsUtils", INSTALL_opts="--no-staged-install")
# other option
install_github("james-thorson/FishStatsUtils")
require(FishStatsUtils)

# Install VAST dependency, INLA:
#install.packages("INLA", repos=c(getOption("repos"), INLA="https://inla.r-inla-download.org/R/stable"), dep=TRUE)
require(INLA)

#Install the latest version of VAST, downloaded from github: https://github.com/James-Thorson-NOAA/VAST

devtools::install_local("~/Downloads/VAST-master")
# -->>FAILS
# NB: this package now depends on R (>= 3.5.0)
# WARNING: Added dependency on R >= 3.5.0 because serialized objects in  serialize/load version 3 cannot be read in older versions of R.  File(s) containing such objects:  'VAST/inst/extdata/GOA_MICE_example/settings.RData'  WARNING: Added dependency on R >= 3.5.0 because serialized objects in  serialize/load version 3 cannot be read in older versions of R.  File(s) containing such objects:  'VAST/inst/extdata/Spatially_varying_coefficient/Data.RData'  WARNING: Added dependency on R >= 3.5.0 because serialized objects in  serialize/load version 3 cannot be read in older versions of R.  File(s) containing such objects:  'VAST/inst/extdata/Spatially_varying_coefficient/Kmeans-100.RData'  WARNING: Added dependency on R >= 3.5.0 because serialized objects in  serialize/load version 3 cannot be read in older versions of R.  File(s) containing such objects:  'VAST/inst/extdata/Spatially_varying_coefficient/saved_estimates.RData'  WARNING: Added dependency on R >= 3.5.0 because serialized objects in  serialize/load version 3 cannot be read in older versions of R.  File(s) containing such objects:  'VAST/inst/extdata/Spatially_varying_coefficient/settings.RData'
# ─  building 'VAST_3.5.1.tar.gz'
# 
# * installing *source* package ‘VAST’ ...
# ** using staged installation
# ** R
# ** inst
# ** byte-compile and prepare package for lazy loading
# Error: (converted from warning) package ‘TMB’ was built under R version 3.6.2
# Execution halted
# ERROR: lazy loading failed for package ‘VAST’
# * removing ‘/Library/Frameworks/R.framework/Versions/3.6/Resources/library/VAST’
# Error: Failed to install 'VAST' from GitHub:
#   (converted from warning) installation of package ‘/var/folders/tc/60rlkl596_5d1xd2f87p5x4r0000gn/T//Rtmpniy328/fileb55c454fe1b9/VAST_3.5.1.tar.gz’ had non-zero exit status

# Also tried this from Issue #147 but with same error
devtools::install_github("james-thorson/vast", INSTALL_opts=c("--no-multiarch --no-test-load"))

require(VAST)
