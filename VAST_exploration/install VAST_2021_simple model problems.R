# I previously had VAST working, and today I updated VAST and R and then tested with the simple model example.

# R version 4.0.4 (2021-02-15)
# Platform: x86_64-apple-darwin17.0 (64-bit)
# Running under: macOS Mojave 10.14.6
# FishStatsUtils_2.8.0 
# VAST_3.6.1           
# TMB_1.7.19  

# The model ran for a while, but then ended with this error:
# Error in optimHess(parameter_estimates$par, fn = fn, gr = gr) : 
  # gradient in optim evaluated to length 1 not 75
# In addition: Warning messages:
# 1: In sparseMatrix(i = index.i, j = index.j, x = Aij, dims = c(mesh$n,  :
  # 'giveCsparse' has been deprecated; setting 'repr = "T"' for you
# 2: In sparseMatrix(i = index.i, j = index.j, x = Aij, dims = c(mesh$n,  :
  # 'giveCsparse' has been deprecated; setting 'repr = "T"' for you
# 3: In sparseMatrix(i = index.i, j = index.j, x = Aij, dims = c(mesh$n,  :
  # 'giveCsparse' has been deprecated; setting 'repr = "T"' for you
# Previously reported problem with Matrix and TMB, so tried this

library(devtools)
install_version("Matrix", version = "1.2.8")
# Got this warning while installing...
# ld: warning: directory not found for option '-L/usr/local/clang7/lib'
# installing to /Library/Frameworks/R.framework/Versions/4.0/Resources/library/00LOCK-Matrix/00new/Matrix/libs
library(Matrix)
install_version("TMB", "1.7.18")
# Now VAST and FishStatsUtils, do NOT update TMB
devtools::install_github("James-Thorson-NOAA/FishStatsUtils", ref = "development", force = TRUE)
#Installed while skipping all updates
devtools::install_github("James-Thorson-NOAA/VAST", ref = "development", force = TRUE)
library(VAST)
library(FishStatsUtils)




