# last run Mar 4, 2021
# Mac OS Mojave 10.14.6
# R 4.0.4
# RStudio 1.4.1106

# Install and load devtools package
# install.packages("devtools")
library("devtools")

# Install package
install_github("james-thorson/VAST", INSTALL_opts="--no-staged-install")
# Load package
library(VAST)

# ^^^didn't work, so try this
# Install TMB from CRAN
install.packages("TMB")
# Install INLA using currently recommended method
install.packages("INLA", repos=c(getOption("repos"), INLA="https://inla.r-inla-download.org/R/stable"), dep=TRUE)
# Install FishStatsUtils from CRAN
install_github("james-thorson/FishStatsUtils", INSTALL_opts="--no-staged-install")
#got errors with package 'farver' and so had to decline downloading from source, then it worked
