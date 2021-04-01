# last run Mar 30, 2021
# previously ran under R 4.0.4 and it had tons of problems. Switched back to R 3.6
sessionInfo()
# R version 3.6.3 (2020-02-29)
# Platform: x86_64-apple-darwin15.6.0 (64-bit)
# Running under: macOS Mojave 10.14.6

# Install and load devtools package
# install.packages("devtools")
library("devtools")

# Standard recommended way to install didn't work. Skip ahead
# # Install package
# install_github("james-thorson/VAST", INSTALL_opts="--no-staged-install")
# # Load package
# library(VAST)


# Install TMB from CRAN
install.packages("TMB")
# Install INLA using currently recommended method
install.packages("INLA", repos=c(getOption("repos"), INLA="https://inla.r-inla-download.org/R/stable"), dep=TRUE)
# Install FishStatsUtils from CRAN
devtools::install_github("james-thorson/FishStatsUtils", INSTALL_opts="--no-staged-install")
#got errors 

#downloaded FishStatsUtil as zip
devtools::install_local("/Users/janellemorano/Downloads/FishStatsUtils-main.zip")
