# Simple example
# from https://github.com/James-Thorson-NOAA/VAST/wiki/Simple-example
# Updated April 14, 2021

# Example of using VAST with high-level wrapper functions


# Set local working directory (change for your machine)
# setwd( "/Users/janellemorano/Git/Reference-R-scripts/VAST_exploration/simple_model_output" )

# Load package
# library(VAST)
# sessionInfo()
# R version 4.0.5 (2021-03-31)
# Platform: x86_64-apple-darwin17.0 (64-bit)
# Running under: macOS Mojave 10.14.6
# 
# Matrix products: default
# BLAS:   /System/Library/Frameworks/Accelerate.framework/Versions/A/Frameworks/vecLib.framework/Versions/A/libBLAS.dylib
# LAPACK: /Library/Frameworks/R.framework/Versions/4.0/Resources/lib/libRlapack.dylib
# 
# locale:
#   [1] en_US.UTF-8/en_US.UTF-8/en_US.UTF-8/C/en_US.UTF-8/en_US.UTF-8
# 
# attached base packages:
#   [1] stats     graphics  grDevices utils     datasets  methods   base     
# 
# other attached packages:
#   [1] FishStatsUtils_2.9.1 VAST_3.7.1           TMB_1.7.18           devtools_2.4.0      
# [5] usethis_2.0.1       
# 
# loaded via a namespace (and not attached):
#   [1] tidyselect_1.1.0        remotes_2.3.0           purrr_0.3.4            
# [4] sf_0.9-8                splines_4.0.5           lattice_0.20-41        
# [7] rnaturalearth_0.1.0     vctrs_0.3.7             generics_0.1.0         
# [10] testthat_3.0.2          utf8_1.2.1              rlang_0.4.10           
# [13] pkgbuild_1.2.0          e1071_1.7-6             pillar_1.6.0           
# [16] glue_1.4.2              withr_2.4.1             DBI_1.1.1              
# [19] TMBhelper_1.3.0         sp_1.4-5                sessioninfo_1.1.1      
# [22] lifecycle_1.0.0         memoise_2.0.0           callr_3.6.0            
# [25] fastmap_1.1.0           ps_1.6.0                curl_4.3               
# [28] parallel_4.0.5          class_7.3-18            fansi_0.4.2            
# [31] Rcpp_1.0.6              KernSmooth_2.23-18      INLA_21.02.23          
# [34] classInt_0.4-3          cachem_1.0.4            desc_1.3.0             
# [37] pkgload_1.2.1           fs_1.5.0                RANN_2.6.1             
# [40] processx_3.5.1          dplyr_1.0.5             ThorsonUtilities_1.0   
# [43] splancs_2.01-40         grid_4.0.5              rprojroot_2.0.2        
# [46] rgdal_1.5-23            cli_2.4.0               tools_4.0.5            
# [49] magrittr_2.0.1          proxy_0.4-25            tibble_3.1.0           
# [52] pkgconfig_2.0.3         crayon_1.4.1            ellipsis_0.3.1         
# [55] Matrix_1.2-8            prettyunits_1.1.1       rstudioapi_0.13        
# [58] R6_2.5.0                rnaturalearthdata_0.1.0 units_0.7-1            
# [61] compiler_4.0.5 

# load data set
# see `?load_example` for list of stocks with example data 
# that are installed automatically with `FishStatsUtils`. 
# Load package
library(VAST)

# load data set
# see `?load_example` for list of stocks with example data 
# that are installed automatically with `FishStatsUtils`. 
example = load_example( data_set="EBS_pollock" )

# Make settings (turning off bias.correct to save time for example)
settings = make_settings( n_x = 100, 
                          Region = example$Region, 
                          purpose = "index2", 
                          strata.limits = example$strata.limits, 
                          bias.correct = FALSE )

# Run model
fit = fit_model( settings = settings, 
                 Lat_i = example$sampling_data[,'Lat'], 
                 Lon_i = example$sampling_data[,'Lon'], 
                 t_i = example$sampling_data[,'Year'], 
                 c_i = rep(0,nrow(example$sampling_data)), 
                 b_i = example$sampling_data[,'Catch_KG'], 
                 a_i = example$sampling_data[,'AreaSwept_km2'], 
                 v_i = example$sampling_data[,'Vessel'] )

# Plot results
plot( fit )
