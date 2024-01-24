# How to add covariates
# following https://github.com/James-Thorson-NOAA/VAST/wiki/Specify-covariates-and-visualize-responses

# Load packages
library(VAST)
library(splines)  # Used to include basis-splines
library(effects)  # Used to visualize covariate effects

# load data set
# see `?load_example` for list of stocks with example data
# that are installed automatically with `FishStatsUtils`.
example = load_example( data_set="covariate_example" )

# Make settings (turning off bias.correct to save time for example)
settings = make_settings( n_x=100,
                          Region=example$Region,
                          purpose="index2",
                          use_anisotropy=FALSE,
                          bias.correct=FALSE,
                          fine_scale=TRUE )

# Define formula.
# In this case I'm demonstrating how to use a basis-spline with
# three degrees of freedom to model a nonlinear effect of log-transformed bottom depth,
# based on example developed by Nicholas Ducharme-Barth.
X1_formula = ~ bs( log(BOT_DEPTH), degree=2, intercept=FALSE)
#X1_formula = ~ poly( log(BOT_DEPTH), degree=2 )
# I'm also showing how to construct an interaction
X2_formula = ~ poly(log(BOT_DEPTH), degree=2) + poly( BOT_TEMP, degree=2 )

# If all covariates as "static" (not changing among years),
#  then set Year = NA to cause values to be duplicated internally for all values of Year
# If using a mix of static and dynamic covariates,
#  then duplicate rows for static covariates for every value of Year
# Here, all covariates are static, so I'm using the first approach.
example$covariate_data[,'Year'] = NA

# Rescale covariates being used to have an SD >0.1 and <10 (for numerical stability)
example$covariate_data[,'BOT_DEPTH'] = example$covariate_data[,'BOT_DEPTH'] / 100

# Run model
fit = fit_model( "settings" = settings,
                 Lat_i = example$sampling_data[,'Lat'],
                 Lon_i = example$sampling_data[,'Lon'],
                 t_i = example$sampling_data[,'Year'],
                 b_i = example$sampling_data[,'Catch_KG'],
                 a_i = example$sampling_data[,'AreaSwept_km2'],
                 X1_formula = X1_formula,
                 X2_formula = X2_formula,
                 covariate_data = example$covariate_data )

# Standard plots
setwd("/Users/janellemorano/Git/VAST output_DONOTGIT/_currentrun")
plot( fit,
      plot_set=c(3,11,12),
      TmbData=fit$data_list )
