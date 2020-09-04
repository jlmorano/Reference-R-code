# INLA Example 1.3
# from https://becarioprecario.bitbucket.io/spde-gitbook/ch-INLA.html#sec:INLAexample

library(INLA)
data(SPDEtoy)
head(SPDEtoy)
# variables:
# y	= Simulated observations at the locations.
# s1 =	x-coordinate in the unit square.
# s2 =	y-coordinate in the unit square.

dim(SPDEtoy)
# 200   3

## 1.3 Simple linear model
# Create a dataframe from this data
SPDEtoy.sp <- SPDEtoy
# Assign coordinates of spatial location of data
coordinates(SPDEtoy.sp) <- ~ s1 + s2
# Graph it
bubble(SPDEtoy.sp, "y", key.entries = c(5, 7.5, 10, 12.5, 15), 
       maxsize = 2, xlab = "s1", ylab = "s2")
# Clear trend on the data with more measurements in bottom left corner

# The first model we fit to the SPDEtoy dataset with INLA is a linear regression on the coordinates.
# Observations (yi) at location (si) is assumed to be distributed as Gaussian, with mean mu,i and precision tau
# The mean mu, i is assumed to be equal to alpha + Beta1s1i + Beta2s2i, where alpha is model intercept
# and Beta are coefficients of the covariates
# Default is the prior on intercept is uniform distribution, prior on coefficients also Gaussian with zero mean and precision 0.001, prior on the precision Tau is a Gamma with parameters 1 and 0.00005.

# Note, models in this section are not reasonable spatial models because they lack rotation invariance of the coordinate system. But these models are for illustrating how INLA works.

# Linear model fit to SPDEtoy data
m0 <- inla(y ~ s1 + s2, data = SPDEtoy)
summary(m0)

# The output provides a summary of the posterior marginals of the intercept, coefficients of the covariates and the precision of the error term. 
# Note that the results provided in the summary of the model and the posterior marginals in Figure 1.2 both suggest that the value of the response decreases as the value of the y-coordinate s2 increases.


## 1.3.1 Simple non-linear model
# Assign random effects with f() function with formula that defines the model.
# Here, defining the model with smooth terms on each covariate (s1, s2)
# rw1 = random walk of order one
# scale.model = TRUE makes the model to be scaled to have an average variance of 1
# A complete list with the names of the implemented models can be obtained with: 
# names(inla.models()$latent), or 
# inla.list.models("latent")
f.rw1 <- y ~ f(s1, model = "rw1", scale.model = TRUE) +
  f(s2, model = "rw1", scale.model = TRUE)

# Fit model with smooth terms assigned to covariates
m1 <- inla(f.rw1, data = SPDEtoy)
summary(m1)

## 1.3.2 INLA objects

# inla()
# summary.fixed	Summary of fixed effects.
# marginals.fixed	List of marginals of fixed effects.
# summary.random	Summary of random effects.
# marginals.random	List of marginals of random effects.
# summary.hyperpar	Summary of hyperparameters.
# marginals.hyperpar	List of marginals of the hyperparameters.
# mlik	Marginal log-likelihood.
# summary.linear.predictor	Summary of linear predictors.
# marginals.linear.predictor	List of marginals of linear predictors.
# summary.fitted.values	Summary of fitted values.
# marginals.fitted.values	List of marginals of fitted values.

m0$summary.fixed
m1$summary.fixed

# The posterior marginal of the intercept could be plotted using:
  
plot(m0$marginals.fixed[[1]], type = "l", xlab = expression(alpha), ylab = "density")

## 1.3.3 Prediction for NA values at points

# Add a line with NA value at coordinate
SPDEtoy.pred <- rbind(SPDEtoy, c(NA, 0.5, 0.5))

# Next, the model will be fitted to the newly created SPDEtoy.pred dataset, which contains 201 observations. Note that option compute = TRUE needs to be set in control.predictor in order to compute the posterior marginals of the fitted values. The model with fixed effects on the covariates will be used:
m0.pred <- inla(y ~ s1 + s2, data = SPDEtoy.pred,control.predictor = list(compute = TRUE))

# The results now provide the predictive distribution for the missing observation (in the 201st position of the list of marginals of the fitted values):
m0.pred$marginals.fitted.values[[201]]

## 1.4 Additional arguments and control options
# Skipped for now

# 1.4.1 Estimation method
# For example, we could compute the model with non-linear random effects using two different integration strategies and then compare the computation times
m1.ccd <- inla(f.rw1, data = SPDEtoy,
               control.compute = list(dic = TRUE, cpo = TRUE, waic = TRUE),
               control.inla = list(int.strategy = "ccd"))
m1.eb <- inla(f.rw1, data = SPDEtoy,
              control.compute = list(dic = TRUE, cpo = TRUE, waic = TRUE),
              control.inla = list(int.strategy = "eb"))


#
#
#
## 2.6.3 Meshes for toy example
coords <- as.matrix(SPDEtoy[, 1:2])
p5 <- coords[1:5, ]
m1 <- inla.mesh.2d(p5, max.edge = c(0.5, 0.5)) 
plot(m1)
mesh1 <- inla.mesh.2d(coords, max.edge = c(0.035, 0.1)) 
mesh2 <- inla.mesh.2d(coords, max.edge = c(0.15, 0.2)) 
plot(mesh1)
plot(mesh2)


## 2.8.2 Model and covariate selection
