# Homework #6

# From ILRS.R

# Steps for Iteratively Reweighted Least Squares
# following Waller and Gotway (2004) Section 9.2.1
#
# 1) Estimate beta (trend)
# 2) Calculate epsilon (residuals)
# 3) Estimate variogram (theta) and covariance matrix Sigma 
# 4) Re-estimate beta using IRLS based on Sigma weights
# 5) Repeat steps 2-4 until estimates of beta and theta converge

library(sp)
library(gstat)
library(lattice)


# 1. Estimate beta (trend)
#####
# First simulate some data with large scale trend and
# small scale variation
#
# Trend first
# Decide size of matrix
size = 20
# Define X matrix, which is going to be 1,2,3 by rows
X = matrix(seq(size),nrow=size,ncol=size,byrow=T)
# Define Y matrix, which is going to be 1,2,3 by columns
Y = matrix(seq(size),nrow=size,ncol=size,byrow=F)
# Define Z0 matrix which is a function of X and Y with constants
Z0 = 1 + 2 * X + 3 * Y
# Play around with the numbers to see how things change

# To plot the surface, turn the matrices into vectors for wireframe to read
x = c(X)
y = c(Y)
z0 = c(Z0)
#
wireframe(z0~x+y,drape=T)
# It's plotting the surface when each coordinate (x,y,z) is a value from each matrix (or vector in this case)
# See how the plotting shifts as you change the values in the Z0 equation
# This surface is flat because there's no variation. We've just created the trend


# 2) Calculate residuals (epsilon)
#####
# Let's add some variation
# Generate some Gaussian random data epsilon ~ N(0,1)
epsilon = rnorm(length(z0),0,20)  # 20 to see it
# Add epsilon (the variance) to z0
z1 = z0+epsilon
#
# This is error (variance) (epsilon) by x and y 
wireframe(epsilon~x+y,drape=T)
# Now this is the error (epsilon) on the x, y, and z surface
wireframe(z1~x+y,drape=T)


# 3) Estimate variogram (theta) and covariance matrix Sigma 
#####
# Now create spatial covariance matrix (MM)
# First specify parameters of variogram
v.psill=8
v.range=10
v.nugget=5
#
#
# Calculate the pairwise distances between points
# Finding the difference between each x and y

# Take the x values (the vectorized matrix of x)
n = length(x)
# Find the absolute difference between each x
# all combinations of x
MMx=abs(matrix(x,nrow=n,ncol=n,byrow=F)
	 -matrix(x,nrow=n,ncol=n,byrow=T))

# Find the absolute difference between each y
# all combinations of y
MMy=abs(matrix(y,nrow=n,ncol=n,byrow=F)
	 -matrix(y,nrow=n,ncol=n,byrow=T))

# Pythagorian theorem to create distance matrix between all points
MMdist=sqrt(MMx^2+MMy^2)
#
# Spherical variogram to get variance-covariance matrix
# First make a function to make computation a little easier
my.cov = function(x){
variogramLine(vgm(v.psill, "Sph", v.range, v.nugget),dist_vector=c(x),covariance=T)$gamma}
# gamma is covariance

# Apply the variogram line to the distance matrix to compute the variance-covariance
# Make Variance-Covariance Matrix
MM = matrix(my.cov(MMdist),nrow=n,ncol=n)

# Apply Choleski decomposition
LL=t(chol(MM))

# Simulate correlated data based on variance-covariance matrix
zeta=LL%*%rnorm(n,0,1)
#
wireframe(zeta~x+y,drape=T)

#
z2 = z0 + zeta
#
wireframe(z2~x+y,drape=T)

# Now we have data that we've created

# Pretending we didn't know the orgins of the data, we'll now determine the trend and residuals
#####
# Step 1, Estimate beta
#
summary(lm(z2~x+y))
#
# Now using matrix notation
#
X1 = cbind(rep(1,length(x)),x,y)
#
(beta1 = solve(t(X1)%*%X1)%*%t(X1)%*%z2)
#
# Step 2, Calculate residuals
#
resid1 = resid(lm(z2~x+y))
resid1 = z2 - X1 %*% beta1
#
# Step 3, Estimate variogram and covariance matrix
#         theta = psill, range, nugget
#
varsim2d = data.frame(
	x=x,
	y=y,
	z=resid1)  # also try z2 in place of resid1
#
coordinates(varsim2d)=c("x","y")
#
# Variogram estimation
#
my.vario=variogram(z~1,data=varsim2d)
#
#
# Plot the empirical variogram
#
plot(gamma~dist,data=my.vario)
#
#
# Fit variogram model
# Here I could do this by eye, but I am
# going to cheat a bit and use the true
# values as starting parameters.
#
#
my.fit=fit.variogram(my.vario,
	vgm(model="Sph",psill=v.psill,range=v.range,nugget=v.nugget),
	fit.method=1)
#
my.fit
my.psill=my.fit$psill[2]
my.range=my.fit$range[2]
my.nugget=my.fit$psill[1]
my.max = max(my.vario$dist)
#
# Add the true variogram model line 
# to the empirical variogram 
#	
lines(variogramLine(vgm(my.psill, "Sph", my.range, my.nugget),maxdist=my.max),type="l",lwd=2,col=9)
#
#
# Covariance matrix from empirical variogram
# but using the same distance matrix from above
my.cov = function(x){
variogramLine(vgm(my.psill, "Sph", my.range, my.nugget),dist_vector=c(x),covariance=T)$gamma}
#
# Make Variance-Covariance Matrix
#
Sigma = matrix(my.cov(MMdist),nrow=n,ncol=n)


# Step 4, Re-estimate beta using Sigma and IRLS
######
(beta2 = solve(t(X1)%*%solve(Sigma)%*%X1)%*%t(X1)%*%solve(Sigma)%*%z2)
beta1


# Step 5, Repeat steps 2-4 until convergence
#####
