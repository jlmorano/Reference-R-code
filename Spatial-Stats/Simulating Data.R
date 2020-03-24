# HW#6

## Simulating Data: Simulate a surface that has both trend and covariation
# Trend is B in Y = xB + E
# Covariation is E in E~N(0,E)

# Creating a variance-covariance matrix
# for stochastic simulation
# and adding trend with different levels
# of variation
#
# set alpha to a level associated with 
# an AR(1) autoregressive process where
# Z(t)=alpha*Z(t-1)+e
#
alpha = 0.9  # degree of covariance
sigma = 10  # level of variance
#
# Create an n by n matrix filled with zeros
n  = 20
MM = matrix(0,nrow=n,ncol=n)
#
# Create off-diagonal correlations
# with correlations decreasing 
for(i in seq(n)) {
  MM[row(MM)-col(MM)==i]=i
  MM[col(MM)-row(MM)==i]=i	
}

# Now raise alpha to the power established
# by the matrix
MM = alpha^MM
round(MM,2)
#
# Put 1s along the diagonal
diag(MM)=1
#
# Take a look at the correlation matrix
MM
#
# Use the Cholesky decomposition to
# find the lower triangular matrix LL
LL=t(chol(MM))

# Look at LL to see that it is lower triangular
LL
round(LL,2)
t(round(LL,2))
round(LL %*% t(LL),2)

# Now generate n random normal deviates with
# mean 0 and variance 1. 
w = rnorm(n,0,sigma)
#
# Multiply the random normal deviates by LL to
# get z, an autocorrelated realization
z=LL %*% w
# z is now what?

# Plot the data to see what we have created
# The x is time for plotting
x = seq(n)
#

# This sets up two plots per page
#
par(mfrow=c(2,1))
#
# First plot the uncorrelated data
#
plot(x,w)
#
# Put a smooth line through to guide the eye
#
lines(supsmu(x,w),col=8)
#
# A horizontal line at zero for reference
#
abline(h=0)
#
#
title("Uncorrelated Data")
#
#
# Next plot the correlated data
#
plot(x,z)
lines(supsmu(x,z),col=8)
abline(h=0)
title("Correlated Data")
#
# Go back now to one plot per
# page
#
par(mfrow=c(1,1))
#


########

# 20x20 matrix = 200 points
nn = 200
# Select parameters of the trend
nn.psill = 100
nn.range = 30
nn.nugget = 0
#
# Compute the distances between points
my.dist = abs(matrix(seq(nn),nrow=nn,ncol=nn,byrow=T)-
                matrix(seq(nn),nrow=nn,ncol=nn,byrow=F))

# Creating a Covariance Function
cov.func = function(x){
  variogramLine(vgm(nn.psill, "Sph", nn.range, nn.nugget),dist_vector=c(x),covariance=T)$gamma}
#
my.cov = matrix(cov.func(my.dist),nrow=nn,ncol=nn)

# Decompose using Choleski into lower triangular matrix
my.LMatrix  =  t(chol(my.cov))

# Generate nn MVN(0,I) random variates
my.before.data = rnorm(nn)

# Create nn MVN(0,U'U) random variates
my.after.data = my.LMatrix%*%my.before.data
#
zz = my.after.data
yy = rep(0,nn)
xx = seq(nn)
my.loc = data.frame(xx=xx,yy=yy)
coordinates(my.loc)=c("xx","yy")

plot(xx,zz,pch=15)
#
#

#####################

##### Define surface (geo-referenced points) 
#Create a 20x20 grid with x and y coordinates, where the points are evenly spaced from each other.

# Create a 20x20 matrix; the dimensions of the grid
n = 20
nn = n*n

# Create matrices for calculating distances between points
# building x coordinates
Mx=matrix(seq(n),nrow=n,ncol=n,byrow=T)
# building y coordinates
My=matrix(seq(n),nrow=n,ncol=n,byrow=F)
Vx=c(Mx)
Vy=c(My)
#
# Plot the x, y (Vx, Vy, respectively) of the simulated geographical points
par(pty='s')
plot(Vx,Vy)

# Calculate the pairwise distances between all points
# The absolute distance between each point as the 
# difference between each x, 
MMx=abs(matrix(Vx,nrow=nn,ncol=nn,byrow=F)
        -matrix(Vx,nrow=nn,ncol=nn,byrow=T))
# and each y
MMy=abs(matrix(Vy,nrow=nn,ncol=nn,byrow=F)
        -matrix(Vy,nrow=nn,ncol=nn,byrow=T))
# and then apply the Pythagorian theorem to create the distance matrix between all points
# given 2 points, covariance (h or distance) = sqrt of the sum of the difference between the xs 
# and the difference beween the ys
MMdist=sqrt(MMx^2+MMy^2)
# Take a look at upper lefthand corner of distance matrix
MMdist[1:5,1:5]

# MMdist is now the matrix of the distance between each point in simulated data