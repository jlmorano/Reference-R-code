#Simulating Data: How to

## Simulate Data

#First, I'm creating 20x20 grid with x and y coordinates, where the points are evenly spaced from each other.

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