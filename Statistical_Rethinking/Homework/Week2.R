# Statistical Rethinking
# Week 2

library(rethinking)
data(Howell1)

d <- Howell1
# filter data of adults age 18+
d2 <- d[ d$age >= 18 , ]
# average height
Hbar <- mean(d2$height)
dat <- list(W=d2$weight,H=d2$height,Hbar=Hbar)

# Linear Regression of weight based on height
m1 <- quap(
  alist(
    W ~ dnorm( mu , sigma ) ,
    mu <- a + b*( H - Hbar ) ,
    a ~ dnorm( 60 , 10 ) ,
    b ~ dlnorm( 0 , 1 ) ,
    sigma ~ dunif( 0 , 10 )
  ) , data=dat )

# Now predict the weight based on the linear regression from the above dataset for these heights
# list of heights and average height 
dat2 <- list( H=c(140,160,175,100) , Hbar=Hbar )
# Get posterior simulations of sigma and beta from the m1 object
h_sim <- sim( m1 , data=dat2 )
# Take the simulated posteriors (h_sim), find the mean (mean), on the columns (2) of h_sim
Ew <- apply(h_sim,2,mean)
h_ci <- apply(h_sim,2,PI,prob=0.89)

datr <- cbind( H=c(140,160,175) , Ew , L89=h_ci[1,] , U89=h_ci[2,] )
round(datr,1)

# 4 

# height of 26 boys measured over 9 periods
data(Oxboys)
d <- Oxboys

# Add delta column
d$delta <- NA
# Calculate the change (delta) in height between each occasion/period
for ( i in 1:nrow(d) ) {
  if ( d$Occasion[i] > 1 ) d$delta[i] <- d$height[i] - d$height[i-1]
}
# Keep/check? for missing values
d <- d[ !is.na(d$delta) , ]

# simulation from priors
n <- 1e3
alpha <- rnorm(n,0,0.1)
sigma <- rexp(n,3)
delta_sim <- rlnorm(n,alpha,sigma)
dens(delta_sim)

# the model
m4 <- quap(
  alist(
    delta ~ dlnorm( alpha , sigma ),
    alpha ~ dnorm( 0 , 0.1 ),
    sigma ~ dexp( 3 )
  ), data=list(delta=d$delta) )

# compute posterior sum of 8 increments
post <- extract.samples(m4)

dsim <- rlnorm(1e3,post$alpha,post$sigma)
dens(dsim)

inc_sum <- sapply( 1:1000 , function(s) sum(rlnorm(8,post$alpha[s],post$sigma[s])) )
dens(inc_sum)