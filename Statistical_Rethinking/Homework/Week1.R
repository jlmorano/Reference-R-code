# Statistical Rethinking
# Week 1

##### 1. Grid method
# 11 land, 4 water, assuming the globe is covered in 70% water
# W <- 4
# L <- 11
# N = 15
# p <- 0.7

# define grid (list of possible explanations)
p_grid <- seq(from=0, to=1, length.out=100)

# define prior (probability of each value of p)
prior <- rep(1,100)

# compute likelihood at each value in grid (probability of the data)
likelihood <- dbinom(4, size = 15, prob = p_grid) #dbinom( number of water, size = number of tosses)
# Posterior distribution
# likelihood <- dbinom( W, W+L, p_grid )

# compute product of likelihood and prior (the posterior, unstandardized)
unstd.posterior <- likelihood * prior

# standardize the posterior, so it sums to 1
posterior <- unstd.posterior / sum(unstd.posterior)

# Plot
plot(p_grid, posterior, type="b",
     xlab="probability of water",
     ylab="posterior probability")
mtext("Question #1")


##### 2. Grid method, different data and prior
# 2 land, 4 water, assuming the globe is covered mostly in water
# W <- 4
# L <- 2
# N = 6
# p <- ifelse(p_grid <0.5, 0, 1)

# define grid (list of possible explanations)
p_grid <- seq(from=0, to=1, length.out=1000)

# define prior (probability of each value of p)
prior <- c( rep( 0 , 500 ) , rep( 2 , 500 ) ) #correct answer
# prior <- ifelse(p_grid <0.5, 0, 1) # I had

# compute likelihood at each value in grid (probability of the data)
likelihood <- dbinom(4, size = 6, prob = p_grid) #dbinom( number of water, size = number of tosses)
# Posterior distribution
# likelihood <- dbinom( W, W+L, p_grid )

# compute product of likelihood and prior (the posterior, unstandardized)
unstd.posterior <- likelihood * prior

# standardize the posterior, so it sums to 1
posterior <- unstd.posterior / sum(unstd.posterior)

# Plot
plot(p_grid, posterior, type="b",
     xlab="probability of water",
     ylab="posterior probability")
mtext("Question #2")


# Alt with tidyverse
library(tidyverse)
data <- tibble(toss = c("w", "w", "w", "w", "l", "l"))

# how many grid points would you like?
n <- 1000
n_success <- 4
n_trials  <- 6

(
  data <-
    tibble(p_grid = seq(from = 0, to = 1, length.out = n),
           # note we're still using a flat uniform prior
           prior  = ifelse(p_grid <0.5, 0, 1)) %>% 
    mutate(likelihood = dbinom(n_success, size = n_trials, prob = p_grid)) %>% 
    mutate(posterior = (likelihood * prior) / sum(likelihood * prior))
)

ggplot(data, aes(x = p_grid, y = posterior)) +
  geom_point() +
  scale_y_continuous("posterior probability") +
  xlab("probability of water") +
  ggtitle("Question 2 tidyverse alternative")

##### 3. Compute 89% percentile and HPDI intervals
# Use posterior distribution from #2
library(rethinking)
samples <- sample( p_grid , prob=posterior , size=1e4 , replace=TRUE )
plot( samples , ylim=c(0,1) , xlab="samples" , ylab="proportion water" )
PI( samples , 0.89 )
HPDI( samples , 0.89)

# continue with tidyverse

# how many samples would you like?
n_samples <- 1e4

# make it reproducible
set.seed(3)

samples <-
  data %>% 
  slice_sample(n = n_samples, weight_by = posterior, replace = T)

glimpse(samples)

samples %>% 
  mutate(sample_number = 1:n()) %>% 
  ggplot(aes(x = sample_number, y = p_grid)) +
  geom_point(alpha = 1/10) +
  scale_y_continuous("proportion of water (p)", limits = c(0, 1)) +
  xlab("sample number")

samples %>% 
  ggplot(aes(x = p_grid)) +
  geom_density(fill = "black") +
  scale_x_continuous("proportion of water (p)", limits = c(0, 1))

# Compute 89% percentile
samples <- sample(p_grid, prob=posterior, size = 10000, replace=TRUE)
plot(samples)


##### 4. 
# rnorm is the random sample of a normal distribution
# pnorm estimate of the probability density
N <- 1e5
trueW <- rbinom(N,size=20,prob=0.7)
obsW <- rbinom(N,size=trueW,prob=0.8)

# or as single sample
W <- rbinom(N,size=20,prob=0.7*0.8)

mean(obsW/20)
mean(W/20)

# now analyze
# Pr(p|W,N) = Pr(W|p,N)Pr(p) / Z
# Pr(W|N,p) = Pr(W)Pr(W|W)

W <- rbinom(1,size=20,prob=0.7*0.8)
grid_p <- seq(from=0,to=1,len=100)
pr_p <- dbeta(grid_p,1,1)
prW <- dbinom(W,20,grid_p*0.8)
post <- prW*pr_p

post_bad <- dbinom(W,20,grid_p)

# Plot
plot(p_grid, post, type="l",
     xlab="proportion water",
     ylab="plausibility")
mtext("Question #4")



