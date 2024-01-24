# Ch2-Building-A-Model

# 6 land, 9 water, assuming the globe is covered in 70% water
W <- 9
L <- 6
# N = 15
p <- 0.7

# define prior (probability of each value of p)
prior <- rep(1,100)
# prior <- ifelse(p_grid <0.5, 0, 1)
# prior <- exp(-5*abs(p_grid - 0.5))
# prior <- dbeta(p_grid, 3, 1)

# compute likelihood at each value in grid (probability of the data)
likelihood <- dbinom(6, size = 9, prob = p_grid) #dbinom( number of water, size = number of tosses)
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
mtext("100 points")