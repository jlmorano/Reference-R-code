# Generative Simulation Model (Lec 9)
# You should be able to make a generative model of your system before you attempt the statistical model with your data or else you don't really understand your theory.

# This example is to determine of there is bias towards a gender in admission to UCBerkeley
# where Gender (G) -> Department (D) -> Admission (A)
# AND G -> A

library(rethinking)
# Number of applicants
N <- 1000

# Gender distribution: equal between gender 1 and gender 2, stochastic 
G <- sample( 1:2, size = N, replace = TRUE)

# D after G because D is a function of G
# gender 1 tends to apply to departments 1, 2 to 2
D<- rbern(N, ifelse( G ==1, 0.3, 0.8 )) + 1

# matrix of acceptance rates [dept, gender]
accept_rate <- matrix( c(0.1, 0.3, 0.1, 0.3), nrow=2)

# Simulate acceptance
A <- rbern(N, accept_rate[D,G])

# Application rate for each gender
table(G,D)

# Acceptance rate for each gender by department
table(G,A)

# The acceptance rate looks about the same no matter the path: application rate differences or admission rate differences, which means we need to use the backdoor method to deduce.

# Prior Predictive Simulation
# a <- rnorm(1e4, 0, 10)
a <- norm(1e4, 0, 1.5)
# b <- rnorm(1e4, 0, 10)
b <- rnorm(1e4, 0, 0.5)

xseq <- seq(from=-3, to =3, len=100)
p <- sapply(xseq, function(x) inv_logit(a+b*x))

plot (NULL, 
      xlim = c(-2.5, 2.5), 
      ylim = c(0,1),
      xlab = "x value", 
      ylab = "probability")
for ( i in 1:10) lines ( xseq, p[i,], lwd=3, col=2)
