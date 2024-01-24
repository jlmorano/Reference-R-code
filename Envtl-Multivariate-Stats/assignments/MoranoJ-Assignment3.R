# Assignment #3

#In this exercise, you will explore and model how annual precipitation totals covary across 4 rain gages along the southern shore of Lake Ontario in New York State. Annual precipitation in inches between 1980 and 2010 for each of these gages is provided in the file “Ontario.prcp” on Canvas under the folder Assignment 3. Note that some of these data are missing.

#### 1. Assess Normality ####
#############################

#### a) Create a matrix (annual.prcp) only containing the precipitation data at the four gages (not the years). Convert this matrix from a data.frame object (what read.table() returns) into adata.matrix  object. Data matrices are better suited for matrix multiplication.
annual.prcp.df <- read.table("/Users/janellemorano/Git/Reference-R-scripts/Envtl-Multivariate-Stats/assignments/Ontario.prcp.txt",  header = TRUE)
annual.prcp <- as.matrix(annual.prcp.df[,2:5])

# Plot annual precipitation over time for understanding of trends
library(tidyverse)
annual.prcp.long <- gather(annual.prcp.df, Gage, Annual.prcp, Gage1:Gage4, factor_key = TRUE)

library(viridis)
ggplot(annual.prcp.long, aes(x = Year, y = Annual.prcp, colour = Gage, group = Gage)) +
  geom_line() +
  theme_bw() +
  scale_color_viridis(discrete=TRUE)

#### Check Marginal Distributions ####

##### b) Check marginal distributions
par(mfrow = c(2, 2))
for (i in 1:4) {
  # Sort data
  x <- sort(annual.prcp[,i], na.last = NA)
  # Calculate pi=1/(n+1)
  pi <- c((1:length(x))/ (length(x) + 1))
  # Find mean and standard deviation of data with pnorm()
  epi <- pnorm(x, mean(x), sd(x))
  # Plot probability plots (P-P) of the data for all four gages
  plot(epi,pi,
       main= paste0("Gage ", i))
  abline(a = 0,                                        
         b = 1,
         col = "red",
         lwd = 2)
}
par(mfrow = c(1, 1))

#### Check Multivariate Structure ####

#### c) Calculate the covariance matrix of the precipitation data matrix using the cov() function
S <- cov(annual.prcp, use= "pairwise.complete")
# Calculate mean vector mu
mu <- apply(annual.prcp, MARGIN = 2, FUN = mean, na.rm = TRUE)

#### d) Manahalobis distance between each of 31 observations and the mean
# D2 = ((xi-mu)^T) * S^-1 * (x-mu)
# D2 <-  t(xi - mu) %*% solve(S) %*% (xi - mu)
# xi is a row of observations across each of 4 gages

rows = nrow(annual.prcp) #number of observations = number of rows
D.sq <- c()

for (i in 1:rows) {
  # subset data for each row
  xi <- unname(annual.prcp[i,])
  # calculate Dsq for each row, where mu is defined above
  D2 <- t(xi - mu) %*% solve(S) %*% (xi - mu)
  # Export data
  D.sq[i] = D2
}

#### e) P-P plot for D.sq (empirical/model-free non-exceedence probabilities (NEP) vs. analytical/model-based NEP.

# 1) Fit normal dist to data, ie find mean and sd
D.sq.mean = mean(D.sq)
D.sq.sd = sd(D.sq)
df = ncol(annual.prcp) #should be 4

# 2) Sort the data, use sort() function to order a vector of data from smallest to largest, and set the argument na.last=NA to drop the NAs
D.sq = sort(D.sq, na.last = NA)

# 3) Calculate analytical/model-based NEP
NEP.analytical.D.sq <- pchisq(D.sq, df)

# 4) Calculate empirical/model-free NEP
NEP.empirical.D.sq <- (1: length(D.sq)) / (length(D.sq) + 1)

# 5) Plot empirical v analytical
plot(NEP.analytical.D.sq, NEP.empirical.D.sq,
     xlim = c(0, 1),
     ylim = c(0, 1),
     xlab = "Analytical NEP",
     ylab = 'Empirical NEP',
     main = 'P-P Plot')
abline(a = 0,                                        
       b = 1,
       col = "red",
       lwd = 2)

#### 2) Gap-Filling Missing Data (Imputation) ####

#### g) Use conditional normality for MVN variable to estimate precip data for each year and gage with missing data using all available obs that year.

# Steps to find Expected Value
# E = mu.na + S12 * (S22)^-1 * (x.obs - mu.obs)
# 1) Go row by row, find the rows with the NA, then find the columns in the row with NA
# 2) For each column with NA in the row...
# 3) ...Get the mu for the column with the NA, X^T = [mu1, mu2, mu3, mu4]
# 4) ...Then get the mu for the other columns with observations, X^T = [mu1, mu2, mu3, mu4]
# 5) ...Then get the observed values in the columns that have data
# 6) ???Calculate and split the covariance matrix based on the NAs
# 7) Find the expected value of the NAs and replace the NA

# Run loop on data to find NAs and calculate expected values
i <- 1
# make copy of data
annual.prcp.fill <- annual.prcp
# create empty vectors for output
i.id.all <- c()
j.na.all <- c()
means.na.all <- c()
var.na.all <- c()
CI.upper.all <- c()
CI.lower.all <- c()

for (i in 1:length(annual.prcp[,1])) {
# 1) Go row by row, find the rows with the NA, then find the columns in the row with NA
  if (sum(is.na(annual.prcp[i,])) >0 ) {
    # ID columns in row with NA and put in vectors
    i.id <- c(i)
    i.id.all <- c(i.id) #append to vector
    j.na <- which(is.na(annual.prcp[i,]))
    j.obs <- which(!is.na(annual.prcp[i,]))
    j.na.all <- c(j.na) #append to vector

# 2) For each column with NA in the row...
# 3) ...Get the mu for the column with the NA, X^T = [mu1, mu2, mu3, mu4]
    mu.na <- mu[j.na]
# 4) ...Then get the mu for the other columns with observations, X^T = [mu1, mu2, mu3, mu4]
    mu.obs <- mu[j.obs] #also could use setdiff(mu, mu.na)
# 5) ...Then get the observed values in the row in the columns that have data
    x.obs <- annual.prcp[i,j.obs]

# 6) Calculate and split the covariance matrix, getting the S12 (upper right quadrant) that represents the row with no data and the column with data and the S22 (lower right quadrant) that represents the row and column with data 
    S11 <- S[j.na, j.na] #no data for row and column
    S12 <- S[j.na, j.obs] #row index without data, col index with data
    S21 <- S[j.obs, j.na] #row index with data, col index no data
    S22 <- S[j.obs, j.obs] #row index with data, col index of data

# 7) Find the expected value of the NAs (need to deal with the NAs in the calculation)
    x.na <- mu.na + S12 %*% solve(S22) %*% (x.obs - mu.obs)
    # replace this vector back into the dataset with i and j index
    annual.prcp.fill[i,j.na] <- x.na

# 8) Find the variance
    var.na <- S11 - S12 %*%  solve(S22) %*% S21
    var.na <- diag(var.na)
    
# 10) Report the estimates and 95% confidence bounds of each missing data point
    # Calculate 95% confidence bounds of each missing data point
    # CI = 1.96 * sqrt(conditional variance or SD)
    margin <- 1.96 * sqrt(var.na)
    lower <- x.na - margin
    upper <- x.na + margin

    # Append estimates, variances, and CI values
    means.na.all <- c(means.na.all, x.na)
    var.na.all <- c(var.na.all, var.na)
    CI.lower.all <- c(CI.lower.all, lower)
    CI.upper.all <- c(CI.upper.all, upper)
      
  }
}
NA.means <- data.frame()
NA.means <- rbind(NA.means, data.frame(i.id.all, j.na.all, means.na.all, var.na.all, CI.lower.all, CI.upper.all))
print(NA.means)

##### 3) Multivariate Inference ####
# i) Plot gape-filled data as annual precipitation over time for each gage

annual.prcp.fill <- cbind(annual.prcp.fill, as.matrix(annual.prcp.df[,1]))
plot(annual.prcp.fill[,5], annual.prcp.fill[,1], type = "l", col = "#404788FF", lwd = 2, 
     ylim = c(0,60),
     xlab = "",
     ylab = "Annual Precipitation",
     main = "Annual Precipitation at 4 Gages")
lines(annual.prcp.fill[,5], annual.prcp.fill[,2], type = "l", lwd = 2, col = "#238A8DFF")
lines(annual.prcp.fill[,5], annual.prcp.fill[,3], type = "l", lwd = 2, col = "#55C667FF")
lines(annual.prcp.fill[,5], annual.prcp.fill[,4], type = "l", lwd = 2, col = "#FDE725FF")
legend("bottomright", box.lty=0,
       lty=1, cex=0.8, lwd = 2,
       legend = c("Gage 1", "Gage 2", "Gage 3", "Gage 4"), 
       col = c("#404788FF", "#238A8DFF", "#55C667FF", "#FDE725FF"))

# Linear regression of Gage X against years
lm.gage1 <- lm(annual.prcp.fill[,1] ~ annual.prcp.fill[,5])
summary(lm.gage1)

lm.gage2 <- lm(annual.prcp.fill[,2] ~ annual.prcp.fill[,5])
summary(lm.gage2)

lm.gage3 <- lm(annual.prcp.fill[,3] ~ annual.prcp.fill[,5])
summary(lm.gage3)

lm.gage4 <- lm(annual.prcp.fill[,4] ~ annual.prcp.fill[,5])
summary(lm.gage4)

# Function to grab p-values from lm summaries
pval <- function (modelobject) {
  if (class(modelobject) != "lm") stop("Not an object of class 'lm' ")
  f <- summary(modelobject)$fstatistic
  p <- pf(f[1],f[2],f[3],lower.tail=F)
  attributes(p) <- NULL
  return(p)
}

pval(lm.gage1)
pval(lm.gage2)
pval(lm.gage3)
pval(lm.gage4)

# J) Partition the data into two segments: first 15 years of data, and the other 16 years of data
annual.prcp.fill.1980.1994 <- annual.prcp.fill[1:15,]
annual.prcp.fill.1995.2010 <- annual.prcp.fill[16:31,]

# K) Calculate the multivariate mean for each of these subsets.
# delta.x = xbar.2 - xbar.1 = 0?
xbar.1 <- apply(annual.prcp.fill.1980.1994[,1:4], MARGIN = 2, FUN = mean)
xbar.2 <- apply(annual.prcp.fill.1995.2010[,1:4], MARGIN = 2, FUN = mean)
delta.x <- xbar.2 - xbar.1
print(delta.x)

# L) Calculate the covariance matrix for each of these subsets.
# var.delta.x = cov(xbar.2 - xbar.1)
cov.delta.x.1 <- cov(annual.prcp.fill.1980.1994[,1:4])
# var.delta.x.1 <- diag(cov.delta.x.1)
cov.delta.x.2 <- cov(annual.prcp.fill.1995.2010[,1:4])
# var.delta.x.1 <- diag(cov.delta.x.2)

# M) Estimate the pooled covariance matrix based on the two covariance matrices above.
# Var(delta.x) = ( (n1 - 1)*cov.n1 + (n2-1)*cov.n2 ) / (n1 + n2 -2)
n1 <- length(annual.prcp.fill.1980.1994[,1])
n2 <- length(annual.prcp.fill.1995.2010[,1])
var.pooled <- (1/n1 + 1/n2) * ( ( (n1 -1) / (n1 + n2 -2) ) * cov.delta.x.1 + ( (n2 - 1) / (n1 + n2 -2) ) * cov.delta.x.2 )

# n) Calculate and report the Hoteling-T statistic that compares the two means of these different subsets.
T.sq = t(delta.x) %*% solve(var.pooled) %*% t(t(delta.x)) #t(t(delta.x)) to get the dimensions correct

# Then calculate and report the F-statistic used in the F-test to compare these mean vectors (a scaled version the Hoteling-T statistic).
# K = degrees of freedom = 1? or 3?
# F = (n1+n2-k-1)/((n1+n2-2)*k) * T.sq
k <- 4
F.test <- ( ( n1 + n1 - k - 1) / ( (n1 + n2 - 2) * k) ) * T.sq

# O) Compare this statistic to a 95% critical value of the F-distribution with appropriate degrees of freedom.
df <- n1 + n2 - k -1
qf(0.95, k, df)
  
# Comment on whether there is evidence to suggest that the vector of means of annual precipitation at the four gages has changed across the southern shore of Lake Ontario. How does this compare against the 4 individual trend tests you conducted above?
