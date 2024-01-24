# A) Read in data
Gages2 <- read.table("/Users/janellemorano/Git/Reference-R-scripts/Envtl-Multivariate-Stats/data/gages2.data.txt", sep = " ", header = TRUE)

# B) Function to standardize
scale2 <- function(x) {
  x <- (x- mean(x))/sd(x)
  return(x)
} 

# C) Standardize each variable in the dataset and save to Gages2.scale
# MARGIN = 2 applies the function scale2 to each column
Gages2.scale <- data.frame(apply(Gages2, MARGIN = 2, FUN=scale2)) 
# head(Gages2.scale)

# E) '.' applies lm to all columns; +0 removes intercept
lm.Gages2.scale <- lm(runoff ~ . +0, data = Gages2.scale)
summary(lm.Gages2.scale)
# Remember that Betahat is the Estimate/StdError = t-value

# F) 
#Create a vector of runoff predictions based on the model. These are predictions
pred.runoff <- predict(lm.Gages2.scale)
#Calculate the root mean squared error (RMSE) of the predictions.
sqrt(mean((Gages2.scale$runoff - pred.runoff)^2))

plot(x = Gages2.scale$runoff, 
     y = pred.runoff,
     xlim = c(-2.5, 2),
     ylim = c(-2.5, 2),
     xlab='Observed Annual Runoff (standardized)',
     ylab='Predicted Annual Runoff (standardized)',
     main='Predicted vs. Observed Annual Runoff')
# add line passing through the intercept and slope
abline(a = 0,                                        
       b = 1,
       col = "red",
       lwd = 2)

# G)
library(car)
sort(vif(lm.Gages2.scale))

# H)
library(glmnet)
# Create 100 lambda values between 0 and 0.5 (use sequence() to make these lambda values.
lambda.seq <- seq(0, 0.5, length=100)
# Change the variable types from a data.frame to a data matrix to use in glmnet.
X <- data.matrix(Gages2.scale) # turns the data frame into a data matrix
x <- X[,2:ncol(X)] # pulls out the independent variables
y <- X[,1] # pulls out the dependent variable

# The cv.glmnet function will automatically select a 10-fold CV and average the cross-validated mean square error for predictions over the 10 out-of-sample prediction sets.
# Lasso regression: alpha=1
cv.lasso <- cv.glmnet(x, y, lambda = lambda.seq, alpha = 1)
# Ridge regression: alpha = 0
cv.ridge <- cv.glmnet(x, y, lambda = lambda.seq, alpha = 0)

# I)
# plot the mean cross-validated error values against the lambda values for both ridge and lasso cross-validations in 2 side-by-side plots.
par(mfrow = c(1, 2))
plot(cv.lasso$lambda, cv.lasso$cvm)
plot(cv.ridge$lambda, cv.ridge$cvm)
# Select the lambda with the smallest mean cross-validated error
min(cv.lasso$lambda.min)
min(cv.ridge$lambda.min)

# J)
# Lasso regression: alpha=1
lasso <- glmnet(x, y, lambda = cv.lasso$lambda.min, alpha = 1)

# Ridge regression: alpha = 0
ridge <- glmnet(x, y, lambda = cv.ridge$lambda.min, alpha = 0)

# Grab the estimates for each of the models
beta.OLS <- as.matrix(summary(lm.Gages2.scale)$coefficients[,2])
beta.lasso <- lasso$beta
beta.ridge <- ridge$beta

# Put them together
table1 <- cbind(beta.OLS, beta.lasso, beta.ridge)
# print(table1)

# K)
subset1 <-sample(1:nrow(X),
                 size=round(nrow(X)/2),
                 replace=F) #training data
Gages2.scale.subset1 <- Gages2.scale[subset1,]

subset2 <- (1:nrow(X))[-subset1] #validation set (new data)
Gages2.scale.subset2 <- Gages2.scale[subset2,]

# L)
subset1.OLS <- lm(runoff ~ 0+ ., data = Gages2.scale.subset1)

# M)
predictions <- predict(subset1.OLS, newdata = Gages2.scale.subset2)
# observed y values from validation data minus the training data
sqrt(mean((subset2$runoff - predictions)^2))

