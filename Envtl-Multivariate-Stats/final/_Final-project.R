# Final project
# Research Question: What is the relationship between environmental factors and species abundance/biomass?
# Janelle L. Morano
# last updated May 11, 2022
###############################

# Clean up
rm(list = ls(all = TRUE))
graphics.off()

################
## Get Data
################

# NEFSC menhaden catch & biomass, depth, surface & bottom water temp, surface & bottom salinity
# menhaden2 <- read.csv("/Users/janellemorano/Git/Reference-R-scripts/Envtl-Multivariate-Stats/final/data/nefsc-menhaden-catch-envtl.csv", header = TRUE)
menhaden <- read.csv("/Users/janellemorano/Git/Reference-R-scripts/Envtl-Multivariate-Stats/final/data/nefsc.menhaden.1963-2021.csv", header = TRUE)

library(dplyr)
# There are a lot of zeros, so log transform abundance & biomass
menhaden <- menhaden %>%
  mutate(logAb = ifelse(abundance >0, log(abundance), 0),
         logBio = ifelse(biomass >0, log(biomass), 0))

# Where are NAs
sapply(menhaden, function(x) sum(is.na(x)))
# Remove them
menhaden <- na.omit(menhaden)
head(menhaden)
range(menhaden$year)
# 1992 2020



menhaden %>% 
  filter(abundance >0) %>%
  summarise(range(surftemp),
            range(bottemp),
            range(surfsalin),
            range(botsalin),
            range(depth),
            mean(abundance),
            mean(biomass))

menhaden %>%
  summarise(sum = sum(abundance > 0),
            sum2 = sum(abundance == 0))


plot(menhaden$year,menhaden$biomass)


# Split by Spring and Fall survey seasons
menhaden.spring <- menhaden %>%
  filter(season == "SPRING")

menhaden.fall <- menhaden %>%
  filter(season == "FALL")

menhaden.spring %>% 
  filter(abundance >0) %>%
  summarise(range(surftemp),
            range(bottemp),
            range(surfsalin),
            range(botsalin),
            range(depth))

menhaden.fall %>% 
  filter(abundance >0) %>%
  summarise(range(surftemp),
            range(bottemp),
            range(surfsalin),
            range(botsalin),
            range(depth))

################
## Correlation
################

# Examine correlation between variables
library(corrplot)
library(RColorBrewer)

par(mfrow = c(1,2))
corr.spring <-cor(menhaden.spring[,14:18])
corrplot(corr.spring, type="lower", 
         order="alphabet", # alt "hclust"
         tl.srt = 45, 
         bg = "White",
         col=brewer.pal(n=8, name="RdYlBu"),
         title = "\n\n Spring")


corr.fall <-cor(menhaden[,14:18])
corrplot(corr.fall, type="lower", 
         order="alphabet", # alt "hclust"
         tl.srt = 45, 
         bg = "White",
         col=brewer.pal(n=8, name="RdYlBu"),
         title = "\n\n Fall")
par(mfrow = c(1,1))


################
## PCA
################
# PCA (prcomp()) on "lat", "lon", "depth", "surftemp", "surfsalin", "bottemp", "botsalin"
# Remember for PCA...U = XW
# X = the data
# pca$x #U matrix; columns are PCAs/transformed X values; elements are scores
# pca$rotation #W matrix; weights; columns are EOFs/eigenvectors/principle axes; elements are loadings
# pca$sdev #sqrt of eigenvalues/lambdas of covariance matrix; sdev^2 are the lambdas
# variance explained by the PC = sdev^2/ sum(sdev^2)
# total variance (sum of diagonals of cov matrix) is the sum of the eigenvalues
# names(pca)

apply(menhaden.spring[,14:18],FUN=var,2)
# depth    surftemp   surfsalin     bottemp    botsalin 
# 5167.491314    9.285682    2.021100    9.571179    1.716069

pca.spring <- prcomp(menhaden.spring[,14:18], center= TRUE, scale = TRUE)
summary(pca.spring)

pca.fall <- prcomp(menhaden.fall[,14:18], center = TRUE, scale = TRUE)
summary(pca.fall)

# Calculate and report the variance explained by each of the EOFs
var_explained.spring <- pca.spring$sdev^2 / sum(pca.spring$sdev^2)
print(var_explained.spring)

var_explained.fall <- pca.fall$sdev^2 / sum(pca.fall$sdev^2)
print(var_explained.fall)


## Scree plot
################

# Scree plot and +/- 1 standard error for the eigenvalues**
# scree plot is the variance explained or proportion of variation by each PC
# Put eigenvalues into df
eigen_spring <- data.frame(PC= paste0("PC",1:5), #there are 7 PCs
                       EOF=pca.spring$sdev^2)

eigen_fall <- data.frame(PC= paste0("PC",1:5), #there are 7 PCs
                       EOF=pca.fall$sdev^2)

# add SE
# SE = lambda(i) * sqrt(2/n)
SE.spring <- pca.spring$sdev^2 * sqrt(2/length(pca.spring$sdev))
SE.fall <- pca.fall$sdev^2 * sqrt(2/length(pca.fall$sdev))
# add to df
eigen_spring$SE <- SE.spring
head(eigen_spring)
eigen_fall$SE <- SE.fall
head(eigen_fall)

# Plot
library(ggplot2)
library(cowplot)

# Spring
# pca$sdev by cols of U pca$x OR var_explained_df$var_explained by var_explained_df$PC
scree.spring <- eigen_spring
# order PCs
scree.spring <- scree.spring %>%
  arrange(desc(EOF)) %>%
  mutate(PC=factor(PC, levels=PC))


scree.spring <- ggplot(data = scree.spring, aes(x = PC, y = EOF, group = 1)) +
  geom_line() +
  geom_point() +
  geom_errorbar(aes(ymin=EOF-SE, ymax=EOF+SE)) +
  xlab("Principal Component") +
  ylab("Variance Explained") +
  ggtitle("Spring") +
  theme_classic()

# plot(pca.spring)
# screeplot(pca.spring, type = "line", main = "Spring Scree plot")

# Fall
# pca$sdev by cols of U pca$x OR var_explained_df$var_explained by var_explained_df$PC
scree.fall <- eigen_fall
# order PCs
scree.fall <- scree.fall %>%
  arrange(desc(EOF)) %>%
  mutate(PC=factor(PC, levels=PC))

scree.fall <- ggplot(data = scree.fall, aes(x = PC, y = EOF, group = 1)) +
  geom_line() +
  geom_point() +
  geom_errorbar(aes(ymin=EOF-SE, ymax=EOF+SE)) +
  xlab("Principal Component") +
  ylab("Variance Explained") +
  ggtitle("Fall") +
  theme_classic()

# plot(pca.fall)
# screeplot(pca.fall, type = "line", main = "Fall Scree plot")

plot_grid(scree.spring, scree.fall, labels = c('A', 'B'))

## Biplot of PCs
################

# pca.spring$x <- -1*pca.spring$x
biplot(pca.spring, cex = 0.6,
       main = "Spring")

# pca.fall$x <- -1*pca.fall$x
biplot(pca.fall, cex = 0.6,
       main = "Fall")


# Another way
# library(devtools)
# install_github("vqv/ggbiplot")
# require(ggbiplot)
# ggbiplot(pca.spring)

# PCA with vegan
# library(vegan)
# rda <- rda(menhaden.spring[,14:18])
# vegan::biplot(rda)

# PCA with vegan
# library(vegan)
# rda <- rda(menhaden.fall[,14:18])
# vegan::biplot(rda)

# ggfortify and ggplot2
library(ggfortify)
autoplot(pca.spring, main = "Spring", 
         loadings = TRUE, loadings.label = TRUE, loadings.label.size = 5)
autoplot(pca.fall, main= "Fall", 
         loadings = TRUE, loadings.label = TRUE, loadings.label.size = 5)

# Plot PC patterns on map
################

## Spring
# Keep 1st PC
PC1.spring <- pca.spring$x[,1]

# make vector of color breaks
range(PC1.spring)
mybreaks <- c(seq(-8.5,7.5,by=0.2))
mycut <- cut(PC1.spring,breaks=mybreaks,label=FALSE)
mycolpal <- colorRampPalette(c("red","white","blue"))
mycol <- mycolpal(length(mybreaks))[mycut]

# Plot PC on map
library(maps)
# par(mfrow = c(2,6))
plot(x = menhaden.spring$lon, y = menhaden.spring$lat, col = mycol, pch = 20, cex = .25,
     xlab = "Longitude", ylab= 'Latitude', main = 'Spring: PC 1')
maps::map("world", add = T)


## Fall
# Keep 1st EOFs from pca$rotation into cur_EOF
# EOF.fall <- pca.fall$rotation[,1]
PC1.fall <- pca.fall$x[,1]

# make vector of color breaks
range(PC1.fall)
mybreaks <- c(seq(-9.5,5,by=0.2))
mycut <- cut(PC1.fall,breaks=mybreaks,label=FALSE)
mycolpal <- colorRampPalette(c("red","white","blue"))
mycol <- mycolpal(length(mybreaks))[mycut]

# Plot EOFs or PC1 on map
plot(x = menhaden.fall$lon, y = menhaden.fall$lat, col = mycol, pch = 20, cex = .25,
     xlab = "Longitude", ylab= 'Latitude', main = 'Fall: PC 1')
maps::map("world", add = T)

#PC2
PC2.fall <- pca.fall$x[,2]
range(PC2.fall)
mybreaks <- c(seq(-5.5,5.1,by=0.2))
mycut <- cut(PC2.fall,breaks=mybreaks,label=FALSE)
mycolpal <- colorRampPalette(c("red","white","blue"))
mycol <- mycolpal(length(mybreaks))[mycut]

plot(x = menhaden.fall$lon, y = menhaden.fall$lat, col = mycol, pch = 20, cex = .25,
     xlab = "Longitude", ylab= 'Latitude', main = 'Fall: PC 2')
maps::map("world", add = T)




################
## Regression
################
library(viridis)

# Direct environmental relationship to menhaden abundance
par(mfrow = c(3, 2))
# Bottom temp
plot(menhaden.spring$bottemp, menhaden.spring$logBio,
     xlab = "Bottom Temperature (°C)",
     ylab = "log(Biomass)",
     main = "Spring",
     col = "#404788FF",
     frame = FALSE)
plot(menhaden.fall$bottemp, menhaden.fall$logBio,
     xlab = "Bottom Temperature (°C)",
     ylab = "log(Biomass)",
     main = "Fall",
     col = "#404788FF",
     frame = FALSE)
# Bottom salinity
plot(menhaden.spring$botsalin, menhaden.spring$logBio,
     xlab = "Bottom Salinity (psu)",
     ylab = "log(Biomass)",
     main = "Spring",
     col = "#238A8DFF",
     frame = FALSE)
plot(menhaden.fall$botsalin, menhaden.fall$logBio,
     xlab = "Bottom Salinity (psu)",
     ylab = "log(Biomass)",
     main = "Fall",
     col = "#238A8DFF",
     frame = FALSE)
# Depth
plot(menhaden.spring$depth, menhaden.spring$logBio,
     xlab = "Depth (m)",
     ylab = "log(Biomass)",
     main = "Spring",
     col = "#55C667FF",
     frame = FALSE)
plot(menhaden.fall$depth, menhaden.fall$logBio,
     xlab = "Depth (m)",
     ylab = "log(Biomass)",
     main = "Fall",
     col = "#55C667FF",
     frame = FALSE)
par(mfrow = c(1, 1))


par(mfrow = c(3, 2))
qqnorm(menhaden.spring$bottemp, pch = 1, frame = FALSE, col = "#404788FF", main = "Spring, Bottom Temp")
qqline(menhaden.spring$bottemp, col = "red", lwd = 2)
qqnorm(menhaden.fall$bottemp, pch = 1, frame = FALSE, col = "#404788FF", main = "Fall, Bottom Temp")
qqline(menhaden.fall$bottemp, col = "red", lwd = 2)

qqnorm(menhaden.spring$botsalin, pch = 1, frame = FALSE, col = "#238A8DFF", main = "Spring, Bottom Salinity")
qqline(menhaden.spring$botsalin, col = "red", lwd = 2)
qqnorm(menhaden.fall$botsalin, pch = 1, frame = FALSE, col = "#238A8DFF", main = "Fall, Bottom Salinity")
qqline(menhaden.fall$botsalin, col = "red", lwd = 2)

qqnorm(menhaden.spring$depth, pch = 1, frame = FALSE, col = "#55C667FF", main = "Spring, Depth")
qqline(menhaden.spring$depth, col = "red", lwd = 2)
qqnorm(menhaden.fall$depth, pch = 1, frame = FALSE, col = "#55C667FF", main = "Fall, Depth")
qqline(menhaden.fall$depth, col = "red", lwd = 2)
par(mfrow = c(1, 1))



# PCs as related to abundance & biomass
# plot(PC1.spring, menhaden.spring$logAb)
# plot(PC1.spring, menhaden.spring$logBio)
# plot(menhaden.spring$lat, PC1.spring)
# plot(menhaden.spring$lon, PC1.spring)
# 
# plot(PC1.fall, menhaden.fall$logAb)
# plot(PC1.fall, menhaden.fall$logBio)
# plot(menhaden.fall$lat, PC1.fall)
# plot(menhaden.fall$lon, PC1.fall)
# 
# plot(PC2.fall, menhaden.fall$logAb)
# plot(PC2.fall, menhaden.fall$logBio)
# plot(menhaden.fall$lat, PC2.fall)
# plot(menhaden.fall$lon, PC2.fall)

## Build a linear regression
# Standardize envt'l variables
scale2 <- function(x) {
  x <- (x- mean(x))/sd(x)
  return(x)
} 

menhaden.spring.scale <- data.frame(apply(menhaden.spring[,14:18], MARGIN = 2, FUN=scale2)) 
# add lat, lon, logBio back in
menhaden.spring.scale <- cbind(menhaden.spring[,c(11:12,23)], menhaden.spring.scale, PC1.spring)

menhaden.fall.scale <- data.frame(apply(menhaden.fall[,14:18], MARGIN = 2, FUN=scale2)) 
# add lat, lon, logAb, PCs back in
menhaden.fall.scale <- cbind(menhaden.fall[,c(11:12,23)], menhaden.fall.scale, PC1.fall, PC2.fall)



### Linear models
##################
## Spring
lm.menhaden.spring.scale <- lm(logBio ~ depth + surftemp + surfsalin + bottemp + botsalin +0, data = menhaden.spring.scale)
summary(lm.menhaden.spring.scale)
pred.lm.spring <- predict(lm.menhaden.spring.scale)
sqrt(mean((menhaden.spring.scale$logBio - pred.lm.spring)^2)) #RMSE
# 0.2469764

lm2.menhaden.spring.scale <- lm(logBio ~ depth + bottemp + botsalin +0, data = menhaden.spring.scale)
summary(lm2.menhaden.spring.scale)
pred.lm2.spring <- predict(lm.menhaden.spring.scale)
sqrt(mean((menhaden.spring.scale$logBio - pred.lm2.spring)^2)) #RMSE
# 0.2469764

lmPC.menhaden.spring.scale <- lm(logBio ~ PC1.spring, data = menhaden.spring.scale)
summary(lmPC.menhaden.spring.scale)
pred.lmPC.spring <- predict(lmPC.menhaden.spring.scale)
sqrt(mean((menhaden.spring.scale$logBio - pred.lmPC.spring)^2)) #RMSE
# 0.2476228

## Fall
lm.menhaden.fall.scale <- lm(logBio ~ depth + surftemp + surfsalin + bottemp + botsalin +0, data = menhaden.fall.scale)
summary(lm.menhaden.fall.scale)
pred.lm.fall <- predict(lm.menhaden.fall.scale)
sqrt(mean((menhaden.fall.scale$logBio - pred.lm.fall)^2)) #RMSE
# 0.2705572

lm2.menhaden.fall.scale <- lm(logBio ~ depth + bottemp + botsalin +0, data = menhaden.fall.scale)
summary(lm2.menhaden.fall.scale)
pred.lm2.fall <- predict(lm2.menhaden.fall.scale)
sqrt(mean((menhaden.fall.scale$logBio - pred.lm2.fall)^2)) #RMSE
# 0.2706311

lmPC.menhaden.fall.scale <- lm(logBio ~ PC1.fall + PC2.fall, data = menhaden.fall.scale)
summary(lmPC.menhaden.fall.scale)
pred.lmPC.fall <- predict(lmPC.menhaden.fall.scale)
sqrt(mean((menhaden.fall.scale$logBio - pred.lmPC.fall)^2)) #RMSE
# 0.269942


plot(x = menhaden.spring.scale$logBio, 
     y = pred.lmPC.spring,
     # xlim = c(-2.5, 2),
     # ylim = c(-2.5, 2),
     xlab='Observed log(Biomass)',
     ylab='Predicted log(Biomass)',
     main='Predicted vs. Observed log(Biomass)')
# add line passing through the intercept and slope
abline(a = 0,                                        
       b = 1,
       col = "red",
       lwd = 2)

plot(x = menhaden.fall.scale$logBio, 
     y = pred.lmPC.fall,
     # xlim = c(-2.5, 2),
     # ylim = c(-2.5, 2),
     xlab='Observed log(Biomass)',
     ylab='Predicted log(Biomass)',
     main='Predicted vs. Observed log(Biomass)')
# add line passing through the intercept and slope
abline(a = 0,                                        
       b = 1,
       col = "red",
       lwd = 2)

### Ridge and LASSO on envt'l variables
##################
library(glmnet)

#### Spring ####
################

# Create 100 lambda values
lambda.seq <- seq(0, 0.5, length=100)

matrix <- data.matrix(menhaden.spring.scale) # turns the data frame into a data matrix
x <- matrix[,4:8] # independent variables
y <- matrix[,3] # dependent variable

# Lasso regression: alpha=1
cv.lasso.spring <- cv.glmnet(x, y, lambda = lambda.seq, alpha = 1)
# Ridge regression: alpha = 0
cv.ridge.spring <- cv.glmnet(x, y, lambda = lambda.seq, alpha = 0)

par(mfrow = c(1, 2))
plot(cv.lasso.spring$lambda, cv.lasso.spring$cvm)
plot(cv.ridge.spring$lambda, cv.lasso.spring$cvm)
# Select the lambda with the smallest mean cross-validated error
min(cv.lasso.spring$lambda.min)
min(cv.lasso.spring$lambda.min)

# Lasso regression: alpha=1
lasso <- glmnet(x, y, lambda = cv.lasso.spring$lambda.min, alpha = 1)

# Ridge regression: alpha = 0
ridge <- glmnet(x, y, lambda = cv.lasso.spring$lambda.min, alpha = 0)

# Grab the estimates for each of the models
beta.OLS <- as.matrix(summary(lm.menhaden.spring.scale)$coefficients[,2])
beta.lasso <- lasso$beta
beta.ridge <- ridge$beta
table1 <- cbind(beta.OLS, beta.lasso, beta.ridge)
print(table1)


## Out of sample predictions
###########
# get relevant cols from matrix above
spring.matrix <- matrix[,3:8]
rmse.OLS <- c()
rmse.lasso <- c()
rmse.ridge <- c()

for (i in 1:100) {
  #Randomly split database menhaden.spring.scale into 2 equal sizes (train and validate)
  train <-sample(1:nrow(spring.matrix),
                 size=round(nrow(spring.matrix)/2),
                 replace=F) #training data
  test <- (1:nrow(matrix))[-train] #validation/testing set (new data)
  train <- menhaden.spring.scale[train,3:8]
  test <- menhaden.spring.scale[test,3:8]
  
  #Fit OLS on train
  fit.OLS <- lm(logBio ~ depth + surftemp + surfsalin + bottemp + botsalin +0, data = train)
  
  #Predict OLS on test
  predict.OLS <- predict(fit.OLS, newdata = test)
  
  #Calculate RMSE and add to results
  a <- c(sqrt(mean((test$logBio - predict.OLS)^2)))
  rmse.OLS <- rbind(rmse.OLS, a)
  
  # Select lambda values
  lambda.seql <- seq(0, 0.5, length = 100)
  train.matrix <- data.matrix(train)
  x <- train.matrix[,2:ncol(train.matrix)]
  y <- train.matrix[,1]
  train.lasso <- cv.glmnet(x,y, lambda = lambda.seql, alpha = 1)
  train.ridge <- cv.glmnet(x,y, lambda = lambda.seql, alpha = 0)
  
  # Fit lasso and ridge with optimized lambda values on set1
  fit.lasso <- glmnet(x,y, lambda = train.lasso$lambda.min, alpha = 1)
  fit.ridge <- glmnet(x,y, lambda = train.ridge$lambda.min, alpha = 0)
  
  # Predict with validation set
  test.matrix <- data.matrix(test)
  pred.lasso <- predict(fit.lasso, newx = test.matrix[,2:ncol(test.matrix)])
  pred.ridge <- predict(fit.ridge, newx = test.matrix[,2:ncol(test.matrix)]) 
  
  # Calculate RMSE and report
  # ...observed values (from test/validation data) minus the estimated (prediction) data
  b <- sqrt(mean((test.matrix[,1] - pred.lasso)^2))
  rmse.lasso <- rbind(rmse.lasso, b)
  c <- sqrt(mean((test.matrix[,1] - pred.ridge)^2))
  rmse.ridge <- rbind(rmse.ridge, c)
}

# Convert each matrix of rmse to dataframe, rename, and add column to ID
rmse.OLS <- as.data.frame(rmse.OLS)
rmse.OLS$RMSE <- rmse.OLS$V1
rmse.OLS$regtype <- "OLS"

rmse.lasso <- as.data.frame(rmse.lasso)
rmse.lasso$RMSE <- rmse.lasso$V1
rmse.lasso$regtype <- "LASSO"

rmse.ridge <- as.data.frame(rmse.ridge)
rmse.ridge$RMSE <- rmse.ridge$V1
rmse.ridge$regtype <- "Ridge"

spring.results <- rbind(rmse.OLS, rmse.lasso, rmse.ridge)

boxplot(RMSE ~ regtype,
        data = spring.results, 
        main ="RMSE for Predictions of log(Biomass)",
        xlab ="RMSE")



#### Fall ####
##############

matrix <- data.matrix(menhaden.fall.scale) # turns the data frame into a data matrix
x <- matrix[,4:8] # independent variables
y <- matrix[,3] # dependent variable

# Lasso regression: alpha=1
cv.lasso.fall <- cv.glmnet(x, y, lambda = lambda.seq, alpha = 1)
# Ridge regression: alpha = 0
cv.ridge.fall <- cv.glmnet(x, y, lambda = lambda.seq, alpha = 0)

plot(cv.lasso.fall$lambda, cv.lasso.fall$cvm)
plot(cv.ridge.fall$lambda, cv.lasso.fall$cvm)
# Select the lambda with the smallest mean cross-validated error
min(cv.lasso.fall$lambda.min)
min(cv.lasso.fall$lambda.min)

# Lasso regression: alpha=1
lasso <- glmnet(x, y, lambda = cv.lasso.fall$lambda.min, alpha = 1)

# Ridge regression: alpha = 0
ridge <- glmnet(x, y, lambda = cv.lasso.fall$lambda.min, alpha = 0)

# Grab the estimates for each of the models
beta.OLS <- as.matrix(summary(lm.menhaden.fall.scale)$coefficients[,2])
beta.lasso <- lasso$beta
beta.ridge <- ridge$beta
table2 <- cbind(beta.OLS, beta.lasso, beta.ridge)
print(table2)

## Out of sample predictions
###########################

# get relevant cols from matrix above
fall.matrix <- matrix[,3:8]
rmse.OLS <- c()
rmse.lasso <- c()
rmse.ridge <- c()

for (i in 1:100) {
  #Randomly split database menhaden.spring.scale into 2 equal sizes (train and validate)
  train <-sample(1:nrow(fall.matrix),
                 size=round(nrow(fall.matrix)/2),
                 replace=F) #training data
  test <- (1:nrow(fall.matrix))[-train] #validation/testing set (new data)
  train <- menhaden.fall.scale[train,3:8]
  test <- menhaden.fall.scale[test,3:8]
  
  #Fit OLS on train
  fit.OLS <- lm(logBio ~ depth + surftemp + surfsalin + bottemp + botsalin +0, data = train)
  
  #Predict OLS on test
  predict.OLS <- predict(fit.OLS, newdata = test)
  
  #Calculate RMSE and add to results
  a <- c(sqrt(mean((test$logBio - predict.OLS)^2)))
  rmse.OLS <- rbind(rmse.OLS, a)
  
  # Select lambda values
  lambda.seql <- seq(0, 0.5, length = 100)
  train.matrix <- data.matrix(train)
  x <- train.matrix[,2:ncol(train.matrix)]
  y <- train.matrix[,1]
  train.lasso <- cv.glmnet(x,y, lambda = lambda.seql, alpha = 1)
  train.ridge <- cv.glmnet(x,y, lambda = lambda.seql, alpha = 0)
  
  # Fit lasso and ridge with optimized lambda values on set1
  fit.lasso <- glmnet(x,y, lambda = train.lasso$lambda.min, alpha = 1)
  fit.ridge <- glmnet(x,y, lambda = train.ridge$lambda.min, alpha = 0)
  
  # Predict with validation set
  test.matrix <- data.matrix(test)
  pred.lasso <- predict(fit.lasso, newx = test.matrix[,2:ncol(test.matrix)])
  pred.ridge <- predict(fit.ridge, newx = test.matrix[,2:ncol(test.matrix)]) 
  
  # Calculate RMSE and report
  # ...observed values (from test/validation data) minus the estimated (prediction) data
  b <- sqrt(mean((test.matrix[,1] - pred.lasso)^2))
  rmse.lasso <- rbind(rmse.lasso, b)
  c <- sqrt(mean((test.matrix[,1] - pred.ridge)^2))
  rmse.ridge <- rbind(rmse.ridge, c)
}

# Convert each matrix of rmse to dataframe, rename, and add column to ID
rmse.OLS <- as.data.frame(rmse.OLS)
rmse.OLS$RMSE <- rmse.OLS$V1
rmse.OLS$regtype <- "OLS"

rmse.lasso <- as.data.frame(rmse.lasso)
rmse.lasso$RMSE <- rmse.lasso$V1
rmse.lasso$regtype <- "LASSO"

rmse.ridge <- as.data.frame(rmse.ridge)
rmse.ridge$RMSE <- rmse.ridge$V1
rmse.ridge$regtype <- "Ridge"

fall.results <- rbind(rmse.OLS, rmse.lasso, rmse.ridge)

boxplot(RMSE ~ regtype,
        data = fall.results, 
        main ="RMSE for Predictions of log(Biomass)",
        xlab ="RMSE")


library(stats)
AIC(lm.menhaden.spring.scale)
AIC(lm2.menhaden.spring.scale)
AIC(lmPC.menhaden.spring.scale)

AIC(lm.menhaden.fall.scale)
AIC(lm2.menhaden.fall.scale)
AIC(lmPC.menhaden.fall.scale)
