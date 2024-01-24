# Assignment 4

# explore the major modes of variability in a reconstruction of the Palmer Drought Severity Index (PDSI) across North America, a monthly measure of the balance between atmospheric moisture supply and demand and takes into account regional climatology and short-term hydrologic persistence.
# Data: The Living Blended Drought Atlas (LBDA), a paleoclimate reconstruction of average summer (June–August) PDSI over North America based on 1845 tree ring chronologies spatially complete over North America from 1473 to 2005
#   columns are locations
#   rows are time
#   values are the PDSI values


# a) Load LBDA.RData and lon.lat.RData
load("/Users/janellemorano/Git/Reference-R-scripts/Envtl-Multivariate-Stats/data/LBDA.RData")
load("/Users/janellemorano/Git/Reference-R-scripts/Envtl-Multivariate-Stats/data/lon.lat.RData")
ls()

# 1. Conduct a Principal Component Analysis
# b) Conduct a PCA on the covariance matrix (prcomp()) on the LBDA matrix (all values at all locations and times)
pca <- prcomp(LBDA, center= TRUE, scale= TRUE)

summary(pca) #ids the PCs and their stdev

# Remember for PCA...U = XW
# X = the data (LBDA matrix)
# W = weights; columns are EOFs/eigenvectors/principle axes; elements are loadings
# U = columns are PCAs/transformed X values; elements are scores
# total variance (sum of diagonals of cov matrix) is the sum of the eigenvalues

# names(pca)
pca$x #U matrix
pca$rotation #W matrix
pca$sdev #sqrt of eigenvalues/lambdas of covariance matrix; sdev^2 are the lambdas
# variance explained by the PC = sdev^2/ sum(sdev^2)

# c) Calculate and report the variance explained by each of the first 10 EOFs
var_explained <- pca$sdev^2 / sum(pca$sdev^2)
print(var_explained[1:10])

# d) Scree plot and +/- 1 standard error for the first 10 eigenvalues
# scree plot is the variance explained or proportion of variation by each PC
# Put eigenvalues into df
eigen_df <- data.frame(PC= paste0("PC",1:533), #there are 533 PCs
                               EOF=pca$sdev^2)
print(eigen_df[1:10,])

# add SE
# SE = lambda(i) * sqrt(2/n)
SE <- pca$sdev^2 * sqrt(2/length(pca$sdev))
# add to df
eigen_df$SE <- SE
head(eigen_df)

# Plot
library(ggplot2)
library(dplyr)
# pca$sdev by cols of U pca$x OR var_explained_df$var_explained by var_explained_df$PC
scree <- eigen_df[1:10,]
# order PCs
scree <- scree %>%
  arrange(desc(EOF)) %>%
  mutate(PC=factor(PC, levels=PC))

ggplot(data = scree, aes(x = PC, y = EOF, group = 1)) +
  geom_line() +
  geom_point() +
  geom_errorbar(aes(ymin=EOF-SE, ymax=EOF+SE)) +
  xlab("Principal Component") +
  ylab("Variance Explained") +
  ggtitle("Scree Plot") +
  theme_bw()

# North's Rule of Thumb: how many PCs to retain here?
# North's Rule is to make a scree plot and look for where the SE of eigenvalues are not overlapping, because when the SEs are overlapping, they are not different from each other and the uncertainty between the values is too great and they should be ignored. 
# Looking at this graph, PCs 6-10 are not different from each other. PC5 is barely different from PC6. PCs1-4 are more different from the other PCs, so keep the first 4.

# e) Plot EOFs (elements are loadings) patterns on map
# loadings = weights/EOFs/W matrix
# Retain the 4 EOFs from pca$rotation into cur_EOF
cur_EOF <- pca$rotation[,1:4]
# Now have the first 4 EOFs for each row (location) of data

# make vector of color breaks
mycol_list <- list() 
for (i in 1:4) {
  mybreaks <- c(seq(-0.04,-0.001,by=0.004),seq(0.001,0.04,by=0.004))
  mycut <- cut(cur_EOF[,i],breaks=mybreaks,label=FALSE)
  mycolpal <- colorRampPalette(c("red","white","blue"))
  mycol <- mycolpal(length(mybreaks))[mycut]
  mycol_list[[i]] <- mycol
}

# f) Plot each EOF on map
# In a multi-panel figure, plot each EOF using the plot() function, the information
# in lon.lat, and the vector of colors in mycol. Add a map of countries by adding
# map("world",add=T)
library(maps)
par(mfrow = c(2,2))
plot(x = lon.lat$lon, y = lon.lat$lat, col = mycol_list[[1]],
     xlab = "Longitude", ylab= 'Latitude', main = 'EOF 1')
map("world", add = T)

plot(x = lon.lat$lon, y = lon.lat$lat, col = mycol_list[[2]],
     xlab = "Longitude", ylab = "Latitude", main = 'EOF 2')
map("world", add = T)

plot(x = lon.lat$lon, y = lon.lat$lat, col = mycol_list[[3]],
     xlab = 'Longitude', ylab= 'Latitude', main = 'EOF 3')
map("world", add = T)

plot(x = lon.lat$lon, y = lon.lat$lat, col = mycol_list[[4]],
     xlab = 'Longitude', ylab= 'Latitude', main = 'EOF 4')
map("world", add = T)

par(mfrow = c(1,1))

# 2) Create a Parsimonious Forecast Model
# g) forecast PDSI in next year for each EOF

# Save the forecast predictions in a vector
PC.forecast <- c()

for (i in 1:4) {
  # select PCAs (U matrix)
  PC <- pca$x[,i]
  
  # g) fit a simple linear regression model between lead and lag versions of given PC
  # x = t-1
  x <- PC[1:(length(PC)-1)]
  # y: t
  y <- PC[2:length(PC)]
  # put x & y values in df
  df <- data.frame(x,y)
  # fit model
  PC.lm <- lm(y ~ x , data = df)
  
  # h) forecast for each of the retained PCs in 2005 (the last PC value) for the time step n+1 (=2006)
  forecast <- predict( PC.lm, newdata = data.frame( x = PC[length(PC)] ) )
  
  # # Export the predictions for 2006
  PC.forecast[i] <- forecast
}

# See predictions
PC.forecast

# i) Using the synthesis equation and the vector of 1-step ahead forecasts, develop a 1-step ahead forecast for the PDSI for all of the grid cells.

# Synthesis equation = X = U %*% t(W)
# X = output
# U = forecast
# t(W) = transpose of original loadings (ie pca$rotation)
forecast.PDSI <- PC.forecast %*% t(pca$rotation[,1:4])

# j) Plot forecasts on a map
# make vector of color breaks of forecast.PDSI, from -1 to 1
mybreaks <- mybreaks <- c(seq(-1,-0.01,by=0.02),seq(0.01,1,by=0.02))
mycut <- cut(forecast.PDSI, breaks=mybreaks,label=FALSE) # something is wrong is going on here
mycolpal <- colorRampPalette(c("red","white","blue"))
mycol <- mycolpal(length(mybreaks))[mycut]

# Now plot it
plot(x = lon.lat$lon, y = lon.lat$lat, col = mycol,
     xlab = "Longitude", ylab= 'Latitude', main = 'Forecast of 2006 PDSI')
map("world", add = T)


# Rotate the EOFs and Derive Rotated Principal Components

# k-m) Rotate EOFs (use 1st 4 EOFs/weights/W matrix of original pca$rotation)
my.varimax <- varimax(pca$rotation[,1:4])

# Find rotated PCs: multiply the P LBDA by REOFs (reofs)
# W* = W %* %T
# new set of rotated EOFs = original EOFs %*% rotation matrix
rotated.EOF <- pca$rotation[,1:4] %*% my.varimax$rotmat
rotated.PCs <- LBDA %*% rotated.EOF

# m) Report the correlation matrix of the rotated PCs and the original PCs
# Rotated PCs
cor(rotated.PCs)
# original PCs
cor(pca$x[,1:4]) 

# make vector of color breaks of rotated.PC, from -1 to 1
# mybreaks <- mybreaks <- c(seq(-1,-0.01,by=0.2),seq(0.01,1,by=0.2))
# mycut <- cut(rotated.EOF, breaks=mybreaks, label=FALSE) # something is wrong is going on here
# mycolpal <- colorRampPalette(c("red","white","blue"))
# mycol <- mycolpal(length(mybreaks))[mycut]
# 
# # Now plot it
# plot(x = lon.lat$lon, y = lon.lat$lat, col = mycol,
#      xlab = "Longitude", ylab= 'Latitude', main = 'Rotated EOFs')
# map("world", add = T)

# make vector of color breaks
mycol_list <- list() 
for (i in 1:4) {
  mybreaks <- c(seq(-0.04,-0.001,by=0.004),seq(0.001,0.04,by=0.004))
  mycut <- cut(rotated.EOF[,i],breaks=mybreaks,label=FALSE)
  mycolpal <- colorRampPalette(c("red","white","blue"))
  mycol <- mycolpal(length(mybreaks))[mycut]
  mycol_list[[i]] <- mycol
}

# f) Plot each rotated EOF on map
par(mfrow = c(2,2))
plot(x = lon.lat$lon, y = lon.lat$lat, col = mycol_list[[1]],
     xlab = "Longitude", ylab= 'Latitude', main = 'EOF 1')
map("world", add = T)

plot(x = lon.lat$lon, y = lon.lat$lat, col = mycol_list[[2]],
     xlab = "Longitude", ylab = "Latitude", main = 'EOF 2')
map("world", add = T)

plot(x = lon.lat$lon, y = lon.lat$lat, col = mycol_list[[3]],
     xlab = 'Longitude', ylab= 'Latitude', main = 'EOF 3')
map("world", add = T)

plot(x = lon.lat$lon, y = lon.lat$lat, col = mycol_list[[4]],
     xlab = 'Longitude', ylab= 'Latitude', main = 'EOF 4')
map("world", add = T)

par(mfrow = c(1,1))
