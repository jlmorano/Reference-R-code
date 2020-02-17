#ntroduction to Modeling Term Project

#modified from model in:
#Rustici M, Ceccherelli G, Piazzi L. 2017. Predator exploitation and sea urchin bistability: Consequence on benthic alternative states. Ecological Modelling. 344:1–5. doi:10.1016/j.ecolmodel.2016.10.021.

# Model:
# Prey (urchin)
# NEW FROM PAPER: Added fishing mortality on urchin (u*sqrt(X))
# dX/dt = growth - predation - fishing mortality
# dX/dt = rx*X*(1-(X/Kx)) - Y*((A*X^2)/(x^2+B)) - u*sqrt(X)

# Predator (fish)
# dX/dt = growth - prey consumption - fishing
# dX/dt = ry*Y*(1-Y/Ky) - c*Y*(A*X^2/(x^2+B)) - s*sqrt(Y)

##Parameters
# c = coefficient of conversion from prey to predator
# A = prey handling time
# B = prey encounter rate
# s = fishing parameter on predator fish
# rx = growth prey
# ry = growth predator
# K = carrying capacity
# X = number of sea urchin per surface
# Y = number of predator fish per surface
####NEW###
# u = fishing mortality on urchin

##Set parameters
# ry = 2.1
# Kx = 31
# Ky = 26
# A = B= 1
# c = 0.95

##Vary parameters
# rx
# s
# u

#Clear all variables
rm(list=ls(all=TRUE))

require(deSolve)
require(Rpplane)

####### Phase-Plane Figure ########
#Create Function to calculate population of Urchin-Fish Population
mod.urchinfish=function(X,Y,parms) {
  rx = parms[1]; #growth prey
  ry = parms[2]; #growth predator
  Kx = parms[3]; #carrying capacity of prey
  Ky = parms[4]; #carying capacity of predator
  c = parms[5]; #coefficient of conversion from prey to predator
  A = parms[6]; #A = prey handling time
  B = parms[7]; #B = prey encounter rate
  s = parms[8]; #s = fishing parameter on predator
  u = parms[9]; #u = fishing parameter on urchin prey
  dX = rx*X*(1-(X/Kx)) - Y*((A*X^2)/(X^2+B)) - u*sqrt(X) #prey: urchin
  dY = ry*Y*(1-Y/Ky) + c*Y*(A*X^2/(X^2+B)) - s*sqrt(Y) #predator: fish
  return(c(dX, dY))
}

##Set parameters
# rx = 4.7
# ry = 2.1
# Kx = 31
# Ky = 26
# A = B= 1
# c = 0.95

#Vary s (0-6.5)
#Vary u (0-6.5)
parms=c(4.7, 2.1, 31, 26, 0.95, 1, 1, 6, 1)
Rpplane(fun=mod.urchinfish, xlim=c(-1,50),ylim=c(-1,50),parms=parms,x_lab="Urchin",y_lab="Fish")
#grabbed the x,y points as s varied
#saved into S points_u.csv (contains imaginary and real eigenvalues)
#and S points_u-modified.csv (contains only real eigenvalues)

####### Bifurcation Figure ########
# Use ggplot2
library(ggplot2)

Xss_m = read.csv("/Users/janellemorano/Box/Courses/_Intro to Modeling/Term Project/S points_u-modified.csv", header = TRUE)

#Subset to graph at each S value
s1 <- subset(Xss_m, S ==1)
s2 <- subset(Xss_m, S ==2)
s3 <- subset(Xss_m, S ==3)

# S=1
plot1 <- ggplot(s1) +
  geom_line(aes(x= u, y= x, color = Type), size=2) +
  ggtitle("s = 1") +
  xlab("u") +
  ylab("Xss") +
  theme_classic(base_size = 22) +
  theme(legend.position = "none")

# S=2
plot2 <- ggplot(s2) +
  geom_line(aes(x= u, y= x, color = Type), size=2) +
  ggtitle("s = 2") +
  xlab("u") +
  ylab("Xss") +
  theme_classic(base_size = 22) +
  theme(legend.position = "none")

grid.arrange(plot1, plot2, ncol=2)

# S=3
ggplot(s3) +
  geom_line(aes(x= u, y= x, color = Type), size=2) +
  ggtitle("s = 3") +
  xlab("u") +
  ylab("Xss") +
  theme_classic(base_size = 22)

# Graph just the real values for interpretation
Xss_mr = read.csv("/Users/janellemorano/Box/Courses/_Intro to Modeling/Term Project/S points_u-modified_real.csv", header = TRUE)

#Subset to graph at each S value
s1r <- subset(Xss_mr, S ==1)
s2r <- subset(Xss_mr, S ==2)
s3r <- subset(Xss_mr, S ==3)

# S=1
ggplot(s1r) +
  geom_line(aes(x= u, y= x, color = Type), size=2) +
  ggtitle("s = 1") +
  xlab("u") +
  ylab("Xss") +
  theme_classic(base_size = 22)
# forcing color assignment to legend is not working
# + scale_fill_manual(values=c("saddle" = "red", "stable" = "green", "unstable" = "blue"))

# S=2
ggplot(s2r) +
  geom_line(aes(x= u, y= x, color = Type), size=2) +
  ggtitle("s = 2") +
  xlab("u") +
  ylab("Xss") +
  theme_classic(base_size = 22)

# S=3
ggplot(s3r) +
  geom_line(aes(x= u, y= x, color = Type), size=2) +
  ggtitle("s = 3") +
  xlab("u") +
  ylab("Xss") +
  theme_classic(base_size = 22)
