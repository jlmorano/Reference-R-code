#ntroduction to Modeling Term Project

#recreate model in:
#Rustici M, Ceccherelli G, Piazzi L. 2017. Predator exploitation and sea urchin bistability: Consequence on benthic alternative states. Ecological Modelling. 344:1–5. doi:10.1016/j.ecolmodel.2016.10.021.

# Model:
# Prey (urchin)
# dX/dt = growth-predation
# dX/dt = rx*X*(1-(X/Kx)) - Y*((A*X^2)/(x^2+B))

# Predator (fish)
# dX/dt = growth - prey consumption - fishing
# dX/dt = ry*Y*(1-Y/Ky) - c*Y*(A*X^2/(x^2+B)) - s*sqrt(Y)

##Parameters
# c = coefficient of conversion from prey to predator
# A = prey handling time
# B = prey encounter rate
# s = fishing parameter
# rx = growth prey
# ry = growth predator
# K = carrying capacity
# X = number of sea urchin per surface
# Y = number of predator fish per surface

##Set parameters
# ry = 2.1
# Kx = 31
# Ky = 26
# A = B= 1
# c = 0.95

##Vary parameters
# rx
# s

#Clear all variables
rm(list=ls(all=TRUE))

require(deSolve)
require(Rpplane)

####### Phase-Plane Figure ########
#Create Function to calculate population of Urchin-Fish Population
urchinfish=function(X,Y,parms) {
  rx = parms[1]; #growth prey
  ry = parms[2]; #growth predator
  Kx = parms[3]; #carrying capacity of prey
  Ky = parms[4]; #carying capacity of predator
  c = parms[5]; #coefficient of conversion from prey to predator
  A = parms[6]; #A = prey handling time
  B = parms[7]; #B = prey encounter rate
  s = parms[8]; #s = fishing parameter
  dX = rx*X*(1-(X/Kx)) - Y*((A*X^2)/(X^2+B)) #prey: urchin
  dY = ry*Y*(1-Y/Ky) + c*Y*(A*X^2/(X^2+B)) - s*sqrt(Y) #predator: fish
  return(c(dX, dY))
}

##Set parameters
# rx = 5
# ry = 2.1
# Kx = 31
# Ky = 26
# A = B= 1
# c = 0.95

#Vary s (0-6.5)
parms=c(4.7, 2.1, 31, 26, 0.95, 1, 1, 1)
Rpplane(fun=urchinfish, xlim=c(-1,50),ylim=c(-1,50),parms=parms,x_lab="Urchin",y_lab="Fish")
#grabbed the x,y points as s varied
#saved into S points.csv

####### Bifurcation Figure ########
Xss = read.csv("/Users/janellemorano/Documents/_Spring 2020 courses/Modeling Course/Term Project/S points.csv", header = TRUE)
#plot saddle-low
plot(Xss$S, Xss$DL.x, xlim=c(0,8),ylim=c(0,40), type = "o", xlab = "S", ylab = "Xss", col= "blue", lwd = 3)
#plot saddle-high
lines(Xss$S, Xss$DH.x, type = "o", col = "blue", lwd = 3)
#plot stable-high
lines(Xss$S, Xss$SH.x, type = "o", lwd = 3)
#plot stable-low
lines(Xss$S, Xss$SL.x, type = "o", lwd = 3)
#plot unstable
lines(Xss$S, Xss$UL.x, type = "o", col="blue", lwd = 3)

#plot predator, stable-high
lines(Xss$S, Xss$SH.y, type = "o", col = "purple", lwd = 3)

