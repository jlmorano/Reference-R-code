#Exercise 2.32
#Right whales

#Stages are 1=calf, 2=immature female,
#3=mature female, 4=mother (mature female with calf).

##Set parameters
#Fecundity
f1 = 0
f2 = 0
f3 = 0.08
f4 = 0

#Survival/Growth
s1 = 0.92
s2 = 0.08
s3 = 0.2

#Staying the same
a2 = 0.86
a3 = 0.8
a4 = 0

#Mature, without being a mother
b = 0.62

##Build matrix
#Leslie Matrix
L = matrix(c(
  f1, f2, f3, f4,
  s1, a2, 0, 0,
  0, s2, a3, b,
  0, 0, s3, a4),
  nrow=4,
  byrow=TRUE)
L

#Get the eigenvectors, to get the ratio of invididuals in each age class
eigen(L)
#dominant eigenvector
w = eigen(L)$vectors[,1]

#the population starts in 2015 with a total of 300 females in the stable stage distribution, with w
300*w/sum(w)
#So this puts the total number in the population, proportionally into each age class
#[1]  12.75081  99.51226 155.85991  31.87702

#Initial population, at each age
n0 <- 300*w/sum(w)
#with inital 300
#[1]  12.75081  99.51226 155.85991  31.87702


#Multiply the Leslie Matrix (L) by the initial population vector
n1 = L %*% n0
#to get the population at each age
n1
#total population
sum(n1)

#Now create a matrix to project 100 years in the future
pop = matrix(0,100,4)

#Initial population
pop[1,] <- n0

#create matrix N to capture how many moms must be added to the population and the population
N<- matrix(0,100,100)

for (m in 1:20) {
  #Calculate the population over the next 100 years
  for (i in 2:100) {
    # note that the first time through,i=2 so pop[i-1,] is the initial age structure
    pop[i,]<- L %*% pop[i-1,]
    pop[i,4]<-pop[i,4]+m
    N[i,m]<-sum(pop[i,])
  }
}

plot(N[100,1:20],type="o",lty=1)



