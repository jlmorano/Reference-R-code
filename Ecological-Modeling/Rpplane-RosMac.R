rm(list=ls(all=TRUE))
require(Rpplane); 
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# Function to compute state variable derivatives. 
# Note that the state variables are sent in as two separate arguments, 
# not as a vector, and that time t is not an argument.
# The model here is a rescaled Rosenzweig-MacArthur model
# dx/d(tau) = rx(1-x)-xy/(B+x)
# dy/d(tau) = axy/(B+x)-y  
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>


RosMac=function(n1,n2,parms) {
    r1=parms[1]; r2=parms[2]; 
    a1=parms[3]; a2=parms[4];
    b=parms[5]; K=parms[6]; 
    dx1=r1*n1*(1-n1/K) - n2*(a1*n1)/(b+n1);
    dx2=n2*(a2*n1)/(b+n1)-r2*n2;	
    return(c(dx1,dx2)) ;
}

parms=c(1,1,2,2,200,5000) 
Rpplane(fun=RosMac, xlim=c(-0.02,15000),ylim=c(-0.01,15000),parms=parms,x_lab="Prey, K=5000",y_lab="Predator")


