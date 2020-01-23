rm(list=ls(all=TRUE))
require(Rpplane); 

# Function to compute state variable derivatives. 
# Note that the state variables are sent in as two separate arguments, 
# not as a vector, and that time t is not an argument. 

coralmucus=function(B,P,parms) {
	r.B=parms[1]; r.P=parms[2]; lambda=parms[3]; 
	S=1-B-P;
	dBdt=r.B*B*S - B;
	dPdt=r.P*P*S*exp(-lambda*B)- P; 
	return(c(dBdt,dPdt)) 
}

r.B=2; r.P=4; lambda=2; 
parms=c(r.B,r.P,lambda)
Rpplane(coralmucus,c(-0.02,0.8),c(-0.02,0.9),parms=parms); 


