### Checking the moments of the unit Weibull distribution
library("gsl")
################################################################
tau=0.5 #### setting the distribution parameters for conference purposes
mu1=0.5
beta1 = 3.5
####################################################
### Creating a function to calculate the density ###
####################################################
f_UW<-function(x){
  beta1*log(tau)/(x*(log(mu1)))*(log(x)/log(mu1))^(beta1-1)*tau^((log(x)/log(mu1))^(beta1))
}

integrate(f_UW,0,1)

# sth numerical moment
s = 5
mh_UW<-function(x){
  x^s*f_UW(x)
}

R = 100
mu = vector()
for( k in 0:R){
  mu[k+1] = (s*log(mu1))^k/(factorial(k)*(-log(tau))^(k/beta1))*
    gamma(k/beta1+1)
}
q = sum(mu)
q ### sth moment of the UW distribution

integrate(mh_UW,0,1)

# sth  numerical incomplete moment
r = 0.8
mhi_UW<-function(x){
  x^s*f_UW(x)
}

vec1= vector()
###### sth incomplete moment of the WU distribution
for( k in 0:R){
  vec1[k+1] = (-s)^k*(-log(tau)/(-log(mu1))^beta1 )^(-k/beta1)*
    (gamma_inc(k/beta1 + 1, (log(tau)/(-log(mu1))^beta1)*(log(r))^beta1))/factorial(k)
}
h = sum(vec1)
h  #sth incomplete moment of the WU distribution

integrate(mhi_UW, 0, r)
  

