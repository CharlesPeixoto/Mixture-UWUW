rm(list = ls()) 

### Checking the moments of the unit Weibull distribution
################################################################
tau=0.5 #### setting the distribution parameters for conference purposes
mu1=0.6
beta1 = 1.5
####################################################
### Creating a function to calculate the density ###
####################################################
f_UW<-function(x){
  beta1*log(tau)/(x*(log(mu1)))*(log(x)/log(mu1))^(beta1-1)*tau^((log(x)/log(mu1))^(beta1))
    }

integrate(f_UW,0,1)

####################################################
### Creating a function to calculate the accumulated ###
####################################################
F_UW<-function(x){
 tau^((log(x)/log(mu1))^(beta1))
}

y=.9
integrate(f_UW,0,y)
F_UW(y)

# sth numerical moment
s = 5
mh_UW<-function(x){
  x^s*f_UW(x)
}

R = 15
z = beta1*(-(log(tau))/((-log(mu1))^beta1))
mu = vector()
k = 0
for( k in 0:R){
 mu[k+1] = (1/factorial(k))*
   ((log(tau))^(k))/
   ((-log(mu1))^(k*beta1))*
   (s^(-beta1*(k+1)))*
   gamma(beta1*(k+1))
}
q = z*sum(mu)
q ### sth moment of the WU distribution

integrate(mh_UW,0,1)

# sth  numerical incomplete moment
r = 0.8
mhi_UW<-function(x){
  x^s*f_UW(x)
}

###### sth incomplete moment of the WU distribution
for( k in 0:R){
  mu[k+1] = (1/factorial(k))*
    ((log(tau))^k)/
    ((-log(mu1))^(k*beta1))*
    (s^(-beta1*(k+1)))*
    pgamma(beta1*(k+1), - s*log(r))
}
h = z*sum(mu)
h

integrate(mhi_UW, 0, r)
  




