##Checking the moments of the two-component unit Weibull mixture model
library("gsl")
####################################
tau = 0.5 
mu1 = 0.1
mu2 = 0.8
beta1 = 3
beta2 = 2
p = 0.4
####################################################
### Creating a function to calculate density ###
####################################################
f_UWUW<-function(x){
  (p*beta1*log(tau)/(x*(log(mu1)))*(log(x)/log(mu1))^(beta1-1)*tau^((log(x)/log(mu1))^(beta1)))+
    (1-p)*(beta2*log(tau)/(x*(log(mu2)))*(log(x)/log(mu2))^(beta2-1)*tau^((log(x)/log(mu2))^(beta2)))
}

integrate(f_UWUW,0,1) 

####################################################
### Creating a function to calculate the cumulative ###
####################################################
F_UWUW<-function(x){
  p*(tau^((log(x)/log(mu1))^(beta1)))+(1-p)*(tau^((log(x)/log(mu2))^(beta2)))
}

y=.9
integrate(f_UWUW,0,y)
F_UWUW(y)
 

# First analytical moment
R = 100
vec= vector()
vec1 = vector()
k = 0
s = 5
mh_UWUW<-function(x){
  x^s*f_UWUW(x)
}

for( k in 0:R){
  vec[k+1] = ((s*log(mu1))^k)/(factorial(k)*(-log(tau))^(k/beta1))*
    gamma((k/beta1 )+1)
  }

for( k in 0:R){
  vec1[k+1] = ((s*log(mu2))^k)/(factorial(k)*(-log(tau))^(k/beta2))*
    gamma((k/beta2)+1)
}

q = p*sum(vec) + (1-p)*sum(vec1)
q   ## moment of the two-component unit Weibull mixture model

integrate(mh_UWUW,0,1)

# sth  numerical incomplete moment
r = 0.8
mhi_UWUW<-function(x){
  x^s*f_UWUW(x)
}

vec2 = vector()
vec3 = vector()

a = - log(tau)/(-log(mu1))^beta1
g  =  - log(tau)/(-log(mu2))^beta2

# h-th analytical moment
for( k in 0:R){
  vec2[k+1] = (-s)^k*a^(-k/beta1)*
    (gamma_inc(k/beta1 + 1, a*(-log(r))^beta1))/factorial(k)
}

for( k in 0:R){
  vec3[k+1] = (-s)^k*g^(-k/beta2)*
    (gamma_inc(k/beta2 + 1, g*(-log(r))^beta2))/factorial(k)
}

h = p*sum(vec2) + (1-p)*sum(vec3)
h   ## sth moment of the two-component unit Weibull mixture model

integrate(mhi_UWUW,0,r)
