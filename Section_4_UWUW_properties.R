##Checking the moments of the two-component unit Weibull mixture model
####################################
tau = 0.5 
mu1 = 0.6
mu2 = 0.5
beta1 = 1.5
beta2 = 1.3
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

# h-th numerical moment
s = 5
mh_UWUW<-function(x){
  x^s*f_UWUW(x)
}

# First analytical moment
R = 15
mu = vector()
beta = vector()
k = 0
d = p* beta1*((-log(tau)/(-log(mu1))^beta1))
g = (1-p)* beta2*((-log(tau)/(-log(mu2))^beta2))

for( k in 0:R){
    mu[k+1] = (1/factorial(k))*(((log(tau))^k)/((-log(mu1))^(k*beta1)))*
      s^(-beta1*(k+1))*gamma(beta1*(k+1))
  }

for( k in 0:R){
  beta[k+1] = (1/factorial(k))*(((log(tau))^k)/((-log(mu2))^(k*beta2)))*
    s^(-beta2*(k+1))*gamma(beta2*(k+1))
}

q = d*sum(mu) + g*sum(beta)
q   ## moment of the two-component unit Weibull mixture model

integrate(mh_UWUW,0,1)


# sth  numerical incomplete moment
r = 0.9
mhi_UWUW<-function(x){
  x^s*f_UWUW(x)
}
muu = vector()
betaa = vector()
# h-th analytical moment
for( k in 0:R){
  muu[k+1] = (1/factorial(k))*(((log(tau))^k)/((-log(mu1))^(k*beta1)))*
    s^(-beta1*(k+1))*pgamma(beta1*(k+1), - s*log(r))
}

for( k in 0:R){
  betaa[k+1] = (1/factorial(k))*(((log(tau))^k)/((-log(mu2))^(k*beta2)))*
    s^(-beta2*(k+1))*pgamma(beta2*(k+1), - s*log(r))
}

h = d*sum(muu) + g*sum(betaa)
h   ## sth moment of the two-component unit Weibull mixture model

integrate(mhi_UWUW,0,r)
