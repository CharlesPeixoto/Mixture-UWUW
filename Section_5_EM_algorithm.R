#Clear the memory
library(readxl)
library(AdequacyModel)

tau = 0.5  
R = 10000
semente = 271
set.seed(semente)
tempo.inicio = Sys.time()
iter = 0
      
## Sample size
vn = c(100,250,500)

### Parameters    
mu_1 = 0.3
mu_2 = 0.7
beta_1 = 2.0
beta_2 = 3.0
p_1 = 0.4
p_2 = (1 - p_1)

mu = c(mu_1, mu_2)
beta = c(beta_1, beta_2)
p = c(p_1, p_2)

## Quantile function of the mixture of distributions
rmixWei<-function(n, mu_1, mu_2, beta_1, beta_2, p_1){
  x <- rep(NA, n)
  for(l in 1: n){
    u1 <- runif(n)
    u2 <- runif(n)
    if(u1[l] < p_1) { x[l] <- (mu_1^((log(u2[l])/log(tau))^(1/beta_1)))
    }else{
      x[l] <- (mu_2^((log(u2[l])/log(tau))^(1/beta_2)))
    }
  }
  return(x)
}

k = 1 #indexing results in an array
mu1 = mu2 = beta1 = beta2 = p1 = rep(NA,R)
Mat_mu_beta = matrix(NA, length(vn), 11)

## Two-component unit Weibull mixture model (expressed in terms of density)
mist_Wei = function(x){
(p[1]*beta[1]*log(tau)/(x*(log(mu[1])))*(log(x)/log(mu[1]))^(beta[1]-1)*tau^((log(x)/log(mu[1]))^(beta[1])))+
p[2]*(beta[2]*log(tau)/(x*(log(mu[2])))*(log(x)/log(mu[2]))^(beta[2]-1)*tau^((log(x)/log(mu[2]))^(beta[2])))

}

### Mixture log-likelihood
veros <- function(param){
mu = c(param[1] , param[2])
beta = c(param[3] , param[4])
if((mu[1] > 0) && (mu[1] < 1) && (mu[2] > 0) && (mu[2] < 1) && (beta[1] >0) && (beta[2] >0) )
return(-sum(log((pi[1]*beta[1]*log(tau)/(x*(log(mu[1])))*(log(x)/log(mu[1]))^(beta[1]-1)*tau^((log(x)/log(mu[1]))^(beta[1])))+
                  pi[2]*(beta[2]*log(tau)/(x*(log(mu[2])))*(log(x)/log(mu[2]))^(beta[2]-1)*tau^((log(x)/log(mu[2]))^(beta[2]))))))
else return(-Inf)
}

## E-Step (Expression arising from the Bayes Rule)
fpost = function(x, j){
fp1 = p[j]*(beta[j]*log(tau)/(x*(log(mu[j])))*(log(x)/log(mu[j]))^(beta[j]-1)*tau^((log(x)/log(mu[j]))^(beta[j])))
fp2 = mist_Wei(x)
fp = fp1/fp2
return(fp)
}

## Lace for sample sizes
for(n in vn){

## Initial value
chute <- c(mu, beta)

## Monte Carlo loop
i= 0
while( i < R){

### Pseudo-random sample
x = rmixWei(n, mu_1, mu_2, beta_1, beta_2, p_1)

## Maximize Aspect Ratio (find the p-value)
for(j in 1 : 2){
pi[j] = mean(fpost(x, j))
}

## Estimation with ConstrOptim [Nelder-Mead]
estima <- constrOptim(chute , veros, NULL, method="Nelder-Mead",
    ui = rbind(c(1 ,0 ,0 ,0) ,c(0 ,1 ,0 ,0) ,c(0 ,0 ,1 ,0) ,c(0 ,0 ,0 ,1)),
    ci = c(0 ,0 ,0 ,0), hessian = FALSE , outer.iterations = 500)

if(estima$convergence == 0){
i = i + 1
mu1[i] <- estima$par[1]
mu2[i] <- estima$par[2]
beta1[i] <- estima$par[3]
beta2[i] <- estima$par[4]
p1[i] <- pi[1]
}

else{
iter = iter + 1
}
}

#EMV's
## Parameter estimates
media_mu_1 = mean(mu1)
media_mu_2 = mean(mu2)
media_beta_1 = mean(beta1)
media_beta_2 = mean(beta2)
media_p_1 = mean(p1)

## Variance of parameter estimates
var_mu_1 = var(mu1)
var_mu_2 = var(mu2)
var_beta_1 = var(beta1)
var_beta_2 = var(beta2)
var_p_1 = var(p1)

## Percent relative bias
vies_mu_1 = (media_mu_1  - mu_1)*100
vies_mu_2 = (media_mu_2 - mu_2)*100
vies_beta_1 = (media_beta_1 - beta_1)*100
vies_beta_2 = (media_beta_2 - beta_2)*100
vies_p_1 = (media_p_1 - p_1)*100

### Mean square error of the estimates
eqm_mu_1 = sd(mu1)^2 + ((vies_mu_1)/100)^2
eqm_mu_2 = sd(mu2)^2 + ((vies_mu_2)/100)^2
eqm_beta_1 = sd(beta1)^2 + ((vies_beta_1)/100)^2
eqm_beta_2 = sd(beta2)^2 + ((vies_beta_2)/100)^2
eqm_p_1 = sd(p1)^2 + ((vies_p_1)/100)^2

#Results

colnames(Mat_mu_beta) <- c("n", "vies_mu_1", "vies_mu_2", "vies_beta_1", "vies_beta_2", "vies_p_1", "eqm_mu_1", "eqm_mu_2", "eqm_beta_1", "eqm_beta_2", "eqm_p_1")
Mat_mu_beta[k,] <- c(n, vies_mu_1, vies_mu_2, vies_beta_1, vies_beta_2, vies_p_1, eqm_mu_1, eqm_mu_2, eqm_beta_1, eqm_beta_2, eqm_p_1) 

k = k + 1
}

## List with results
tempo.fim = Sys.time()
tempo.exec = tempo.fim- tempo.inicio
resultado = list(est_mu_beta = round(Mat_mu_beta,4), tempo=tempo.exec)

resultado

