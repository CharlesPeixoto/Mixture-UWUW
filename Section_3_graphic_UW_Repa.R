##############################################
###############    Unit Weibull  #############
##############################################
##############################################
#Clear the memory
rm(list = ls())
  
#Inicializations
fromx=0
tox=0.999
yliminf=0
ylimsup=2.7

tau = 0.5
#Probability density function
f_UW <- function(x)
{
  b*log(tau)/(x*(log(u)))*(log(x)/log(u))^(b-1)*tau^((log(x)/log(u))^(b))
}

###########################################################################
# Information for the construction of Figure 3a of Section 3.
b = 3.0
u = 0.2
curve(f_WU,from=fromx, to=tox, add = FALSE, lty= 1,xlab = expression("x"),
      ylab = expression("f(x)"),ylim =c(yliminf,ylimsup), col =1, lwd = 1) 

b = 3.0
u = 0.3
curve(f_WU,from=fromx, to=tox, add = TRUE, lty=1,
      ylab = expression("f(x)"),ylim =c(yliminf,ylimsup), col = 2, lwd = 1)

b = 3.0
u = 0.5
curve(f_WU,from=fromx, to=tox, add = TRUE, lty=1,
      ylab = expression("f(x)"),ylim =c(yliminf,ylimsup), col = 3, lwd = 1)

b = 3.0
u = 0.6
curve(f_WU, from=fromx, to=tox, add = TRUE, lty=1, ylab = expression("f(x)"),
      ylim =c(yliminf,ylimsup), col = 4, lwd = 1)

b = 3.0
u = 0.7
curve(f_WU,from=fromx, to=tox, add = TRUE, lty=1,
      ylab = expression("f(x)"),xlab = expression("x"),
      ylim =c(yliminf,ylimsup), col = 5, lwd = 1)


legend("top", c(expression(paste(beta,' = 0.2 ')),
                c(expression(paste(beta,' = 0.3 ')),
                  c(expression(paste(beta,' = 0.5 ')),
                    c(expression(paste(beta,' = 0.6 ')),
                      c(expression(paste(beta,' = 0.7 '))))))),
       col = c(1,2,3,4,5),
       lty= c(1,1,1,1,1),
       lwd = c(1,1,1,1,1), bty="n", cex = 0.8)
############################################################################
############################################################################
# Information for the construction of Figure 3b of Section 3.
b = 0.1
u = 0.6
curve(f_WU,from=fromx, to=tox, add = FALSE, lty= 1,xlab = expression("x"),
      ylab = expression("f(x)"),ylim =c(yliminf,ylimsup), col =1, lwd = 1)

b = 0.4
u = 0.6
curve(f_WU,from=fromx, to=tox, add = TRUE, lty=1,
      ylab = expression("f(x)"),ylim =c(yliminf,ylimsup), col = 2, lwd = 1)

b = 0.9
u = 0.6
curve(f_WU,from=fromx, to=tox, add = TRUE, lty=1,
      ylab = expression("f(x)"),ylim =c(yliminf,ylimsup), col = 3, lwd = 1)

b = 1.1
u = 0.6
curve(f_WU, from=fromx, to=tox, add = TRUE, lty=1, ylab = expression("f(x)"),
      ylim =c(yliminf,ylimsup), col = 4, lwd = 1)

b = 1.5
u = 0.6
curve(f_WU,from=fromx, to=tox, add = TRUE, lty=1,
      ylab = expression("f(x)"),xlab = expression("x"),
      ylim =c(yliminf,ylimsup), col = 5, lwd = 1)


legend("top", c(expression(paste(beta,' = 0.1 ')),
                    c(expression(paste(beta,' = 0.4 ')),
                      c(expression(paste(beta,' = 0.9 ')),
                        c(expression(paste(beta,' = 1.1 ')),
                          c(expression(paste(beta,' = 1.5 '))))))),
       col = c(1,2,3,4,5),
       lty= c(1,1,1,1,1),
       lwd = c(1,1,1,1,1), bty="n", cex = 0.8)
###########################################################################


