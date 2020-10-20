####################################################
###############   Unit Weibull Mixture #############
####################################################
####################################################
#Inicializations
fromx=0
tox=1
p = 0.4
tau = 0.5
#Probability density function
f_UWUW<- function(y)
{
  (p*b1*log(tau)/(y*(log(l1)))*(log(y)/log(l1))^(b1-1)*tau^((log(y)/log(l1))^(b1)))+
    (1-p)*(b2*log(tau)/(y*(log(l2)))*(log(y)/log(l2))^(b2-1)*tau^((log(y)/log(l2))^(b2)))
  }
###############################################################################
# Information for the construction of Figure 4a of Section 4.
yliminf=0
ylimsup=4.5

l1 = 0.2
l2 = 0.1
b1 = 0.9
b2 = 2.0
curve(f_UWUW,from=fromx, to=tox, add = FALSE, lty= 1,xlab = expression("y"),
      ylab = expression("f(y)"),ylim =c(yliminf,ylimsup), col =1, lwd = 1) 

l1 = 0.4
l2 = 0.2
b1 = 0.9
b2 = 2.0
curve(f_UWUW,from=fromx, to=tox, add = TRUE, lty=1,
      ylab = expression("f(y)"),ylim =c(yliminf,ylimsup), col = 2, lwd = 1)

l1 = 0.5
l2 = 0.3
b1 = 0.9
b2 = 2.5
curve(f_UWUW,from=fromx, to=tox, add = TRUE, lty=1,
      ylab = expression("f(y)"),ylim =c(yliminf,ylimsup), col = 3, lwd = 1)

l1 = 0.6
l2 = 0.4
b1 = 0.9
b2 = 5.0
curve(f_UWUW,from=fromx, to=tox, add = TRUE, lty=1, ylab = expression("f(y)"),
      ylim =c(yliminf,ylimsup), col = 4, lwd = 1)

l1 = 0.7
l2 = 0.5
b1 = 0.9
b2 = 5.0
curve(f_UWUW,from=fromx, to=tox, add = TRUE, lty=1, ylab = expression("f(y)"),xlab = expression("y"),
      ylim =c(yliminf,ylimsup), col = 5, lwd = 1)
  

legend("topleft", c(expression(paste(mu,1, " = 0.2, ", mu,2, " = 0.1, ", beta,2,' = 2.0 ')),
                     c(expression(paste(mu,1, " = 0.4, ", mu,2, " = 0.2, ", beta,2,' = 2.0 ')),
                       c(expression(paste(mu,1, " = 0.5, ", mu,2, " = 0.3, ", beta,2,' = 2.5 ')),
                         c(expression(paste(mu,1, " = 0.6, ", mu,2, " = 0.4, ", beta,2,' = 5.0 ')),
                           c(expression(paste(mu,1, " = 0.7, ", mu,2, " = 0.5, ", beta,2,' = 5.0'))))))),
       col = c(1,2,3,4,5),
       lty= c(1,1,1,1,1),
       lwd = c(1,1,1,1,1), bty="n", cex = 0.8)
###############################################################################
###############################################################################
# Information for the construction of Figure 4b of Section 4.
yliminf=0
ylimsup=3.5
p = 0.6
tau = 0.5

l1 = 0.3
l2 = 0.1
b1 = 3.0
b2 = 0.5
curve(f_UWUW,from=fromx, to=tox, add = FALSE, lty= 1,xlab = expression("y"),
      ylab = expression("f(y)"),ylim =c(yliminf,ylimsup), col =1, lwd = 1) 

l1 = 0.4
l2 = 0.2
b1 = 3.0
b2 = 1.1
curve(f_UWUW,from=fromx, to=tox, add = TRUE, lty=1,
      ylab = expression("f(y)"),ylim =c(yliminf,ylimsup), col = 2, lwd = 1)

l1 = 0.5
l2 = 0.3
b1 = 3.0
b2 = 1.5
curve(f_UWUW,from=fromx, to=tox, add = TRUE, lty=1,
      ylab = expression("f(y)"),ylim =c(yliminf,ylimsup), col = 3, lwd = 1)

l1 = 0.6
l2 = 0.6
b1 = 3.0
b2 = 1.5
curve(f_UWUW,from=fromx, to=tox, add = TRUE, lty=1, ylab = expression("f(y)"),
      ylim =c(yliminf,ylimsup), col = 4, lwd = 1)

l1 = 0.7
l2 = 0.8
b1 = 3.0
b2 = 1.8
curve(f_UWUW,from=fromx, to=tox, add = TRUE, lty=1,
      ylab = expression("f(y)"),xlab = expression("y"),
      ylim =c(yliminf,ylimsup), col = 5, lwd = 1)


legend("topleft", c(expression(paste(mu,1, " = 0.3, ", mu,2, " = 0.1, ", beta,2,' = 0.5 ')),
                     c(expression(paste(mu,1, " = 0.4, ", mu,2, " = 0.2, ", beta,2,' = 0.8 ')),
                       c(expression(paste(mu,1, " = 0.5, ", mu,2, " = 0.3, ", beta,2,' = 1.1 ')),
                         c(expression(paste(mu,1, " = 0.6, ", mu,2, " = 0.6, ", beta,2,' = 1.5 ')),
                           c(expression(paste(mu,1, " = 0.7, ", mu,2, " = 0.8, ", beta,2,' = 1.8'))))))),
       col = c(1,2,3,4,5),
       lty= c(1,1,1,1,1),
       lwd = c(1,1,1,1,1), bty="n", cex = 0.8)

###############################################################################
###############################################################################
# Information for the construction of Figure 4c of Section 4.
yliminf=0
ylimsup=6
p = 0.8
tau = 0.5

l1 = 0.1
l2 = 0.2
b1 = 2.5
b2 = 1.5
curve(f_UWUW,from=fromx, to=tox, add = FALSE, lty= 1,xlab = expression("y"),
      ylab = expression("f(y)"),ylim =c(yliminf,ylimsup), col =1, lwd = 1) 

l1 = 0.2
l2 = 0.3
b1 = 3.0
b2 = 2.0
curve(f_UWUW,from=fromx, to=tox, add = TRUE, lty=1,
      ylab = expression("f(y)"),ylim =c(yliminf,ylimsup), col = 2, lwd = 1)

l1 = 0.3
l2 = 0.4
b1 = 3.1
b2 = 2.1
curve(f_UWUW,from=fromx, to=tox, add = TRUE, lty=1,
      ylab = expression("f(y)"),ylim =c(yliminf,ylimsup), col = 3, lwd = 1)

l1 = 0.4
l2 = 0.5
b1 = 4.0
b2 = 2.5
curve(f_UWUW,from=fromx, to=tox, add = TRUE, lty=1, ylab = expression("f(y)"),
      ylim =c(yliminf,ylimsup), col = 4, lwd = 1)

l1 = 0.5
l2 = 0.6
b1 = 5.0
b2 = 5.0
curve(f_UWUW,from=fromx, to=tox, add = TRUE, lty=1,
      ylab = expression("f(y)"),xlab = expression("y"),
      ylim =c(yliminf,ylimsup), col = 5, lwd = 1)


legend("topright", c(expression(paste(mu,1, " = 0.1, ", mu,2, " = 0.2, ", beta,1,' = 2.5, ', beta,2,' = 1.5 ')),
                     c(expression(paste(mu,1, " = 0.2, ", mu,2, " = 0.3, ", beta,1,' = 3.0, ', beta,2,' = 2.0 ')),
                       c(expression(paste(mu,1, " = 0.3, ", mu,2, " = 0.4, ", beta,1,' = 3.1, ', beta,2,' = 2.1 ')),
                         c(expression(paste(mu,1, " = 0.4, ", mu,2, " = 0.5, ", beta,1,' = 4.0, ', beta,2,' = 2.5 ')),
                           c(expression(paste(mu,1, " = 0.5, ", mu,2, " = 0.6, ", beta,1,' = 5.0, ', beta,2,' = 5.0'))))))),
       col = c(1,2,3,4,5),
       lty= c(1,1,1,1,1),
       lwd = c(1,1,1,1,1), bty="n", cex = 0.8)


###############################################################################
###############################################################################
# Information for the construction of Figure 4d of Section 4.
yliminf=0
ylimsup=5
p = 0.1
tau = 0.5

l1 = 0.6
l2 = 0.2
b1 = 4.0
b2 = 3.0
curve(f_UWUW,from=fromx, to=tox, add = FALSE, lty= 1,xlab = expression("y"),
      ylab = expression("f(y)"),ylim =c(yliminf,ylimsup), col =1, lwd = 1) 

l1 = 0.7
l2 = 0.3
b1 = 3.0
b2 = 5.0
curve(f_UWUW,from=fromx, to=tox, add = TRUE, lty=1,
      ylab = expression("f(y)"),ylim =c(yliminf,ylimsup), col = 2, lwd = 1)

l1 = 0.8
l2 = 0.4
b1 = 4.0
b2 = 5.5
curve(f_UWUW,from=fromx, to=tox, add = TRUE, lty=1,
      ylab = expression("f(y)"),ylim =c(yliminf,ylimsup), col = 3, lwd = 1)

l1 = 0.9
l2 = 0.5
b1 = 2.0
b2 = 4.0
curve(f_UWUW,from=fromx, to=tox, add = TRUE, lty=1, ylab = expression("f(y)"),
      ylim =c(yliminf,ylimsup), col = 4, lwd = 1)

l1 = 0.7
l2 = 0.5
b1 = 5.0
b2 = 6.0
curve(f_UWUW,from=fromx, to=tox, add = TRUE, lty=1,
      ylab = expression("f(y)"),xlab = expression("y"),
      ylim =c(yliminf,ylimsup), col = 5, lwd = 1)


legend("topright", c(expression(paste(mu,1, " = 0.6, ", mu,2, " = 0.2, ", beta,1,' = 4.0, ', beta,2,' = 3.0 ')),
                     c(expression(paste(mu,1, " = 0.7, ", mu,2, " = 0.3, ", beta,1,' = 3.0, ', beta,2,' = 5.0 ')),
                       c(expression(paste(mu,1, " = 0.8, ", mu,2, " = 0.4, ", beta,1,' = 4.0, ', beta,2,' = 5.5 ')),
                         c(expression(paste(mu,1, " = 0.9, ", mu,2, " = 0.5, ", beta,1,' = 2.0, ', beta,2,' = 4.0 ')),
                           c(expression(paste(mu,1, " = 0.7, ", mu,2, " = 0.5, ", beta,1,' = 5.0, ', beta,2,' = 6.0'))))))),
       col = c(1,2,3,4,5),
       lty= c(1,1,1,1,1),
       lwd = c(1,1,1,1,1), bty="n", cex = 0.8)


