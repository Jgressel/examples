#1
r <- c(5,5,2,1,4)
#a
rshape <- sum(r)+2.6
rscale <- 1.4/(length(r)*1.4+1)
curve(dgamma(x,shape=rshape,scale=rscale),frame=0,to=12, col='blue', lwd=4, main='Exam 2 Problem 1a',  ylim=c(0,.6))
curve(dgamma(x,shape=2.6,scale=1.4),add=T, col='red', lwd=4)
legend(8,.4, c('posterior', 'prior'), lwd=c(4,4), col=c('blue','red'))

#b
#E(X) of prior= 
1.4 * 2.6

#c
#E(X) of posterior= 
rshape * rscale

#d
qgamma(c(.005,.995), shape=rshape, scale= rscale)
# So upper end= 5.75172

#2
updatemu <- function(data, sigma2, mumu, sigma2mu){
  xbar <- mean(data)
  n <- length(data)
  prec <- 1/sigma2
  precmu <- 1/sigma2mu
  meanpost <- (n*prec * xbar + precmu*mumu)/(n*prec+precmu)
  varpost <- 1/(n*prec +precmu)
  return(c(meanpost,varpost))
}

#a
g <- c(82.6, 80.4, 77.2, 79.5, 74.4, 80.8, 82.8, 80.1, 84.4, 74.7, 78.6, 79.4)
gpost <- updatemu(g,9,76,100)
curve(dnorm(x,gpost[1], sqrt(gpost[2])), lwd=3, col='blue', from=50, to=100, main='Exam 2 Problem 2a') #posterior
curve(dnorm(x,76,10), col='red', add=T, lwd=3)
legend(90,.3, c('posterior', 'prior'), lwd=c(4,4), col=c('blue','red'))

#b
quantile(gpost,c(.025,.975))

#c
#Yes, I would reject the null hypothesis that mu= 80 at 5% significance because we can see that 80 is not in our 95%  probability interval??

#d
updatesig2<- function(data, mu, prshape, prscale){
  n <- length(data)
  poshape <- n/2 + prshape
  ssx <- sum((data-mu)^2)
  poscale <- (2 * prscale)/(prscale * ssx + 2)
  out <- c(poshape,poscale)
  return(out)
}
igpdf <- function(x,sh,sc){
  (1/(gamma(sh)*sc^sh))*x^(-sh-1)*exp(-1/(x*sc))
}
g2post <- updatesig2(g,77,3.1,.02)
curve(igpdf(x, g2post[1], g2post[2]), col='blue', from=0, to=80, lwd=3, main='Exam 2 Problem 1d') # posterior for variance 
curve(igpdf(x,3.1, .02), add=T, col='red', lwd=3)
legend(60,.04, legend=c('posterior', 'prior'), col=c('blue','red'),lty=c(1,1),lwd=c(6,6))

#e
q <- 1/rgamma(10000, g2post[1], scale=g2post[2])
quantile(postinv,c(.05,.95))

#f
gibbs <- function(data, loops, mumu, sig2mu, psh, psc){
  out <- matrix(0,loops,2)
  out[1,1] <- mean(data)
  out[1,2] <- var(data)
  for (i in 2:loops){
    parmmu <- updatemu(data, out[i-1,2], mumu, sig2mu)
    out[i,1] <- rnorm(1,parmmu[1], sqrt(parmmu[2]))
    parms2 <- updatesig2(data, out[i,1], psh, psc)
    out[i,2] <- 1/rgamma(1,shape=parms2[1], scale=parms2[2])}
  return(out)}

g3post <- gibbs(g, 10000, 80, 200, 2.1, .005)
plot(density(g3post[,2]), lwd=3, col='blue', main='Exam 2 Problem 2f')

#g
plot(density(g3post[,1]), lwd=3, xlim=c(30,130), col='blue', main='Exam 2 Problem 2g')
gmprior <- rnorm(10000, 80, sqrt(200))
lines(density(gmprior), lwd=3, col='red')
legend(100,.15, c("Posterior","Prior"), lwd=c(3,3), col=c("blue","red"))

#h
quantile(g3post[,1], c(.025,.975))
#lower end is 76.16505

#i
priorgmu <-rnorm(10000, 80, sqrt(200))
priorgsig<-1/rgamma(10000,shape=2.1,scale=0.02)
priorpred<-rnorm(10000,priorgmu,sqrt(priorgsig))
g3post<- gibbs(g, 10000, 80, 200, 2.1, .005)
postpred<-rnorm(100000,g3post[,1],sqrt(g3post[,2]))
hist(g,breaks = 5, xlim = c(40, 120),freq=FALSE, main='Exam 2 Problem 2i')
lines(density(priorpred), lwd=3, col='blue')
lines(density(postpred), lwd=3, col='red')
legend(90,.09, c("Prior Predictive","Posterior Predictive"), lwd=c(3,3), col=c("blue","red"))

#3
#a
t <- c(83.6, 81.1, 78.2, 78.1, 84.2, 85.1, 76.1, 83.7, 87.4, 80.5, 81.1, 84.5)
gb1post<- gibbs(g, 100000, 81, 160, 2.01, .008)
gb2post<- gibbs(t, 100000, 81, 160, 2.01, .008)
plot(density(gb1post[,2]), lwd=3, col='blue', main='Exam 2 Problem 3a')
lines(density(gb2post[,2]), lwd=3, col='red')
legend(70,.04, c("Bridgewater Posterior var"," Taylormade Posterior var"), lwd=c(3,3), col=c("blue","red"))

#b
plot(density(gb1post[,1]), lwd=3, col='blue',xlim=c(70,100), main='Exam 2 Problem 3b')
lines(density(gb2post[,1]), lwd=3, col='red')
legend(86,.2, c("Bridgewater Posterior mean","Taylormade Posterior mean"), lwd=c(3,3), col=c("blue","red"))

#c
diff <- gb1post[,1] - gb2post[,1]
plot(density(diff), lwd=3, col='blue', main='Exam 2 Problem 3c')

#d
diff <- gb1post[,1] - gb2post[,1]
quantile(diff, c(.025,.975))
#upper is 1.882012

#e
#No, I would accept because 0 is in the interval. 

#4
#a
1/(1+9)

#b
curve(dbeta(x,12,139), col='blue', lwd=3, from=-.01, to=1, main='Exam 2 Problem 4b')
curve(dbeta(x,1,9), lwd=3, add=T, col='red')
legend(.6,10, c("Posterior","Prior"), lwd=c(3,3), col=c("blue","red"))

#c
12/(139+12)

#d
curve(dbeta(x,22,158), col='blue', lwd=3, from=-.01, to=1, main='Exam 2 Problem 4d')
curve(dbeta(x,1,9), lwd=3, add=T, col='red')
legend(.6,10, c("Posterior","Prior"), lwd=c(3,3), col=c("blue","red"))

#e
#E(X)=
22/(158+22)


#f
dairy <- rbeta(10000,12,139)
sheep <- rbeta(10000,22,158)
d <- dairy - sheep
quantile(d,c(.025,.975))
#No, i would not reject the null hypothesis. 0 is in the prob. interval

#5
#a
((1/6)*(1))/(((1/6)*1)+((1/3)*(3/5))+((1/2)*(1/5)))

#b
((1/6)*(1)*(1))/(((1/6)*(1)*(1))+((2/6)*(3/5)*(2/4))+((3/6)*(1/5)*(0)))
