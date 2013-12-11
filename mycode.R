### Jordan Gressel- 11/19/2013- Code Samples
#################################################1
#### Regression with Baseball data
b <- read.csv("http://blades.byu.edu/stat330data/baseballVARSELECT.csv", header=T)
head(b)
# I think the most important predictor variables are batting average, number of hits, number of home runs, and number of strike outs
# I feel that the interaction between the Number of hits and doubles, triples, and homeruns could be significant, since the more hits you get the more likley you are to get the other variables

#i
mod1<- lm(Salary.in.thousands.~BattingAvg, data=b)
#ii
mod2 <- lm(Salary.in.thousands.~ BattingAvg + OnBasePerc,data=b)
#iii
mod3 <- lm(Salary.in.thousands.~ RBI + OnBasePerc + NumberHomeRuns, data=b)
#iv
mod4 <- lm(Salary.in.thousands.~ NumberDoubles + NumberWalks + Errors, data=b)
#v
mod5 <- lm(Salary.in.thousands.~ NumberHits + NumberHomeRuns + Strike.outs, data=b)

summary(mod1)
summary(mod2)
summary(mod3)
summary(mod4)
summary(mod5)

# I would choose number 5 because all of the X's are the most significant compared to other models. Also, intuitivaley, all of the X's
# are the most relevant to performance. Mod1 (Batting avg) was significant, but later we see when we added another variable
# it was not longer significant. Mod4 was close to Mod5, but those x's aren't as good of indicators

mod0 <- lm(Salary.in.thousands.~1, data=b)
modall <- lm(Salary.in.thousands.~(BattingAvg +OnBasePerc +NumberRuns +NumberHits +NumberDoubles +NumberTriples +NumberHomeRuns +RBI +NumberWalks +Strike.outs +StolenBases +Errors)^2, data=b)
forward <- step(mod0, scope=list(lower=mod0,upper=modall), direction='forward', data=b)

summary(mod0)
step(modall)

#### Model assumptions
newmod <- lm(Salary.in.thousands. ~ RBI + NumberRuns + Strike.outs + 
               NumberHomeRuns + Errors + BattingAvg + StolenBases + NumberWalks + 
               NumberHits + NumberRuns*Errors + BattingAvg*StolenBases + 
               BattingAvg*NumberWalks + RBI*NumberHomeRuns + StolenBases*NumberWalks + 
               StolenBases*NumberHits, data = b)

summary(newmod)

plot(fitted(newmod), rstudent(newmod))
abline(h=0)
plot(newmod, which=2)
plot(rstudent(newmod), type='b')
abline(h=0)
plot(newmod, which=4)
#Only real violations are with the first plot(residual vs. fitted), so linearity and equal variance are violated.

library(car)
library(alr3)
inverseResponsePlot(newmod)

library(MASS)
boxcox(newmod)

# Refit previous models
mod1t<- lm(I(log(Salary.in.thousands.))~BattingAvg, data=b)
mod2t <- lm(I(log(Salary.in.thousands.))~ BattingAvg + OnBasePerc,data=b)
mod3t <- lm(I(log(Salary.in.thousands.))~ RBI + OnBasePerc + NumberHomeRuns, data=b)
mod4t <- lm(I(log(Salary.in.thousands.))~ NumberDoubles + NumberWalks + Errors, data=b)
mod5t <- lm(I(log(Salary.in.thousands.))~ NumberHits + NumberHomeRuns + Strike.outs, data=b)

summary(mod1t)
summary(mod2t)
summary(mod3t)
summary(mod4t)
summary(mod5t)

tmod0 <- lm(I(log(Salary.in.thousands.))~1, data=b)
tmodall <-lm(I(log(Salary.in.thousands.))~(BattingAvg +OnBasePerc +NumberRuns +NumberHits +NumberDoubles +NumberTriples +NumberHomeRuns +RBI +NumberWalks +Strike.outs +StolenBases +Errors)^2, data=b)
tforward <- step(tmod0, scope=list(lower=tmod0,upper=tmodall), direction='forward', data=b)

tnewmod <- lm(I(log(Salary.in.thousands.)) ~ NumberHits + RBI + Strike.outs + 
                NumberHomeRuns + BattingAvg + NumberWalks + Errors + OnBasePerc + 
                NumberHits:RBI + NumberHits:NumberWalks + RBI:NumberHomeRuns, data=b)

plot(fitted(tnewmod), rstudent(tnewmod))
abline(h=0)
plot(tnewmod, which=2)
plot(rstudent(tnewmod), type='b')
abline(h=0)
# The assumptions for the transformed new model look much better than the first model. Now with the residuals vs. fitted plot, there is a cloud shape, so linearity and equal variance are good. The qqnorm looks even straighter (normality is good) and there is independence



###################################################2
#Gibbs Sampling w/ Normal Distribution 
ccmu <- function(data, mumu, sig2, sig2mu, mu){
  ss <- sum(data-mu)^2
  -(1/(2*sig2))*ss - (1/(2*sig2mu))*(mu-mumu)^2
}
ccsig2 <- function(data,mu, a, b, sig2){
  n <- length(data)
  ss <- sum((data-mu)^2)
  first <- -(n/2) * log(sig2) - (1/(2*sig2))*ss
  second <- (-a-1) * log(sig2) - 1/(sig2*b)
  first + second
}

mg <- function(data,mumu,sig2mu, a, b, burn, loops, csmu, cssig2){ # a and b are priors
  out <- matrix(0, burn+loops,2) # burn + loops rows, 2 columns
  out[1,1] <- mean(data)
  out[1,2] <- var(data)
  cntmu <- 0 # count number of times you move
  cntsig2 <- 0
  for (i in 2:(burn+loops)){
    out[i,1] <- out[i-1,1]
    cand <- rnorm(1,out[i-1,1],) # only symmetric proposals
    r1 <- ccmu(data,mumu, out[i-1,2], sig2mu, cand) #ratio
    r2 <- ccmu(data,mumu, out[i-1,2], sig2mu, out[i-1,1])
    r <- r1 - r2
    if (r > log(runif(1,0,1))){
      out[i,1] <- cand
      if (i > burn){
        cntmu <-  cntmu + 1
      }
    }
    out[i,2] <- out[i-1,2]
    cand <- rnorm(1, out[i,2], cssig2) ##########out[i-2]
    if (cand>0){
      r1 <- ccsig2(data, out[i,1], a, b, cand)
      r2 <- ccsig2(data, out[i,1], a, b, out[i-1,2])
      r <- r1 - r2
      if (r > log(runif(1,0,1))){
        out[i,2] <- cand
        if (i > burn){
          cntsig2 <- cntsig2 + 1
        }
      }     
    }
  }
  print(cntmu/loops)
  print(cntsig2/loops)
  out <- out[(burn+1):(burn + loops),]
  return(out)
}

#example
fish <- c(8,12,10,14,2,0,0)
ans <- mg(fish, -5, 25, 2.01, .02, 200, 1000, 3, 10)



