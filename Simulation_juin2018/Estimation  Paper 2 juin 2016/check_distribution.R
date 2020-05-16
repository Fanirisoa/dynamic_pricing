rm(list=ls())
gc()
setwd("C:/Users/Leacalin/Desktop/ESTIMATION/Estim Pro/Normal") 

#################  Rolling  ##################################
library(zoo)

################# The data ##################################
SP500 = read.table("SP500.csv",header=TRUE,sep=";")
SP500 = as.numeric(SP500[,2])

################# The Log return ##################################
ret0_SP500 = diff(log(SP500))


################# the rolling annual returns ##################################
X <- rollsum(ret0_SP500,356)
n<-length(X)
n

Fn <- ecdf(X)

#################  Estimation  ##################################

###### Using normal distribution as initial  ######
library(MASS) 

P<-function(a,e) {(( (1/6)*(1^3) )-((a/2)*(1^2)) +(((((a)^2)/2)+e)*1))}
D<-function(u,mu,sigma) {(1/((sigma)*sqrt(2*pi)))*exp(-(((u-mu)^2)/(2*(sigma^2))))}
K<- function(u,a,e) {(((1/2)*(u^2))- (a*u) +(((a^2)/2)+e))}
H<-function(u,mu,sigma){pnorm(u,mu,sigma)}
Fprim<- function(u,a,e,mu,sigma) (1/P(a,e))*(D(u,mu,sigma))*(K(H(u,mu,sigma),a,e))

mydt <- function(x,a,e,mu,sigma) Fprim(x,a,e,mu,sigma) 
mydt(1,1,1,1,1)
fitdistr(X, mydt, list(a =0.36, e = 0, mu=0, sigma=1), lower =c(0,0,-6,0))



###### I define my density as in the gumbel ######
library(fitdistrplus)

ds <- function(x,a,e,mu,sigma) mydt(x,a,e,mu,sigma)
###### I define my CDF function as in the gumbel ######

ps<- function(q,a,e,mu,sigma) (((1/6)*((H(q,mu,sigma))^3))-((a/2)*((H(q,mu,sigma))^2))+( ((((a)^2)/2)+e)*(H(q,mu,sigma))))/(((1/6)*(1^3))-((a/2)*(1^2))+(((((a)^2)/2)+e)*1))

###### I define my quantile function as in the gumbel ######

qs<-function(p,a,e,mu,sigma) optimize(function(z)(cdf(z)-p)^2,c(-10,10))$minimum
qs(1,1,1,1,1)
###### The MLE estimation using fitdist and mledist ######
fgu <- fitdist(X, "s", start = list(a = 0.035, e = 0.005, mu=-0.52, sigma=1))
fgu
summary(fgu)
gofstat(fgu)
plot(fgu)

plot(fgu, demp = TRUE)
plot(fgu, histo = FALSE, demp = TRUE)
cdfcomp(fgu, addlegend=FALSE)
denscomp(fgu, addlegend=FALSE)
ppcomp(fgu, addlegend=FALSE)
qqcomp(fgu, addlegend=FALSE)

mledist(X, "s", start = list(a = 0.035, e = 0.005, mu=-0.52, sigma=1))


###### Using normal distribution as initial  ######
library(MASS) 

P<-function(a,e) {(( (1/6)*(1^3) )-((a/2)*(1^2)) +(((((a)^2)/2)+e)*1))}
D<-function(u,mu,sigma) {(1/((sigma)*sqrt(2*pi)))*exp(-(((u-mu)^2)/(2*(sigma^2))))}
K<- function(u,a,e) {(((1/2)*(u^2))- (a*u) +(((a^2)/2)+e))}
H<-function(u,mu,sigma){pnorm(u,mu,sigma)}
Fprim<- function(u,a,e,mu,sigma) (1/P(a,e))*(D(u,mu,sigma))*(K(H(u,mu,sigma),a,e))

mydt <- function(x,a,e,mu,sigma) Fprim(x,a,e,mu,sigma) 
mydt(1,1,1,1,1)
fitdistr(X, mydt, list(a =0.36, e = 0, mu=0, sigma=1), lower =c(0,0,-6,0))

###### Using logistic distribution as initial  ######
library(MASS) 

R<-function(a,e) ( (1/6)*(1^3) )-((a/2)*(1^2)) +(((((a)^2)/2)+e)*1)
DL<-function(u,mu,sigma) dlogis(u,mu,sigma)
PL<-function(u,mu,sigma)  plogis(u,mu,sigma)
FLprim<- function(u,a,e,mu,sigma) (1/R(a,e))*((((1/2)*((PL(u,mu,sigma))^2))- (a*(PL(u,mu,sigma))) +(((a^2)/2)+e)))*(DL(u,mu,sigma))


mydtL <- function(x,a,e,mu,sigma) FLprim(x,a,e,mu,sigma) 
mydtL(1,1,1,1,1)
fitdistr(X, mydtL, list(a =0.4, e = 0.01, mu=-0.4, sigma=0.5), lower =c(0,0,-6,0))

###### I define my density as in the gumbel ######

dsL <- function(x,a,e,mu,sigma) mydtL(x,a,e,mu,sigma)
###### I define my CDF function as in the gumbel ######

psL<- function(q,a,e,mu,sigma) (((1/6)*((PL(q,mu,sigma))^3))-((a/2)*((PL(q,mu,sigma))^2))+( ((((a)^2)/2)+e)*(PL(q,mu,sigma))))/(((1/6)*(1^3))-((a/2)*(1^2))+(((((a)^2)/2)+e)*1))

###### I define my quantile function as in the gumbel ######

qsL<-function(p,a,e,mu,sigma) optimize(function(z)(cdf(z)-p)^2,c(-10,10))$minimum
qsL(1,1,1,1,1)
###### The MLE estimation using fitdist and mledist ######
fguL <- fitdist(X, "sL", start = list(a = 0.035, e = 0.005, mu=-0.52, sigma=1))
fguL
summary(fguL)
gofstat(fguL)
plot(fguL)
?gofstat
plot(fguL, demp = TRUE)
plot(fguL, histo = FALSE, demp = TRUE)

cdfcomp(fguL, addlegend=FALSE)
denscomp(fguL, addlegend=FALSE)
ppcomp(fguL, addlegend=FALSE)
qqcomp(fguL, addlegend=FALSE)

windows()
denscomp(list(fguL,fgu),demp = TRUE,legendtext=c("Distorted Logistic", "Distorted  Normal"),xlab="S&P 500 Rolling Returns Index ",fitcol =c("red","blue"),fitlty =c(1, 1))
denscomp
mledist(X, "s", start = list(a = 0.035, e = 0.005, mu=-0.52, sigma=1))

plot(fgu, histo = FALSE, demp = TRUE)
cdfcomp(fgu, addlegend=FALSE)
denscomp(fgu, addlegend=FALSE)
ppcomp(fgu, addlegend=FALSE)
qqcomp(fgu, addlegend=FALSE)



gofstat(fguL)$ad
gofstat(fguL)$adtest
gofstat(fguL)$cvm
gofstat(fguL)$cvmtest
gofstat(fguL)$ks
gofstat(fguL)$kstest



gofstat(fgu)$ad
gofstat(fgu)$adtest
gofstat(fgu)$cvm
gofstat(fgu)$cvmtest
gofstat(fgu)$ks
gofstat(fgu)$kstest

library(ADGofTest)

a  <- 0.43228237 
e <- 0.01645181 
mu <- -0.42162417 
sigma <-0.60616651
ad.test(X, psL, a, e, mu, sigma )
ks.test(X, psL, a, e, mu, sigma )
psLu<-function(u) psL(u,a,e, mu,sigma)
pslu<-psLu(X)

library(kolmim)
ks.test.imp(X, psL, a, e, mu, sigma)

(1.36/sqrt(n))*sqrt(2)

a  <- 0.32718149
e <- 0.08944526 
mu <- -0.52648491 
sigma <-1.37259251
ad.test(X, ps, a, e, mu, sigma )
ks.test(X, ps, a, e, mu, sigma )
psLu<-function(u) psL(u,a,e, mu,sigma)
pslu<-psLu(X)




plot(x, pkolmim(x, n), type="l")


n

qnorm # this is what you did without having a good enough answer
methods(qnorm) # Next step, ask for the method: 'princomp.default'
getAnywhere('qnorm.default')
stats:::qnorm.default
#debug(gofstat)
#undebug(gofstat) 
