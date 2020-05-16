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
library(fitdistrplus)

f1l <- fitdist(X, "logis")
summary(f1l)
gofstat(f1l)
plot(f1l)

f1n <- fitdist(X, "norm")
plot(f1n)
summary(f1n)

###### Using fitdistr from MASS ######
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

qnorm # this is what you did without having a good enough answer
methods(qnorm) # Next step, ask for the method: 'princomp.default'
getAnywhere('qnorm.default')
stats:::qnorm.default
#debug(gofstat)
#undebug(gofstat) 
