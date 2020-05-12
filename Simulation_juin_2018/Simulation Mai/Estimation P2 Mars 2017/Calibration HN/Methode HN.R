rm(list=ls())
gc()
library(compiler)
enableJIT(1)
enableJIT(3)
library("fBasics")
#####################################################
###    Load both data.contract and data.ret   #######
#####################################################
load("DataPrice20092010.Rdata")

load("DataPrice20112012.Rdata")

#####################################################
###                 Data set                  #######
#####################################################
Data.N=Data.N2

Data.N=Data.N2[-c(506,1462,1638,1645),]
#####################################################
###         Source function to use            #######
#####################################################
##source("C:/Users/e0g411k03dt/Desktop/Estimation paper 2, juillet 2016/Calibration HN/Calibration HN Nogammastart.r")
source("C:/Users/e0g411k03dt/Desktop/Estimation P2 Mars 2017/Calibration HN/Calibration HN.r")
source("C:/Users/e0g411k03dt/Desktop/Estimation P2 Mars 2017/Calibration HN/Matrixe RMSE HN.r")


#####################################################
###         Parameters of the model           #######
#####################################################
###   Initial parameter  ##
##  a0=para_h[1]; a1=para_h[2]; gama=para_h[3];  b1= para_h[4] ;  lamda0= para_h[5]  
#para_h1<-c(1.180234e-12, 1.547729e-06, 4.550518e+02, 6.500111e-01,0.25764 )
#para_h<-c(5.02e-6, 1.32e-6, 421.39, 0.589,0.5)
#para_h<-c(5.436223e-06, 1.334501e-06, 4.634877e+02, 6.302493e-01,0.42273)

##  a0=para_h[1]; a1=para_h[2]; gamastar=para_h[3];  b1= para_h[4]  
para_h<-c(6.094e-09, 1.240e-01, 2.314e-02, 4.011e-01, 3.025e-02)  ## RMSE1$rmse : 0.1452448  RMSE3$rmse : 0.05582874


###   Solution 

#####################################################
###               Volatility                  #######
#####################################################

ts.vol=h(para_h,Data.ret)
ts.plot(ts.vol, col = "steelblue", main = "IG Garch Model",xlab="2009",ylab="Volatility")
grid()


############################################################
####               NLS estimation                         ##
############################################################
## start.time <- Sys.time()
## NLSMSE=optim(par=para_h1,fn=RMSEopti,Data.ret=Data.ret,Data.N=Data.N,method="Nelder-Mead",control = list(maxit = 1000))
## end.time <- Sys.time()
## time.taken <- end.time - start.time
## time.taken
## para_h1<- NLSMSE$par

start.time <- Sys.time()
NLSMSE=optim(par=para_h,fn=RMSE,Data.ret=Data.ret,Data.N=Data.N,method="Nelder-Mead",control = list(maxit = 1000))
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken

para_h1<- NLSMSE$par

############################################################
####                        RMSE                          ##
############################################################

start.time <- Sys.time()
ValRMSE=RMSE(para_h1,Data.ret,Data.N)
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken

ValRMSE$rmse
ValRMSE$Er



start.time <- Sys.time()
MRMSE=MatriceRMSE(para_h1,Data.N)
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken

############################################################
####               Hessian Matrix                         ##
############################################################
start.time <- Sys.time()
hess = hessian(func=RMSE, x=para_h,Data.ret=Data.ret,Data.contract=Data.contract)
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken

hessc <- hessian(func=RMSE, x=para_h, "complex",Data.ret=Data.ret,Data.contract=Data.contract)
all.equal(hess, hessc, tolerance = .Machine$double.eps)

############################################################
####                    Vega                              ##
############################################################
start.time <- Sys.time()
Vega1=Vega(Data.N, type="C")
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken


#####################################################
###         Volatility and  Price             #######
#####################################################

l=100

start.time <- Sys.time()
P=Price_fft(para_h=para_h,Data.ret=Data.ret,Data.contract=Data.N[1:l,])
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken
C=Data.N$C
Ca=C[1:l]
Pa=P[1:l]

ts.plot(Pa,ts(Ca), col =c("steelblue","red"), main = "Valuation with IG Garch Model",xlab="2009",ylab="Prices")
legend(175, 250,  c("C(t)","C*(t)"), col =c("steelblue","red"), lty = c(1, 1))


#####################################################
###         Implicite volatility              #######
#####################################################

Ip <- function(para_h,Data.ret,Data.contract)
{ 
  T=Data.contract$T       ####  Time to maturity expressed in terms of years in terms of days
  S=Data.contract$S       ####  Prix du sous-jacent: Data.contract$S
  K=Data.contract$K       ####  Strike  Prix d'exercice: data$strike
  r=Data.contract$r       ####  Interest rate Data.contract$r
  C=Data.contract$C       ####  Call price
  d=Data.contract$d       ####  Call dividende
  
  Ip <- rep(NA, length(C))
  for (i in 1:length(C)){
    Ip[i] = implied.vol(S[i], K[i], T[i], r[i], C[i],d[i], type="C")
  }
  return(Ip)
}
start.time <- Sys.time()
Ipv=Ip(para_h1,Data.ret,Data.N)
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken

