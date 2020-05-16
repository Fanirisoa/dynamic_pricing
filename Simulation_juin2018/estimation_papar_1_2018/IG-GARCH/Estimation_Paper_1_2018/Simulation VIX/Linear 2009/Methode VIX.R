rm(list=ls())
gc()
library(maxLik)
library(compiler)
enableJIT(1)
enableJIT(3)
#####################################################
###    Load both data.contract and data.ret   #######
#####################################################
## load("DataPrice20092010.Rdata")
##load("DataPrice20112012.Rdata")

load("DataPrice2009.Rdata")

load("Dataprice2010.Rdata")

#####################################################
###         Source function to use            #######
#####################################################
source("C:/Users/fanir/Desktop/Simulation_juin2018/IG-GARCH/Estimation_Paper_1_2018/Simulation VIX/Linear 2009/Loglik VIX lin.r")
source("C:/Users/fanir/Desktop/Simulation_juin2018/IG-GARCH/Estimation_Paper_1_2018/Simulation VIX/Linear 2009/LoglikReturn.r")
source("C:/Users/fanir/Desktop/Simulation_juin2018/IG-GARCH/Estimation_Paper_1_2018/Simulation VIX/Linear 2009/LogMixte.r")
source("C:/Users/fanir/Desktop/Simulation_juin2018/IG-GARCH/Estimation_Paper_1_2018/Simulation VIX/Linear 2009/RMSE function.r")
#source("C:/Users/e0g411k03dt/Desktop/Estimation Paper 1,juillet 2016/Simulation VIX/Linear 2009/Lin TESTGMM.r")


#####################################################
###                 Data set                  #######
#####################################################
Data.N=Data.N2


#####################################################
###         Parameters of the model           #######
#####################################################
###   Initial parameter  ####
##               w=para_h[1]; b=para_h[2]; a=para_h[3];  c= para_h[4]; neta=para_h[5] ; nu=para_h[6]; ro=para_h[7]


para_h<-c(4.238940e-06 , 2.058376e-03  ,3.317425e+03 , 5.058743e-05, -8.281782e-03 , 1.212284e+02,  9.974099e-01)
###   solution  ####
para_h<-c( 6.4945e-06 , 5.4045e-01  ,3.6359e+02,  1.1031e-05, -5.0328e-03 , 1.9460e+02  ,9.974099e-01 )

para_h<-c( 6.4945e-06 , 5.4045e-01  ,3.6359e+02,  1.1031e-05, -5.0328e-03 , 1.9460e+02  ,9.974099e-01 )

para_h<-c( 9.817584e-06,  1.215612e-03 , 3.322312e+03 , 4.542147e-05 ,-7.531277e-03, 1.258401e+02,9.974099e-01)

para_h<-c(2.998099e-06 , 2.097507e-03 , 3.317425e+03 ,4.879044e-05,-8.060392e-03 , 1.258397e+02 , 9.946212e-01)
#####################################################
###         Volatility and  Price             #######
#####################################################

ts.vol=h(para_h,Data.returns)
ts.plot(ts.vol, col = "steelblue", main = "IG Garch Model",xlab="2009",ylab="Volatility")
grid()

#####################################################
###              LOg values                   #######
#####################################################
start.time <- Sys.time()
ILK=IGGARCH_likelihood_MixViX(para_h,Data.returns)
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken
ILK
IGGARCH_likelihood_ret(para_h, Data.returns)
IGGARCH_likelihood_vix(para_h,Data.returns)
#####################################################
###      Optimization  of the model           #######
#####################################################

start.time <- Sys.time()
Sol=optim(para_h,IGGARCH_likelihood_MixViX ,Data.returns=Data.returns, method="Nelder-Mead",control = list(maxit = 10000))
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken
Sol
para_h1<-Sol$par

############################################################
####               Hessian Matrix                         ##
############################################################
start.time <- Sys.time()
Stand_error= Standard_errors(para_h1)
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken
Stand_error
############################################################
####                        RMSE                          ##
############################################################
start.time <- Sys.time()
RMSE1=RMSE(para_h1,Data.ret,Data.N)
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken
RMSE1$rmse

#####################################################
Data.N=Data.N3

start.time <- Sys.time()
RMSE2=RMSE(para_h1,Data.ret,Data.N1)
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken

RMSE2$rmse
RMSE2$error
############################################S#########
###      Optimization  of the model           #######
#####################################################
#####################################################
start.time <- Sys.time()
Sol=optim(para_h,IGGARCH_likelihood_MixViX ,Data.returns=Data.returns, method="Nelder-Mead",control = list(maxit = 5000))
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken


#####################################################
###         Price of the option               #######
#####################################################

start.time <- Sys.time()
P=Price_fft(para_h=para_h,Data.ret=Data.ret,Data.contract=Data.contract[1:l,])
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken
C=Data.contract$C
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
Ipv=Ip(para_h,Data.ret,Data.contract)
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken

############################################################
####               NLS estimation                         ##
############################################################
start.time <- Sys.time()
NLSMSE=optim(par=para_h,fn=RMSE,Data.ret=Data.ret,Data.contract=Data.contract[1,],method="Nelder-Mead",control = list(maxit = 900))
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
Vega1=Vega(Data.contract, type="C")
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken

