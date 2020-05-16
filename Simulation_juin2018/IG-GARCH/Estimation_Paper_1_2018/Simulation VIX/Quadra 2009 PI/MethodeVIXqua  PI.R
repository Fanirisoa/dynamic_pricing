rm(list=ls())
gc()
library(maxLik)
library(compiler)
enableJIT(1)
enableJIT(3)

#####################################################
###    Load both data.contract and data.ret   #######
#####################################################
load("DataPrice2009.Rdata")

load("Dataprice2010.Rdata")
#####################################################
###         Parameters of the model           #######
#####################################################
# para_h<-c() set up the parameters (physical probability) of the model 
#w=para_h[1]; b=para_h[2]; a=para_h[3];  c= para_h[4]; neta=para_h[5] ; nu=para_h[6] ; PI=para_h[7] ; ro=para_h[8]
# 0.10
para_h<-c(1.890578e-06 , 6.587612e-01,  6.745762e+02 , 8.128110e-06, -5.028592e-03 , 1.944569e+02 , 1.320707e+00 , 9.895127e-01)

#######        Solution       #######
para_h<-c( 1.993036e-06 , 6.446413e-01  ,6.745918e+02,  8.488463e-06, -5.032286e-03 , 1.944558e+02  ,1.313840e+00  ,9.916137e-01)
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
ILK=IGGARCH_likelihood_MixVIX(para_h,Data.returns)
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken
IGGARCH_likelihood_ret(para_h, Data.returns) 
IGGARCH_likelihood_vix(para_h, Data.returns)
ILK
#####################################################
###      Optimization  of the model           #######
#####################################################

start.time <- Sys.time()
Sol=optim(para_h,IGGARCH_likelihood_MixVIX ,Data.returns=Data.returns, method="Nelder-Mead",control = list(maxit = 10000))
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken*60
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
RMSE1=RMSE(para_h,Data.ret,Data.contract)
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken


start.time <- Sys.time()
RMSE2=RMSE(para_h1,Data.ret,Data.N)
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken
RMSE2
#####################################################
###              LOg values                   #######
#####################################################
start.time <- Sys.time()
ILK=IGGARCH_likelihood_Mix(para_h,Data.ret)
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken
#####################################################
start.time <- Sys.time()
ILK=IGGARCH_likelihood_MixViX(para_h,Data.ret)
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken




#####################################################
###      Optimization  of the model           #######
#####################################################
start.time <- Sys.time()
Sol=optim(para_h,IGGARCH_likelihood_Mix ,Data.ret=Data.ret, method="Nelder-Mead",control = list(maxit = 5000))
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken
#####################################################
start.time <- Sys.time()
Sol=optim(para_h,IGGARCH_likelihood_MixViX ,Data.ret=Data.ret, method="Nelder-Mead",control = list(maxit = 5000))
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
####                    Vega                              ##
############################################################
start.time <- Sys.time()
Vega1=Vega(Data.contract, type="C")
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken


