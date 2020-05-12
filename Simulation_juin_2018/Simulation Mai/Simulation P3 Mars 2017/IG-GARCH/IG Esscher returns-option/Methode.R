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


#####################################################
###         Source function to use            #######
#####################################################
source("C:/Users/e0g411k03dt/Desktop/Simulation P3 Mars 2017/IG-GARCH/IG Esscher returns-option/LoglikMix.R")
source("C:/Users/e0g411k03dt/Desktop/Simulation P3 Mars 2017/IG-GARCH/IG Esscher returns-option/LoglikOprion.r")
source("C:/Users/e0g411k03dt/Desktop/Simulation P3 Mars 2017/IG-GARCH/IG Esscher returns-option/LoglikReturn.r")

#####################################################
###                 Data set                  #######
#####################################################
Data.N=Data.N2

Data.N=Data.N2[-c(506,1462,1638,1645),]

 
#####################################################
###         Parameters of the model           #######
#####################################################
##w=para_h[1]; b=para_h[2]; a=para_h[3];  c= para_h[4]; neta=para_h[5] ; nu=para_h[6]  

###   Initial parameter  ####

para_h1<-c(4.238940e-06 , 2.058376e-03  ,3.317425e+03 , 5.058743e-05, -8.281782e-03 , 1.2584e+02  )  ## RMSE2$rmse :   RMSE3$rmse :  

para_h<-c(4.238940e-06 , 2.058376e-03  ,3.317425e+03 , 5.058743e-05, -8.281782e-03 , 1.2584e+02 )   ## RMSE2$rmse :0.05435837 RMSE3$rmse : 0.06742708
###   Solution 
para_h<-c( 9.832251e-06 , 1.215820e-03 , 3.317425e+03 , 4.543874e-05 , -7.531312e-03 , 1.258401e+02 ) 

#####################################################
###               Volatility                  #######
#####################################################

ts.vol=h(para_h,Data.ret)
ts.plot(ts.vol, col = "steelblue", main = "IG Garch Model",xlab="2009",ylab="Volatility")
grid()

#####################################################
###              LOg values                   #######
#####################################################
start.time <- Sys.time()
ILK=IGGARCH_likelihood_Mix(para_h,Data.ret, Data.N,Data.returns)
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken

ILK


#####################################################
###      Optimization  of the model           #######
#####################################################
start.time <- Sys.time()
Sol=optim(para_h,IGGARCH_likelihood_Mix ,Data.ret=Data.ret, Data.N = Data.N,Data.returns=Data.returns, method="Nelder-Mead",control = list(maxit = 5000))
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken

para_h1<-Sol$par

############################################################
####                        RMSE                          ##
############################################################
start.time <- Sys.time()
RMSE1=RMSE(para_h1,Data.ret,Data.N)
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken
RMSE1


#####################################################
###    Load both data.contract and data.ret   #######
#####################################################
load("DataPrice20112012.Rdata")

############################################################
####              RMSE    out-sample                      ##
############################################################
Data.N=Data.N2
start.time <- Sys.time()
RMSE2=RMSE(para_h1,Data.ret,Data.N)
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken

RMSE2$rmse


############################################################
####                       Compare VIX                    ##
############################################################
source("C:/Users/e0g411k03dt/Desktop/Simulation P3 Mars 2017/IG-GARCH/IG Esscher returns-option/Comparing_VIX_oti_ret_Ess.r")

start.time <- Sys.time()
C_VIX= Compa_vix(para_h1,Data.returns)
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken

C_VIX

#####################################################
###         Implicite volatility              #######
#####################################################

Ip <- function(para_h,Data.ret, Data.N )
{ 
  T=Data.N$T       ####  Time to maturity expressed in terms of years in terms of days
  S=Data.N$S       ####  Prix du sous-jacent: Data.contract$S
  K=Data.N$K       ####  Strike  Prix d'exercice: data$strike
  r=Data.N$r       ####  Interest rate Data.contract$r
  C=Data.N$C       ####  Call price
  d=Data.N$d       ####  Call dividende
  
  Ip <- rep(NA, length(C))
  for (i in 1:length(C)){
    Ip[i] = implied.vol(S[i], K[i], T[i], r[i], C[i],d[i], type="C")
  }
  return(Ip)
}
start.time <- Sys.time()
Ipv=Ip(para_h,Data.ret,Data.N)
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken


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
