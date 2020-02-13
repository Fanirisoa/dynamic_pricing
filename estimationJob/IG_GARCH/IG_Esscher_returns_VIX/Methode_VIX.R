#####################################################
###              Load Data source             #######
#####################################################
setwd("C:/Users/fanir/Desktop/Simulation_juin2018/Data")  

#####################################################
###             Clean the repertoir           #######
#####################################################
rm(list=ls())
gc()
library(compiler)
enableJIT(1)
enableJIT(3)
library("fBasics")
#library("pracma")
library("numDeriv")
library("nlme")
library("Matrix")

#####################################################
###    Load both data.contract and data.ret   #######
#####################################################
load("DataPrice20092010.Rdata")

#####################################################
###                 Data set                  #######
#####################################################
#Data.N=Data.N2

Data.N=Data.N2[-c(506,1462,1638,1645),]


#####################################################
###         Source function to use            #######
#####################################################
source("C:/Users/fanir/Desktop/Simulation_juin2018/IG-GARCH/IG_Esscher_returns_VIX/LoglikReturn.R")
source("C:/Users/fanir/Desktop/Simulation_juin2018/IG-GARCH/IG_Esscher_returns_VIX/Loglik_VIX_lin.r")
source("C:/Users/fanir/Desktop/Simulation_juin2018/IG-GARCH/IG_Esscher_returns_VIX/LogMixte.r")



#####################################################
###         Parameters of the model           #######
#####################################################
###   Initial parameter  ####
##    w=para_h[1]; b=para_h[2]; a=para_h[3];  c= para_h[4]; neta=para_h[5] ; nu=para_h[6]; ro=para_h[7]

para_h<-c(4.238940e-06 , 2.058376e-03  ,3.317425e+03 , 5.058743e-05, -8.281782e-03 , 1.2584e+02,  9.84599e-01 )  ## RMSE2$rmse :0.04354643  RMSE3$rmse : 0.05756848

##   solution  ####
para_h<-c(1.016620e-05,  2.041894e-03,  3.317506e+03,  4.501851e-05, -7.465712e-03,  1.258399e+02,  9.945561e-01) 

para_h<-c(9.817584e-06 , 1.215612e-03 , 3.322312e+03 , 4.542147e-05, -7.531277e-03,  1.258401e+02,9.94599e-01 ) 

para_h=c(1.206116e-06,  2.305289e-03,  3.317425e+03,  4.902499e-05, -7.972150e-03,  1.258400e+02,9.94599e-01 ) 

para_h=c(2.234137e-06,  2.318400e-03,  3.317424e+03,  4.894911e-05, -7.955236e-03,  1.258397e+02,  9.978291e-01)

#####################################################
###         Volatility and  Price             #######
#####################################################

ts.vol_P= shape_h_P(para_h, Data.returns) 
ts.plot(ts.vol_P, col = "steelblue", main = "IG Garch Model",xlab="2009",ylab="Volatility")
grid()


ts.vol_P= shape_h_P(para_h, Data.returns) 
ts.plot(ts.vol_P, col = "steelblue", main = "IG Garch Model",xlab="2009",ylab="Volatility")
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

# Standard error
Hess=fdHess(para_h1,IGGARCH_likelihood_MixViX, Data.returns=Data.returns)
S_e <- sqrt(diag(solve(nearPD(Hess$Hessian)$mat)))
S_e

############################################################
####                        RMSE                          ##
############################################################
source("C:/Users/fanir/Desktop/Simulation_juin2018/IG-GARCH/IG_Esscher_returns_VIX/RMSE_function.R")
start.time <- Sys.time()
RMSE1=RMSE(para_h1,Data.ret,Data.N)
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken

RMSE1$rmse
RMSE1$norm_rmse

############################################################
####                       Compare VIX                    ##
############################################################
source("C:/Users/fanir/Desktop/Simulation_juin2018/IG-GARCH/IG_Esscher_returns_VIX/Comparing_VIX_oti_ret_Ess.R")

start.time <- Sys.time()
C_VIX= Compa_vix(para_h1,Data.returns)
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken


#####################################################
###    Load both data.contract and data.ret   #######
#####################################################
load("DataPrice20112012.Rdata")


############################################################
####                        RMSE                          ##
############################################################
Data.N=Data.N2
start.time <- Sys.time()
RMSE2=RMSE(para_h1,Data.ret,Data.N)
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken


RMSE2$rmse
RMSE2$norm_rmse



