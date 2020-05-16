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
load("DataPrice2009.Rdata")

#####################################################
###                 Data set                  #######
#####################################################
Data.N=Data.N2

#####################################################
###         Source function to use            #######
#####################################################
source("C:/Users/fanir/Desktop/Simulation_juin2018/estimation_papar_1_2018/IG-GARCH/IG_U_shape_returns_VIX/LoglikReturn.R")
source("C:/Users/fanir/Desktop/Simulation_juin2018/estimation_papar_1_2018/IG-GARCH/IG_U_shape_returns_VIX/Loglik_VIX_Us.r")
source("C:/Users/fanir/Desktop/Simulation_juin2018/estimation_papar_1_2018/IG-GARCH/IG_U_shape_returns_VIX/LogMixte_Us.r")


#####################################################
###         Parameters of the model           #######
#####################################################
###   Initial parameter  ####
#   w=para_h[1]; b=para_h[2]; a=para_h[3];  c= para_h[4]; neta=para_h[5] ; nu=para_h[6] ; PI=para_h[7] ; ro=para_h[8]

para_h<-c(3.238940e-06 , 2.058376e-03  ,3.317425e+03 , 5.058743e-05, -8.281782e-03 , 1.2584e+02, 1.632707e+00 ,  9.83599e-01 )  ## RMSE2$rmse :0.04287557  RMSE3$rmse : 0.05661406

para_h<-c(3.238940e-06 , 2.058376e-03  ,3.317425e+03 , 5.058743e-05, -8.281782e-03 , 1.2584e+02, 1.003707e+00 ,  9.83599e-01 )   ## RMSE2$rmse :0.04620022  RMSE3$rmse : 0.06100658 

##   solution  ####
para_h<-c(9.876201e-06,  8.626756e-03,  3.317423e+03 , 4.491365e-05, -7.516695e-03 , 1.258370e+02,  1.097198e+00 , 9.962589e-01 )   

para_h<-c(7.151078e-06,  5.401796e-01 , 6.714926e+02 , 1.095374e-05 ,-5.142119e-03 , 1.935529e+02 , 1.245220e+00 , 9.914006e-01)
#####################################################
###         Volatility and  Price             #######
#####################################################

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
source("C:/Users/fanir/Desktop/Simulation_juin2018/estimation_papar_1_2018/IG-GARCH/IG_U_shape_returns_VIX/RMSE_function_Us.R")

start.time <- Sys.time()
RMSE1=RMSE(para_h1,Data.ret,Data.N)
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken

RMSE1$rmse
RMSE1$norm_rmse


#####################################################
###    Load both data.contract and data.ret   #######
#####################################################
load("DataPrice20112012.Rdata")

############################################################
####           out sample   RMSE                          ##
############################################################
Data.N=Data.N2
############################################################
####                       Compare VIX                    ##
############################################################
source("C:/Users/fanir/Desktop/Simulation_juin2018/IG-GARCH/IG_U_shape_returns_VIX/Comparing_VIX_Us.R")

start.time <- Sys.time()
C_VIX= Compa_vix(para_h1,Data.returns)
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken

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




#para_h<-c(4.238940e-06 , 2.058376e-03  ,3.317425e+03 , 5.058743e-05, -8.281782e-03 , 1.2584e+02, 1.110707e+00 ,  9.84599e-01 )  ## RMSE2$rmse :0.04684522  RMSE3$rmse :  
#para_h<-c(4.238940e-06 , 2.058376e-03  ,3.317425e+03 , 5.058743e-05, -8.281782e-03 , 1.2584e+02, 1.100707e+00 ,  9.83599e-01 )  ## RMSE2$rmse :0.04617248  RMSE3$rmse :  
para_h<-c(3.238940e-06 , 2.058376e-03  ,3.317425e+03 , 5.058743e-05, -8.281782e-03 , 1.2584e+02, 1.1003707e+00 ,  9.83599e-01 )   ## RMSE2$rmse :0.04620022  RMSE3$rmse : 0.06100658 
#para_h<-c(3.238940e-06 , 2.058376e-03  ,3.317425e+03 , 5.058743e-05, -8.281782e-03 , 1.2584e+02, 1.80707e+00 ,  9.83599e-01 )  ## RMSE2$rmse :0.1153094 RMSE3$rmse :  
