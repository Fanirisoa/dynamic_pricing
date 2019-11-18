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

source("C:/Users/fanir/Desktop/Simulation_juin2018/NGARCH/N Esscher returns-VIX_NonCentre/NGARCH_returns_loglike.R")
source("C:/Users/fanir/Desktop/Simulation_juin2018/NGARCH/N Esscher returns-VIX_NonCentre/Log_Mixte_VIX_Ret.r")
source("C:/Users/fanir/Desktop/Simulation_juin2018/NGARCH/N Esscher returns-VIX_NonCentre/Loglik_VIX_NGARCH.r")

#####################################################
###         Parameters of the model           #######
#####################################################
###   Initial parameter  ####
##      a0=para_h[1]; b1=para_h[2]; a1=para_h[3];  gama= para_h[4]; lambda= para_h[5]; a=para_h[6]; b=para_h[7]  ; c=para_h[8]; d=para_h[9]   ; ro=para_h[10]



para_h<-c(2.062e-06 , 0.9339  ,0.03848 , 0.6348, 0.152 , 1.02 , 0.05735, 0.1578 , 1.26842, 0.95417)  ## RMSE2$rmse :    RMSE3$rmse :  0.04541123

#####################################################
###               Volatility                  #######
#####################################################
ts.vol_P= shape_vol_P(para_h, Data.returns) 
ts.plot(ts.vol_P, col = "steelblue", main = "IG Garch Model",xlab="2009",ylab="Volatility")
grid()


#####################################################
###              LOg values                   #######
#####################################################
start.time <- Sys.time()
ILK= NGARCH_likelihood_MixViX(para_h, Data.returns)
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken

ILK

#####################################################
###      Optimization  of the model           #######
#####################################################

start.time <- Sys.time()
Sol=optim(para_h,NGARCH_likelihood_MixViX,Data.returns=Data.returns, method="Nelder-Mead",control = list(maxit = 10000))
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken
Sol
para_h1<-Sol$par

para_h<-Sol$par
#####################################################
###          Part 3     RMSE Simulation       #######
#####################################################
###         Source function to use            #######
#####################################################
##source("C:/Users/e0g411k03dt/Desktop/Estimation  Paper 2 Mars 2016/Simulation VIX HN/VIX Heston N/Simulation MC VIX HN.r")
source("C:/Users/fanir/Desktop/Simulation_juin2018/NGARCH/N Esscher returns-VIX_NonCentre/Simulation_MC_Pricer_return_NGARCH.r")
source("C:/Users/fanir/Desktop/Simulation_juin2018/NGARCH/N Esscher returns-VIX_NonCentre/Function_Pricer_return_NGARCH.r")
source("C:/Users/fanir/Desktop/Simulation_juin2018/NGARCH/N Esscher returns-VIX_NonCentre/RMSES_return_NGARCH.r")


N= 8^10
#Pricer(N,para_h1,Data.N)$P

############################################################
####                        RMSE                          ##
############################################################

start.time <- Sys.time()
RMSE2=RMSEsim(para_h1,Data.N)
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken

RMSE2$rmse
RMSE2$P
#####################################################
###    Load both data.contract and data.ret   #######
#####################################################
load("DataPrice20112012.Rdata")

Data.N=Data.N2
############################################################
####                        RMSE                          ##
############################################################


start.time <- Sys.time()
RMSE2010=RMSEsim(para_h1,Data.N)
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken

RMSE2010$rmse

############################################################
####                       Compare VIX                    ##
############################################################
source("C:/Users/fanir/Desktop/Simulation_juin2018/NGARCH/N Esscher returns-VIX_NonCentre/Comparing_VIX_oti_ret_Ess.R")
start.time <- Sys.time()
C_VIX= Compa_vix(para_h1,Data.returns)
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken

C_VIX

#####################################################
###    Load both data.contract and data.ret   #######
#####################################################
load("DataPrice20112012.Rdata")

############################################################
####           out sample   RMSE                          ##
############################################################
Data.N=Data.N2

start.time <- Sys.time()
RMSE2=RMSE(para_h1,Data.ret,Data.N)
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken

RMSE2$rmse
