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
source("C:/Users/fanir/Desktop/Simulation_juin2018/NGARCH/N Esscher returns/NGARCH_returns_loglike.R")

#####################################################
###         Parameters of the model           #######
#####################################################
###   Initial parameter  ####
##    a0=para_h[1]; b1=para_h[2]; a1=para_h[3];  gama= para_h[4]; lambda= para_h[5]; a=para_h[6]; b=para_h[7] ## ; c=para_h[5]; d=para_h[6] 


para_h<-c(1.603e-06 , 0.8447  ,0.06175 , 1.146, 0.03736 , 3.805 , -0.7806)  ## RMSE2$rmse :    RMSE3$rmse :  0.04541123

para_h<-c(2.157e-03 , 0.6358  ,0.0954 , 0.545, 0.08762 , 2.7878 , -1.07865)
###   Solution 
para_h<-c(1.157e-03 , 0.5358  ,0.0954 , 0.745, 0.08762 , 2.7878 , -1.02865)  ## RMSE2$rmse :    RMSE3$rmse :  0.007396033

para_h<-c(4.705257e-06  ,7.957262e-01  ,6.170762e-02 , 1.394690e+00 , 5.144851e-02,  1.795145e+00 ,-2.685911e-01)

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
ILK= NGARCH_likelihood_ret(para_h, Data.returns)
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken

#####################################################
###      Optimization  of the model           #######
#####################################################

start.time <- Sys.time()
Sol=optim(para_h,NGARCH_likelihood_ret,Data.returns=Data.returns, method="Nelder-Mead",control = list(maxit = 10000))
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken
Sol
para_h1<-Sol$par
para_h<-Sol$par

# Standard error
Hess=fdHess(para_h1,NGARCH_likelihood_ret, Data.returns=Data.returns)
S_e <- sqrt(diag(solve(nearPD(Hess$Hessian)$mat)))
S_e
#####################################################
###          Part 3 QML-RMSE Simulation       #######
#####################################################
###         Source function to use            #######
#####################################################
##source("C:/Users/e0g411k03dt/Desktop/Estimation  Paper 2 Mars 2016/Simulation VIX HN/VIX Heston N/Simulation MC VIX HN.r")
source("C:/Users/fanir/Desktop/Simulation_juin2018/NGARCH/N Esscher returns/Simulation_MC_Pricer_return_NGARCH.r")
source("C:/Users/fanir/Desktop/Simulation_juin2018/NGARCH/N Esscher returns/Function_Pricer_return_NGARCH.r")
source("C:/Users/fanir/Desktop/Simulation_juin2018/NGARCH/N Esscher returns/NGARCH_returns_loglike.R")

N= 8^10

############################################################
####                        RMSE                          ##
############################################################
source("C:/Users/fanir/Desktop/Simulation_juin2018/NGARCH/N Esscher returns/RMSES_return_NGARCH.R")
start.time <- Sys.time()
RMSE2=RMSEsim(para_h1,Data.N)
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken


RMSE2$rmse
RMSE2$norm_rmse

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
RMSE2010$norm_rmse

############################################################
####                       Compare VIX                    ##
############################################################
source("C:/Users/fanir/Desktop/Simulation_juin2018/NGARCH/N Esscher returns/Comparing_VIX_ret_Ess.R")

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
####           out sample   RMSE                          ##
############################################################
Data.N=Data.N2

start.time <- Sys.time()
RMSE2=RMSE(para_h1,Data.ret,Data.N)
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken

RMSE2$rmse
