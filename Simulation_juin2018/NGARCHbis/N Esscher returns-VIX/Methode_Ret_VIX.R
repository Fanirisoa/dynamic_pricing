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
source("C:/Users/fanir/Desktop/Simulation_juin2018/NGARCHbis/N Esscher returns-VIX/NGARCH_returns_loglike.R")
source("C:/Users/fanir/Desktop/Simulation_juin2018/NGARCHbis/N Esscher returns-VIX/Log_Mixte_VIX_Ret.r")
source("C:/Users/fanir/Desktop/Simulation_juin2018/NGARCHbis/N Esscher returns-VIX/Loglik_VIX_NGARCH.r")

#####################################################
###         Parameters of the model           #######
#####################################################
###   Initial parameter  ####
##    a0=para_h[1]; b1=para_h[2]; a1=para_h[3];  gama= para_h[4]; lambda= para_h[5];  ro=para_h[6]## 
### Bon initial
para_h<-c(1.603e-06 , 0.7957  ,0.06175 , 1.146, 0.03736 , 0.95417)  


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

# Standard error
Hess=fdHess(para_h1,NGARCH_likelihood_MixViX, Data.returns=Data.returns)
S_e <- sqrt(diag(solve(nearPD(Hess$Hessian)$mat)))
S_e
#####################################################
###          Part 3     RMSE Simulation       #######
#####################################################
###         Source function to use            #######
#####################################################
##source("C:/Users/e0g411k03dt/Desktop/Estimation  Paper 2 Mars 2016/Simulation VIX HN/VIX Heston N/Simulation MC VIX HN.r")

source("C:/Users/fanir/Desktop/Simulation_juin2018/NGARCH/N Esscher returns/Simulation_MC_Pricer_return_NGARCH.r")
source("C:/Users/fanir/Desktop/Simulation_juin2018/NGARCH/N Esscher returns/Function_Pricer_return_NGARCH.r")
source("C:/Users/fanir/Desktop/Simulation_juin2018/NGARCH/N Esscher returns/RMSES_return_NGARCH.r")


N= 10

############################################################
####                        RMSE                          ##
############################################################

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
source("C:/Users/fanir/Desktop/Simulation_juin2018/NGARCHbis/N Esscher returns-VIX/Comparing_VIX_oti_ret_Ess.R")
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





