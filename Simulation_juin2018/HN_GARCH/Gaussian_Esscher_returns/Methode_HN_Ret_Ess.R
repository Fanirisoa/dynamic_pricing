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
source("C:/Users/fanir/Desktop/Simulation_juin2018/HN_GARCH/Gaussian_Esscher_returns/Loglik_HN_Ret_P.R")

#####################################################
###         Parameters of the model           #######
#####################################################
##a0=para_h[1]; a1=para_h[2]; gama=para_h[3];  b1= para_h[4] ;  lamda0= para_h[5]   

###   Initial parameter  ####
para_h<-c(5.282379e-13, 2.252557e-05 ,8.143868e+00 ,9.154310e-01 ,1.026485e-0) ##  RMSE2$rmse : 0.02233918 RMSE3$rmse : 0.01818576

###   Solution 
para_h<-c(1.359738e-11, 2.218148e-04, 8.229466e+00, 6.204922e-01, 1.988846e+00)


para_h<-c(1.359738e-12, 2.218148e-04, 8.529466e+00, 4.204922e-01, 1.988846e+00) ##  RMSE2$rmse : 0.06380394 RMSE3$rmse : 0.01818576

#####################################################
###               Volatility                  #######
#####################################################

ts.vol_P= shape_vol_P (para_h, Data.returns) 
ts.plot(ts.vol_P , col = "steelblue", main = "IG Garch Model",xlab="2009",ylab="Volatility")
grid()


ts.vol_Q= shape_vol_Q (para_h, Data.returns) 
ts.plot(ts.vol_Q, col = "steelblue", main = "IG Garch Model",xlab="2009",ylab="Volatility")
grid()

#####################################################
###              LOg values returns           #######
#####################################################
start.time <- Sys.time()
ILK=Heston_likelihood_ret(para_h, Data.returns) 
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken

ILK


#####################################################
###      Optimization  of the model           #######
#####################################################
start.time <- Sys.time()
Sol=optim(para_h, Heston_likelihood_ret, Data.returns=Data.returns, method="Nelder-Mead",control = list(maxit = 5000))
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken

para_h1<-Sol$par
Sol

# Standard error
Hess=fdHess(para_h1,Heston_likelihood_ret, Data.returns=Data.returns)
S_e <- sqrt(diag(solve(nearPD(Hess$Hessian)$mat)))
S_e

############################################################
####            In sample   RMSE                          ##
############################################################
source("C:/Users/fanir/Desktop/Simulation_juin2018/HN_GARCH/Gaussian_Esscher_returns/RMSE_HN_Gaussian.R")

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
source("C:/Users/fanir/Desktop/Simulation_juin2018/HN_GARCH/Gaussian_Esscher_returns/Compare_Vix_HN_ess.R")

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
RMSE2$norm_rmse
############################################################
####            In sample   RMSE                          ##
############################################################
source("C:/Users/fanir/Desktop/Simulation_juin2018/HN_GARCH/Gaussian_Esscher_returns/week_HN_Ret.R")

start.time <- Sys.time()
RMSEwe=weRMSE(para_h1)
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken