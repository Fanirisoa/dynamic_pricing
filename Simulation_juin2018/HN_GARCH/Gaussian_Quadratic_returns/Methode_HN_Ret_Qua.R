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
source("C:/Users/fanir/Desktop/Simulation_juin2018/HN_GARCH/Gaussian_Quadratic_returns/Loglik_Ret_HN_Q_Qua.R")

#####################################################
###         Parameters of the model           #######
#####################################################
##a0=para_h[1]; a1=para_h[2]; gama=para_h[3];  b1= para_h[4] ;  lamda0= para_h[5]   ; Pi=para_h[6]

###   Initial parameter  ####
para_h<-c(2.250026e-12, 1.502777e-06, 4.624124e+02, 6.500293e-01, 8.598965e+00, 1.007890e+00)



###   Solution 
para_h<-c(2.250026e-12, 1.502777e-06, 4.624124e+02, 6.500293e-01, 8.598965e+00, 1.007890e+00)
 
para_h<-c(1.359738e-12, 2.218148e-04, 8.529466e+00, 4.204922e-01, 1.988846e+00, 1.007890e+00) ##  RMSE2$rmse : 0.06380394 RMSE3$rmse : 0.01818576

#####################################################
###              LOg values returns           #######
#####################################################
start.time <- Sys.time()
ILK=Heston_likelihood_ret(para_h, Data.returns)
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken

ILK


# ####################################################
###      Optimization  of the model           #######
#####################################################
start.time <- Sys.time()
Sol=optim(para_h, Heston_likelihood_ret , Data.returns=Data.returns, method="Nelder-Mead",control = list(maxit = 5000))
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken

para_h1<-Sol$par
Sol

############################################################
####            In sample   RMSE                          ##
############################################################
source("C:/Users/e0g411k03dt/Desktop/Simulation P3 Mars 2017/HN-GARCH/Gaussian Quadratic returns/RMSE HN-Gaussian Qua.R")

start.time <- Sys.time()
RMSE1=RMSE(para_h1,Data.ret,Data.N)
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken

RMSE1$rmse


############################################################
####                       Compare VIX                    ##
############################################################
source("C:/Users/e0g411k03dt/Desktop/Simulation P3 Mars 2017/HN-GARCH/Gaussian Quadratic returns/Comparing  VIX_HN_GARCH.R")

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
