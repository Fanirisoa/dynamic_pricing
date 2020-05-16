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
source("C:/Users/fanir/Desktop/Simulation_juin2018/IG-GARCH/IG_Esscher_returns/LoglikReturn.R")

#####################################################
###         Parameters of the model           #######
#####################################################
###   Initial parameter  ####
##  w=para_h[1]; b=para_h[2]; a=para_h[3];  c= para_h[4]; neta=para_h[5] ; nu=para_h[6]; 

para_h<-c(4.238940e-06 , 2.058376e-03  ,3.317425e+03 , 5.058743e-05, -8.281782e-03 , 1.2584e+02 )  ## RMSE2$rmse :    RMSE3$rmse :  0.04541123


para_h<-c(4.238940e-06 , 2.058376e-03  ,3.317425e+03 , 5.058743e-05, -8.281782e-03 , 1.2584e+02 )   ## RMSE2$rmse :0.05435837 RMSE3$rmse : 0.06742708
###   Solution 
para_h<-c( 9.832251e-06 , 1.215820e-03 , 3.317425e+03 , 4.543874e-05 , -7.531312e-03 , 1.258401e+02 ) 

para_h<-c( 9.918251e-06 , 1.015820e-03 , 3.3317425e+03 , 4.543874e-05 , -7.531312e-03 , 1.259401e+02 ) 


#####################################################
###               Volatility                  #######
#####################################################
ts.vol_P= shape_h_P(para_h, Data.returns) 
ts.plot(ts.vol_P, col = "steelblue", main = "IG Garch Model",xlab="2009",ylab="Volatility")
grid()


#####################################################
###              LOg values                   #######
#####################################################
start.time <- Sys.time()
ILK= IGGARCH_likelihood_ret(para_h, Data.returns)
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken

ILK

#####################################################
###      Optimization  of the model           #######
#####################################################

start.time <- Sys.time()
Sol=optim(para_h,IGGARCH_likelihood_ret,Data.returns=Data.returns, method="Nelder-Mead",control = list(maxit = 10000))
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken
Sol
para_h1<-Sol$par


# Standard error
Hess=fdHess(para_h1,IGGARCH_likelihood_ret, Data.returns=Data.returns)
S_e <- sqrt(diag(solve(nearPD(Hess$Hessian)$mat)))
S_e
############################################################
####            In sample   RMSE                          ##
############################################################
source("C:/Users/fanir/Desktop/Simulation_juin2018/IG-GARCH/IG_Esscher_returns/RMSE_function.R")

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
source("C:/Users/fanir/Desktop/Simulation_juin2018/IG-GARCH/IG_Esscher_returns/Comparing_VIX_ret_Ess.R")
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
