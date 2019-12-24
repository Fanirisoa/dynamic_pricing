
###################################################################################
###               Clean the repertoir and laod all the library used         #######
###################################################################################
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
library(xts)

##################################################################################################
###              Load : Data source,    Parameters of the model,  function to use          #######
##################################################################################################
setwd("/Users/leafanirisoa/Documents/projetGit/dynamic_pricing/data_used") 
path = "/Users/leafanirisoa/Documents/projetGit/dynamic_pricing/estimationJob/IG_GARCH/IG_GARCH_RET_VIX_Us"

source(paste(path,"/parameters_settting.R",sep=""))
source(paste(path,"/LoglikReturn.R",sep=""))
source(paste(path,"/Loglik_VIX_Us.r",sep=""))
source(paste(path,"/LogMixte_Us.r",sep=""))
source(paste(path,"/RMSE_function_Us.R",sep=""))
source(paste(path,"/Table_RMSE.R",sep=""))

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
# Hess=fdHess(para_h1,IGGARCH_likelihood_MixViX, Data.returns=Data.returns)
# S_e <- sqrt(diag(solve(nearPD(Hess$Hessian)$mat)))


############################################################
####                        RMSE                          ##
############################################################
N_val = 2^10
start.time <- Sys.time()
RMSE1=RMSE(para_h1,Data.ret,Data.N,N_val)
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

##############################
###   Table Test GMM   #######
##############################
Table_RMSE(para_h=para_h,Data.ret=Data.ret, Data.N=Data.N)



#para_h<-c(4.238940e-06 , 2.058376e-03  ,3.317425e+03 , 5.058743e-05, -8.281782e-03 , 1.2584e+02, 1.110707e+00 ,  9.84599e-01 )  ## RMSE2$rmse :0.04684522  RMSE3$rmse :  
#para_h<-c(4.238940e-06 , 2.058376e-03  ,3.317425e+03 , 5.058743e-05, -8.281782e-03 , 1.2584e+02, 1.100707e+00 ,  9.83599e-01 )  ## RMSE2$rmse :0.04617248  RMSE3$rmse :  
para_h<-c(3.238940e-06 , 2.058376e-03  ,3.317425e+03 , 5.058743e-05, -8.281782e-03 , 1.2584e+02, 1.1003707e+00 ,  9.83599e-01 )   ## RMSE2$rmse :0.04620022  RMSE3$rmse : 0.06100658 
#para_h<-c(3.238940e-06 , 2.058376e-03  ,3.317425e+03 , 5.058743e-05, -8.281782e-03 , 1.2584e+02, 1.80707e+00 ,  9.83599e-01 )  ## RMSE2$rmse :0.1153094 RMSE3$rmse :  
