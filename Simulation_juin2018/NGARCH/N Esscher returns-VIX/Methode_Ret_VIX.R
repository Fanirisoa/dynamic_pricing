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
path = "/Users/leafanirisoa/Documents/projetGit/dynamic_pricing/estimationJob/NIG_GARCH_New_estimation/NGARCH_ret_vix"

source(paste(path,"/parameters_set.R",sep=""))
source(paste(path,"/Loglik_Return_NGARCH.R",sep=""))
source(paste(path,"/Loglik_Mix_NGARCH.R",sep=""))
source(paste(path,"/Loglik_VIX_NGARCH.R",sep=""))


### source(paste(path,"/Loglik_VIX_GJR.R",sep=""))
### source(paste(path,"/Fun_Pricer_Vix_ret_GJR.R",sep=""))
### source(paste(path,"/RMSE_VIX_GJR.R",sep=""))
### source(paste(path,"/MCSim_VIX_ret_GJR.R",sep=""))


#####################################################
###               Volatility  shape           #######
#####################################################
ts.vol_P= shape_vol_P (para_M, Data.returns) 
ts.plot(ts.vol_P , col = "steelblue", main = "NGARCH Model",xlab="2009",ylab="Volatility")
grid()



#####################################################
###              Log values returns           #######
#####################################################

start.time <- Sys.time()
ILK=NGARCH_likelihood_Mix(para_M,Data.ret, Data.N,Data.returns,N) 
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken
ILK

NGARCH_likelihood_ret(para_M, Data.returns)
NGARCH_likelihood_vix(para_M,Data.returns,Data.ret) 


#####################################################
###      Optimization  of the model           #######
#####################################################
start.time <- Sys.time()
Sol=optim(para_M,NGARCH_likelihood_Mix ,Data.ret=Data.ret, Data.N = Data.N,Data.returns=Data.returns, N=N, method="Nelder-Mead",control = list(maxit = 1000))
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken

para_M1<-Sol$par
para_M1
para_M
