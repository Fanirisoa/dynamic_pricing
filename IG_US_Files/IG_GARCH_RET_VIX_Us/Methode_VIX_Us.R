
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
path = "/Users/leafanirisoa/Documents/projetGit/dynamic_pricing/IG_US_Files/IG_GARCH_RET_VIX_Us"



source(paste(path,"/LoglikReturn.R",sep=""))
source(paste(path,"/Loglik_VIX_Us.r",sep=""))
source(paste(path,"/LogMixte_Us.r",sep=""))
source(paste(path,"/parameters_settting.R",sep=""))
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
start.time <- Sys.time()
RMSE1=RMSE(para_h1,Data.ret,Data.N,N_val)
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken

RMSE1$norm_rmse

##############################
###   Table Test GMM   #######
##############################
Table_RMSE(para_h=para_h1,Data.ret=Data.ret, Data.N=Data.N,N_val)


#####################################################
###    Load both data.contract and data.ret   #######
#####################################################
load("DataPrice20112012.Rdata")

############################################################
####           out sample   RMSE                          ##
############################################################
Data.N=Data.N2

############################################################
####                        RMSE                          ##
############################################################
Data.N=Data.N2
start.time <- Sys.time()
RMSE2=RMSE(para_h1,Data.ret,Data.N,N_val)
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken

RMSE2$norm_rmse
