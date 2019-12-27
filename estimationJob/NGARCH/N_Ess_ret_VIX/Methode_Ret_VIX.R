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
path = "/Users/leafanirisoa/Documents/projetGit/dynamic_pricing/estimationJob/NGARCH/N_Ess_ret_VIX"

source(paste(path,"/parameters_settting.R",sep=""))
source(paste(path,"/Loglik_VIX_NGARCH.R",sep=""))
source(paste(path,"/Log_Mixte_VIX_Ret.R",sep=""))
source(paste(path,"/NGARCH_returns_loglike.R",sep=""))

source(paste(path,"/Simulation_MC_Pricer_return_NGARCH.R",sep=""))
source(paste(path,"/Function_Pricer_return_NGARCH.R",sep=""))
source(paste(path,"/RMSES_return_NGARCH.R",sep=""))
source(paste(path,"/Table_RMSE.R",sep=""))



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
para_h<-Sol$par

# Standard error
Hess=fdHess(para_h1,NGARCH_likelihood_MixViX, Data.returns=Data.returns)
S_e <- sqrt(diag(solve(nearPD(Hess$Hessian)$mat)))
S_e

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

##############################
###   Table Test GMM   #######
##############################
Table_RMSE(para_h=para_h1,Data.ret=Data.ret, Data.N=Data.N,N)

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
source("C:/Users/fanir/Desktop/Simulation_juin2018/NGARCH/N Esscher returns-VIX/Comparing_VIX_oti_ret_Ess.R")
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

##############################
###   Table Test GMM   #######
##############################
Table_RMSE(para_h=para_h1,Data.ret=Data.ret, Data.N=Data.N,N)
