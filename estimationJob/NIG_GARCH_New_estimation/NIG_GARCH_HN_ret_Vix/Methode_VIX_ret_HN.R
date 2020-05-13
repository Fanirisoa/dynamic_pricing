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
path = "/Users/leafanirisoa/Documents/projetGit/dynamic_pricing/estimationJob/NIG_GARCH_New_estimation/NIG_GARCH_HN_ret_Vix"

source(paste(path,"/Fun_Pricer_VIX_ret_HN.R",sep=""))
source(paste(path,"/MCSim_VIX_ret_HN.R",sep=""))
source(paste(path,"/Loglik_VIX_HN.R",sep=""))
source(paste(path,"/Loglik_VIX_ret_HN.R",sep=""))
source(paste(path,"/Loglik_Return_HN.R",sep=""))
source(paste(path,"/parameters_set.R",sep=""))
source(paste(path,"/RMSE_VIX_ret.R",sep=""))

######################################################################################
###               Volatility   plot under the initial parameters               #######
######################################################################################

ts.vol=h(para_h,Data.ret)
ts.plot(ts.vol, col = "steelblue", main = "IG Garch Model",xlab="2009",ylab="Volatility")
grid()


#####################################################
###              Log values returns           #######
#####################################################

start.time <- Sys.time()
ILK=Heston_likelihood_Mix(para_M,Data.ret, Data.N,Data.returns,N) 
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken
ILK

NIG_likelihood_dens(para_M, Data.returns)
Heston_likelihood_vix(para_M,Data.returns,Data.ret) 


#####################################################
###      Optimization  of the model           #######
#####################################################
start.time <- Sys.time()
Sol=optim(para_M,Heston_likelihood_Mix ,Data.ret=Data.ret, Data.N = Data.N,Data.returns=Data.returns, N=N, method="Nelder-Mead",control = list(maxit = 10))
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken

para_M1<-Sol$par

############################################################
####                        RMSE                          ##
############################################################
start.time <- Sys.time()
RMSE2=RMSEsim(N,para_M1,Data.ret,Data.N) 
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken

RMSE2$rmse
RMSE2$norm_rmse

