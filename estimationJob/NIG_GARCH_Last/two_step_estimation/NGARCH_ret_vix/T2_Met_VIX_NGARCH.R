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
path = "/Users/leafanirisoa/Documents/projetGit/dynamic_pricing/estimationJob/NIG_GARCH_Last/two_step_estimation/NGARCH_ret_vix"

source(paste(path,"/T2_parameters_set.R",sep=""))
source(paste(path,"/T2_Loglik_Ret_NGARCH.R",sep=""))
source(paste(path,"/T2_Loglik_VIX_NGARCH.R",sep=""))
source(paste(path,"/T2_Loglik_Mix_VIX_NGARCH.R",sep=""))
source(paste(path,"/T2_QMLNIG_VIX_NGARCH.R",sep=""))

######################################################################################
###               Volatility   plot under the initial parameters               #######
######################################################################################

ts.vol= shape_vol_P(para_h, Data.returns) 
ts.plot(ts.vol, col = "steelblue", main = "IG Garch Model",xlab="2009",ylab="Volatility")
grid()

#####################################################
###              Log values returns           #######
#####################################################

start.time <- Sys.time()
ILK=NGARCH_likelihood_Mix(para_h,Data.ret, Data.N,Data.returns,N) 
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken
ILK

NGARCH_likelihood_ret(para_h, Data.returns)
NGARCH_likelihood_vix(para_h,Data.returns,Data.ret) 
NGARCH_likelihood_dens_QML(para_distribution,para_h,Data.returns)


#####################################################
###      Optimization  of the model           #######
#####################################################
start.time <- Sys.time()
Sol=optim(para_h,NGARCH_likelihood_Mix ,Data.ret=Data.ret, Data.N = Data.N,Data.returns=Data.returns, N=N, method="Nelder-Mead",control = list(maxit = 5000))
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken

Sol
para_h1<-Sol$par
para_h
para_h1

##########################################################
#                QML estimation  NIG                     # 
##########################################################
start.time <- Sys.time()
QMLSol=optim(para_distribution,NGARCH_likelihood_dens_QML ,para_h =para_h1,Data.returns=Data.returns, method="Nelder-Mead",control = list(maxit = 5000))
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken
QMLSol

para_distribution1= QMLSol$par

parametres_qml=c(para_h1,para_distribution1)

















