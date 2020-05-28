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
path = "/Users/leafanirisoa/Documents/projetGit/dynamic_pricing/estimationJob/m_t_new/GJR_GARCH/GJR_Gaussian_Esscher_returns"

source(paste(path,"/parameters_setting_mt.R",sep=""))
source(paste(path,"/Loglik_Return_GJR_mt.R",sep=""))

######################################################################################
###               Volatility   plot under the initial parameters               #######
######################################################################################
ts.vol_P= shape_vol_P(para_h, Data.returns) 
ts.plot(ts.vol_P , col = "steelblue", main = "GJR Garch Model",xlab="Time",ylab="Volatility")
grid()

#####################################################
###              Log values returns           #######
#####################################################
start.time <- Sys.time()
ILK=GJR_likelihood_ret_mt(para_h, Data.returns)
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken

ILK

#####################################################
###      Optimization  of the model           #######
#####################################################
start.time <- Sys.time()
Sol=optim(para_h, GJR_likelihood_ret_mt , Data.returns=Data.returns, method="Nelder-Mead",control = list(maxit = 5000))
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken
Sol
para_h1<-Sol$par
para_h
para_h1
