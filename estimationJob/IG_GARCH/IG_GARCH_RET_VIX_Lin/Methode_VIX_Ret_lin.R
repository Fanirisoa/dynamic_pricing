
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
path = "/Users/leafanirisoa/Documents/projetGit/dynamic_pricing/estimationJob/IG_GARCH/IG_GARCH_RET_VIX_Lin"

source(paste(path,"/parameters_set.R",sep=""))
## source(paste(path,"/LoglikReturn_lin.R",sep=""))
## source(paste(path,"/Loglik_VIX_lin.R",sep=""))
## source(paste(path,"/LogMixte_lin.R",sep=""))

source(paste(path,"/Log_Mixte_new_lin.R",sep=""))
source(paste(path,"/Loglik_new_VIX_lin.R",sep=""))
source(paste(path,"/LoglikReturn.R",sep=""))

source(paste(path,"/RMSE_function_lin.R",sep=""))


para_h<-c( 9.817584e-06,  1.215612e-03 , 3.450e+03 , 4.542147e-05 ,-7.531277e-03, 1.258401e+02,9.974099e-01)

para_h<-c( 9.817584e-06,  1.215612e-03 , 3.400e+03 , 4.542147e-05 ,-7.531277e-03, 1.258401e+02,9.974099e-01)


para_h<-c( 9.817584e-06,  1.215612e-03 , 3.322312e+03 , 4.542147e-05 ,-7.531277e-03, 1.258401e+02,9.974099e-01)


para_h<-c( 9.817584e-06,  1.215612e-03 , 3.300e+03 , 4.542147e-05 ,-7.531277e-03, 1.258401e+02,9.974099e-01)

para_h<-c( 9.817584e-06,  1.215612e-03 , 3.280e+03 , 4.542147e-05 ,-7.531277e-03, 1.258401e+02,9.974099e-01)




para_h<-c(2.234137e-06,  2.318400e-03,  3.317424e+03,  4.894911e-05, -7.955236e-03,  1.258397e+02,  9.978291e-01)

para_h<-c(2.234137e-06,  2.318400e-03,  3.400e+03,  4.894911e-05, -7.955236e-03,  1.258397e+02,  9.978291e-01)
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
# S_e

############################################################
####                        RMSE                          ##
############################################################
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
source("C:/Users/fanir/Desktop/Simulation_juin2018/IG-GARCH/IG_Esscher_returns_VIX/Comparing_VIX_oti_ret_Ess.R")

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



