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
path = "/Users/leafanirisoa/Documents/projetGit/dynamic_pricing/estimationJob/Estim_papier2/NIG_GARCH_HN_ret_vix"


source(paste(path,"/T2_parameters_set.R",sep=""))
source(paste(path,"/T2_Loglik_Ret_HN.R",sep=""))
source(paste(path,"/T2_Loglik_VIX_HN.R",sep=""))
source(paste(path,"/T2_Loglik_VIX_ret_HN.R",sep=""))
source(paste(path,"/T2_QMLNIG_VIX_HN.R",sep=""))
source(paste(path,"/T2_simulation.R",sep=""))


######################################################################################
###               Volatility   plot under the initial parameters               #######
######################################################################################

ts.vol= shape_vol_P(para_h, Data.returns) 
ts.plot(ts.vol, col = "steelblue", main = "IG Garch Model",xlab="2009",ylab="Volatility")
grid()

ts.vol= shape_vol_Q(para_h, Data.returns) 
ts.plot(ts.vol, col = "steelblue", main = "IG Garch Model",xlab="2009",ylab="Volatility")
grid()


#####################################################
###              Log values returns           #######
#####################################################

start.time <- Sys.time()
ILK=Heston_likelihood_Mix(para_h,Data.ret, Data.N,Data.returns,N) 
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken
ILK

Heston_likelihood_ret(para_h, Data.returns)
Heston_likelihood_vix(para_h,Data.returns,Data.ret) 


#####################################################
###      Optimization  of the model           #######
#####################################################
start.time <- Sys.time()
Sol=optim(para_h,Heston_likelihood_Mix ,Data.ret=Data.ret, Data.N = Data.N,Data.returns=Data.returns, N=N, method="Nelder-Mead",control = list(maxit = 5000))
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
QMLSol=optim(para_distribution,NIG_likelihood_dens_QML ,para_h =para_h1,Data.returns=Data.returns, method="Nelder-Mead",control = list(maxit = 5000))
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken
QMLSol

para_distribution1= QMLSol$par

parametres_qml=c(para_h1,para_distribution1)



##############################
#####  Simulation de H_t  ####
##############################

h_vol= shape_vol_sim(para_h, para_distribution,2718)
ts.plot(h_vol, col = "steelblue", main = "IG Garch Model",xlab="2009",ylab="Volatility")
grid()

######################################################
###      Modified Optimization  of the model     #####
######################################################
start.time <- Sys.time()
Sol_1=optim(para_h1,modified_Heston_likelihood_ret ,Data.returns = Data.returns,h = h_vol, method="Nelder-Mead",control = list(maxit = 5000))
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken

para_h1
Sol_1$par


#################################
#####  Simulation de VIX_t  ####
#################################

h_vol= shape_vol_sim(para_h, para_distribution,2718)
ts.plot(h_vol, col = "steelblue", main = "IG Garch Model",xlab="2009",ylab="Volatility")
grid()


