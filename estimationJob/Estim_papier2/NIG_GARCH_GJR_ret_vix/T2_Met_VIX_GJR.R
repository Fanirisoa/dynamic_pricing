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
setwd("/Users/leafanirisoa/Documents/GitHub/dynamic_pricing/data_used")  
path = "/Users/leafanirisoa/Documents/GitHub/dynamic_pricing/estimationJob/Estim_papier2/NIG_GARCH_GJR_ret_vix"


source(paste(path,"/T2_parameters_set.R",sep=""))
source(paste(path,"/T2_Loglik_Ret_GJR.R",sep=""))
source(paste(path,"/T2_Loglik_VIX_GJR.R",sep=""))
source(paste(path,"/T2_Loglik_Mix_VIX_GJR.R",sep=""))
source(paste(path,"/T2_QMLNIG_VIX_GJR.R",sep=""))
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
ILK=GJR_likelihood_Mix(para_h,Data.ret, Data.N,Data.returns,N) 
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken
ILK

GJR_likelihood_ret(para_h, Data.returns)
GJR_likelihood_vix(para_h,Data.returns,Data.ret) 


#####################################################
###      Optimization  of the model           #######
#####################################################
start.time <- Sys.time()
Sol=optim(para_h,GJR_likelihood_Mix ,Data.ret=Data.ret, Data.N = Data.N,Data.returns=Data.returns, N=N, method="Nelder-Mead",control = list(maxit = 5000))
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
#####  Simulation de z_t  ####
##############################

z_sim_val= z_sim(para_h1, para_distribution1,2718) 
ts.plot(z_sim_val, col = "steelblue", main = "Simulation z_t",xlab="index",ylab="z_t values")
grid()


##############################
#####  Simulation de H_t  ####
##############################

Vol_sim= shape_vol_sim(para_h1, para_distribution1,z_sim_val,2718)
ts.plot(Vol_sim, col = "steelblue", main = "Simulation h_t",xlab="index",ylab="h_t values")
grid()


#################################
#####  Simulation de ret_t  ####
#################################

Ret_sim=  ret_simulation(para_h1, para_distribution1,z_sim_val, Vol_sim)
ts.plot(Ret_sim, col = "steelblue", main = "Simulation Ret Model",xlab="index",ylab="Y_t values")
grid()


#################################
#####  Simulation de VIX_t   ####
#################################

Vix_sim= shape_VIX_sim(para_h1, para_distribution1,2718)
ts.plot(Vix_sim, col = "steelblue", main = "Simulation VIX Model",xlab="2009",ylab="Volatility")
grid()


#################################
#####  Simulation de ret_t  ####
#################################

Ret_sim=  ret_simulation(para_h1, para_distribution1,z_sim_val, Vol_sim)
ts.plot(Ret_sim, col = "steelblue", main = "Simulation Ret Model",xlab="2009",ylab="Volatility")
grid()


#####################################################
###      Optimization  of the model           #######
#####################################################
GJR_likelihood_ret_sim(para_h1, Ret_sim)
GJR_likelihood_vix_sim(para_h1, Ret_sim,Vix_sim)


start.time <- Sys.time()
Sol_sim=optim(para_h,GJR_likelihood_Mix_sim ,Ret_sim=Ret_sim, Vix_sim = Vix_sim, method="Nelder-Mead",control = list(maxit = 5000))
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken

Sol_sim
para_h2<-Sol_sim$par

para_h
para_h1
para_h2


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

para_distribution
para_distribution_int = para_distribution
##########################################################
#                QML estimation  NIG                     # 
##########################################################
start.time <- Sys.time()
QMLSol_sim=optim(para_distribution_int,NIG_likelihood_dens_QML_sim ,para_h =para_h2,Ret_sim=Ret_sim, method="Nelder-Mead",control = list(maxit = 5000))
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken
QMLSol_sim

para_distribution2 <- QMLSol_sim$par

para_distribution
para_distribution1
para_distribution2

###########################################
#####     Simulation de VIX_t result   ####
###########################################

Vix_sim_2= shape_VIX_sim(para_h2, para_distribution2,2718)
ts.plot(Vix_sim_2, col = "steelblue", main = "Simulation VIX_t Model result",xlab="Index values",ylab="VIX")
grid()

##########################################################
#####       Comparing VIX simulation and estimated    ####
##########################################################
VIX_Model <- Vix_sim_2
VIX_Market <- Vix_sim 

Compa_vix(VIX_Model,VIX_Market)


