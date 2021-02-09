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
path = "/Users/leafanirisoa/Documents/GitHub/dynamic_pricing/estimationJob/Estim_papier2/NIG_GARCH_HN_ret_option"

source(paste(path,"/T2_parameters_set.R",sep=""))
source(paste(path,"/T2_Loglik_Ret_HN.R",sep=""))
source(paste(path,"/T2_Loglik_mix_ret_HN.R",sep=""))
source(paste(path,"/T2_Loglik_opt_HN.R",sep=""))
source(paste(path,"/T2_Loglik_vix_HN.R",sep=""))
source(paste(path,"/T2_QMLNIG_opt_HN.R",sep=""))
source(paste(path,"/T2_simulation.R",sep=""))
source(paste(path,"/T2_MCSim_opt_ret_HN.R",sep=""))
source(paste(path,"/T2_Fun_Pricer_opt_ret_HN.R",sep=""))



######################################################################################
###               Volatility   plot under the initial parameters               #######
######################################################################################

ts.vol= shape_vol_P(para_h, Data.returns) 
ts.plot(ts.vol, col = "steelblue", main = "IG Garch Model",xlab="Simulation",ylab="Volatility")
grid()

ts.vol= shape_vol_Q(para_h, Data.returns) 
ts.plot(ts.vol, col = "steelblue", main = "IG Garch Model",xlab="Simulation",ylab="Volatility")
grid()


#####################################################
###              Log values returns           #######
#####################################################

start.time <- Sys.time()
ILK=Heston_likelihood_Mix_vix(para_h,Data.ret, Data.N,Data.returns,N) 
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
Sol=optim(para_h,Heston_likelihood_Mix_vix ,Data.ret=Data.ret, Data.N = Data.N,Data.returns=Data.returns, N=N, method="Nelder-Mead",control = list(maxit = 5000))
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

para_distribution
para_distribution1

#######################
#####  Parameters  ####
#######################

para_h
para_h1

para_distribution
para_distribution1

para_h1 = para_h
para_distribution1 = para_distribution
##############################
#####  Simulation de z_t  ####
##############################

z_sim_val= z_sim(para_h1, para_distribution1,2718) 
ts.plot(z_sim_val, col = "steelblue", main = "Simulation z_t",xlab="Index values",ylab="z_t values")
grid()


##############################
#####  Simulation de H_t  ####
##############################

Vol_sim= shape_vol_sim(para_h1, para_distribution1,z_sim_val,2718)
ts.plot(Vol_sim, col = "steelblue", main = "Simulation vol of HN-NIG-GARCH",xlab="Index values",ylab="h_t")
grid()


#################################
#####  Simulation de Y_t    ####
#################################

Ret_sim=  ret_simulation(para_h1, para_distribution1,z_sim_val, Vol_sim)
ts.plot(Ret_sim, col = "steelblue", main = "Simulation Y_t Model",xlab="Index values",ylab="Y_t")
grid()


#################################
#####   compute  de S_T      ####
#################################
S_0 = mean(Data.N$S)
S_T = val_S_T(Ret_sim,S_0)
S_T 
Data.Nbis=Data.N2[-c(506,1462,1638,1645),]
S_val=Data.Nbis$S
M=list()
for(i in 1:length(Data.Nbis$S))  
{
  M[i]=val_S_T(Ret_sim,S_val[i])
}

plot(Data.Nbis$S, M, main = "Values of S_T as function of S_0",xlab="Values S_0",ylab="Values S_T",col = "red")

#################################################
####   Generate   dataset of option Data.N   ####
#################################################
Data.PP=Data.N2[-c(506,1462,1638,1645),]
Data_original = Data.PP[200:240,]
Data_original



Data.N <- ger_Data.N(l,r_0, S_T,list.SK,list.T)
Data.N

N=N_sim
######################################################################
######         Compute option prices using MC simulation            ##
######################################################################
Data.N=Data.N[1:40,]

start.time <- Sys.time()
P<-Pricer_P(N_sim,para_h1,para_distribution1,Data.N)
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken
P$P

P_price= c(26.03,56.78,78.16,104.91,130.91,163.61,200.91,230.87,31.86,44.38,65.70,85.62,120.91,150.81,178.41,205.95,9.15,17.32,30.90,51.27,81.58,122.18,169.02,211.07,12.11,38.00,50.48,69.12,98.17,125.67,158.27,194.77,14.18,26.71,38.84,57.77,80.27,111.77,141.27,179.88)
  
option_dataset <-  data.frame(K = Data.N$K, T = Data.N$T, S = Data.N$S, C=P_price, r = Data.N$r)
option_dataset

############################################################### 
######       Estimation tow step : return - option           ##
###############################################################
#####################################################
###      Optimization  of the model           #######
#####################################################
N = 2
N_sim <- 2

para_M1 = c(para_distribution1,para_h1)
Heston_likelihood_Mix_sim_opt(para_h1,para_distribution1,Ret_sim, option_dataset) 

start.time <- Sys.time()
Sol_sim=optim(para_h1,Heston_likelihood_Mix_sim_opt ,para_distribution = para_distribution1, Ret_sim=Ret_sim, Data.N = option_dataset, method="Nelder-Mead",control = list(maxit = 5))
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
QMLSol=optim(para_distribution1,NIG_likelihood_dens_QML ,para_h =para_h2,Data.returns=Data.returns, method="Nelder-Mead",control = list(maxit = 5000))
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken
QMLSol

para_distribution1= QMLSol$par

parametres_qml=c(para_h1,para_distribution1)

para_distribution
para_distribution1 
##########################################################
#                QML estimation  NIG                     # 
##########################################################
start.time <- Sys.time()
QMLSol_sim=optim(para_distribution1,NIG_likelihood_dens_QML_sim ,para_h =para_h1,Ret_sim=Ret_sim, method="Nelder-Mead",control = list(maxit = 5000))
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken
QMLSol_sim

para_distribution2 <- QMLSol_sim$par

para_distribution
para_distribution1
para_distribution2

#####################################################
###      Optimization  of the model           #######
#####################################################
start.time <- Sys.time()
Sol=optim(para_h1,Heston_likelihood_Mix_ret_sim ,Ret_sim = Ret_sim, Data.returns = Data.returns,method="Nelder-Mead",control = list(maxit = 5000))
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken

Sol
para_h1<-Sol$par
para_h
para_h1
