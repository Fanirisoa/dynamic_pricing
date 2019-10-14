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
path = "/Users/leafanirisoa/Documents/projetGit/dynamic_pricing/estimationJob/GJR_GARCH/GJR_Gaussian_Esscher_returns"

source(paste(path,"/parameters_settting.R",sep=""))
source(paste(path,"/Loglik_Return_GJR.R",sep=""))
source(paste(path,"/Function_Pricer_VIX_GJR.r",sep=""))
source(paste(path,"/Simulation_MC.r",sep=""))
source(paste(path,"/RMSE_VIX_GJR.r",sep=""))


######################################################################################
###               Volatility   plot under the initial parameters               #######
######################################################################################
ts.vol_P= shape_vol_P (para_h, Data.returns) 
ts.plot(ts.vol_P , col = "steelblue", main = "GJR Garch Model",xlab="Time",ylab="Volatility")
grid()

ts.vol_Q= shape_vol_Q (para_h, Data.returns) 
ts.plot(ts.vol_Q, col = "steelblue", main = "GJR Garch Model",xlab="Time",ylab="Volatility")
grid()

ts.plot(cbind(ts.vol_P, ts.vol_Q),  gpars = list(col = c("black", "red")), main = "GJR Garch Model",xlab="Time",ylab="Volatility")
grid()

#####################################################
###              Log values returns           #######
#####################################################
start.time <- Sys.time()
ILK=GJR_likelihood_ret(para_h, Data.returns)
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken

ILK

#####################################################
###      Optimization  of the model           #######
#####################################################
start.time <- Sys.time()
Sol=optim(para_h, GJR_likelihood_ret , Data.returns=Data.returns, method="Nelder-Mead",control = list(maxit = 5000))
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken
Sol
para_h1<-Sol$par


####################################################
######         RMSE compare by values of N       ##
####################################################
RMSE_by_N <- function(set_N,para_h, Data.returns) {
  Ni=length(set_N)      ####  length of the vector containing N,

  RMSE <-rep(NA, Ni)
  RMSE_simple <-rep(NA, Ni)
  RMSE_norm <- rep(NA, Ni)
  time_Computation <- rep(NA, Ni)
  N <- rep(NA, Ni)
  
  
  print(paste0("level: ", 1))
  
  for (i in 1:length(set_N)){
    N[i] = set_N[i]
    
    print(paste0("level: ", 2))
    start.time <- Sys.time()
    RMSE=RMSEsim(para_h1,Data.N,N[i])
    end.time <- Sys.time()
    
    print(paste0("level: ", 9))
  
    time.taken <- end.time - start.time
    time_Computation[i]=time.taken  
    RMSE_simple[i] = RMSE$rmse
    RMSE_norm[i] = RMSE$norm_rmse
    
  }

  return(list(RMSE_simple=RMSE_simple,RMSE_norm = RMSE_norm,time_Computation = time_Computation, set_N = set_N)) 

}

N_1<-c(1500, 2500, 3500, 4500, 5500)
start.time <- Sys.time()
RMSE_N_1=RMSE_by_N(N_1,para_h1,Data.N)
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken

RMSE_N_1$RMSE_simple
RMSE_N_1$RMSE_norm
RMSE_N_1$time_Computation



N_2<-c(6500, 7500, 8500, 9500)  

start.time <- Sys.time()
RMSE_N_2=RMSE_by_N(N_2,para_h1,Data.N)
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken

RMSE_N_2$RMSE_simple
RMSE_N_2$RMSE_norm
RMSE_N_2$time_Computation




N_3<-c(3500)  

start.time <- Sys.time()
RMSE_N_3=RMSE_by_N(N_3,para_h1,Data.N)
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken

RMSE_N_3$RMSE_simple
RMSE_N_3$RMSE_norm
RMSE_N_3$time_Computation











############################################################
####                        RMSE                          ##
############################################################


para_h <-para_h1 
start.time <- Sys.time()
RMSE1=RMSEsim(para_h1,Data.N)
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken

RMSE1$rmse
RMSE1$norm_rmse




#####################################################
###    Load both data.contract and data.ret   #######
#####################################################
load("DataPrice20112012.Rdata")

Data.N=Data.N2
############################################################
####        RMSE  out of sample 2011-2012                 ##
############################################################
start.time <- Sys.time()
RMSE2=RMSEsim(para_h1,Data.N2)
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken

RMSE2$rmse
RMSE2$norm_rmse


############################################################
####                       Compare VIX                    ##
############################################################
source("C:/Users/fanir/Desktop/Simulation_juin2018/GJR_GARCH/GJR_Gaussian_Esscher_returns/Comparing VIX_GJR_Garch.R")


start.time <- Sys.time()
C_VIX= Compa_vix(para_h1,Data.returns)
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken



############################################################
####                Simulation Price                      ##
############################################################    
start.time <- Sys.time()
P_T_SimMC1=Pricer(N,para_h1,Data.N)
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken

P_T_SimMC1$P
P_T_SimMC1$Yt
P_T_SimMC1$St
P_T_SimMC1$St_Mar


start.time <- Sys.time()
P_T_SimMC1=Pricer(N,para_h1,Data.N2)
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken

P_T_SimMC1$P


############################################################
####                        RMSE                          ##
############################################################

start.time <- Sys.time()
RMSE2=RMSEsim (para_h1,Data.N)
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken

RMSE2$rmse
RMSE2$P



##########################################################
####                  DATA change                         ##
############################################################

start.time <- Sys.time()
RMSE1=RMSE(para_h,Data.ret,Data.N)
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken

RMSE1$rmse
A=RMSE1$error 

max(A)

D1=c()    
for(i in 1:length(A)) {
  D1[i] = A[i]
}


Data.N<-data.frame(C=Data.N$C,K=Data.N$K,S=Data.N$S,T=Data.N$T,r=Data.N$r,d=Data.N$d,Pe=Data.N$Pe,Per=Data.N$Per,Mod=Data.N$Mod,CsK=Data.N$CsK,D1)


Data.modif=Data.N[!Data.N$D1 <= 0.02,]
Data.modif
Data.N=Data.N[-c(506,1462,1638,1645),]
