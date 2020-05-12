#####################################################
###              Load Data source             #######
#####################################################
source("C:/Users/e0g411k03dt/Desktop/Simulation P3 Mars 2017/Data/Data20092010.R")
source("C:/Users/e0g411k03dt/Desktop/Simulation P3 Mars 2017/Data/Data20112012.R")

#####################################################
###             Clean the repertoir           #######
#####################################################
rm(list=ls())
gc()
library(compiler)
enableJIT(1)
enableJIT(3)
library("fBasics")

#####################################################
###    Load both data.contract and data.ret   #######
#####################################################
load("DataPrice20092010.Rdata")

#####################################################
###                 Data set                  #######
#####################################################
Data.N=Data.N2

Data.N=Data.N2[-c(506,1462,1638,1645),]



#####################################################
###         Source function to use            #######
#####################################################
##source("F:/Simulation class Janvier 2017/HN-GARCH/Gaussian Esscher returns/Loglik Return HN sous P.R")
##source("F:/Simulation class Janvier 2017/HN-GARCH/Gaussian Esscher returns/RMSE HN-Gaussian.r")
source("C:/Users/e0g411k03dt/Desktop/Simulation P3 Mars 2017/GJR-GARCH/GJR Gaussian Esscher returns-VIX/Loglik  VIX GJR.R")
source("C:/Users/e0g411k03dt/Desktop/Simulation P3 Mars 2017/GJR-GARCH/GJR Gaussian Esscher returns-VIX/Loglik Return GJR sous P.R")
source("C:/Users/e0g411k03dt/Desktop/Simulation P3 Mars 2017/GJR-GARCH/GJR Gaussian Esscher returns-VIX/Loglik Mix Ret VIX GJR.R")

 
#####################################################
###         Parameters of the model           #######
#####################################################
##a0=para_h[1]; a1=para_h[2]; a2=para_h[3];  b1= para_h[4] ;  lamda0= para_h[5]  ; ro=para_h[6]

###   Initial parameter  ####
para_h<-c(6.094e-07, 1.240e-01, 2.314e-02, 8.504e-01, 6.025e-02, 9.946611e-01)  ## RMSE2$rmse : 0.1451813  RMSE3$rmse : 0.05582874

para_h<-c(6.094e-11, 1.240e-01, 2.314e-02, 4.011e-01, 3.025e-02, 9.546611e-01)  ## RMSE1$rmse : 0.01700723  RMSE3$rmse : 0.05582874

 

para_h<-c(5.987174e-06,  1.240911e-01,  2.314265e-02,  8.504269e-01,  3.784983e-02, 9.546611e-01)  ## RMSE1$rmse :  0.06265758 RMSE3$rmse :0.07367674
###   Solution ??? ???? ????

#####################################################
###               Volatility                  #######
#####################################################

ts.vol_P= shape_vol_P (para_h, Data.returns) 
ts.plot(ts.vol_P , col = "steelblue", main = "IG Garch Model",xlab="2009",ylab="Volatility")
grid()


ts.vol_Q= shape_vol_Q (para_h, Data.returns) 
ts.plot(ts.vol_Q, col = "steelblue", main = "IG Garch Model",xlab="2009",ylab="Volatility")
grid()

#####################################################
###              LOg values returns           #######
#####################################################
start.time <- Sys.time()
ILK=Heston_likelihood_MixViX(para_h, Data.returns)
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken

ILK

#####################################################
###      Optimization  of the model           #######
#####################################################
start.time <- Sys.time()
Sol=optim(para_h, Heston_likelihood_MixViX, Data.returns=Data.returns, method="Nelder-Mead",control = list(maxit = 5000))
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken

para_h1<-Sol$par
Sol

############################################################
####                        RMSE                          ##
############################################################
#N=1000 
N=2^10

source("C:/Users/e0g411k03dt/Desktop/Simulation P3 Mars 2017/GJR-GARCH/GJR Gaussian Esscher returns-VIX/Function Pricer VIX GJR.r")
source("C:/Users/e0g411k03dt/Desktop/Simulation P3 Mars 2017/GJR-GARCH/GJR Gaussian Esscher returns-VIX/Simulation MC.r")
source("C:/Users/e0g411k03dt/Desktop/Simulation P3 Mars 2017/GJR-GARCH/GJR Gaussian Esscher returns-VIX/RMSE VIX GJR.r")

para_h <-para_h1 
start.time <- Sys.time()
RMSE1=RMSEsim(para_h1,Data.N)
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken

RMSE1$rmse



#####################################################
###    Load both data.contract and data.ret   #######
#####################################################
load("DataPrice20112012.Rdata")

Data.N=Data.N2
############################################################
####        RMSE  out of sample 2011-2012                 ##
############################################################
N=2^10

start.time <- Sys.time()
RMSE2=RMSEsim(para_h1,Data.N2)
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken

RMSE2$rmse

############################################################
####                       Compare VIX                    ##
############################################################
source("C:/Users/e0g411k03dt/Desktop/Simulation P3 Mars 2017/GJR-GARCH/GJR Gaussian Esscher returns/Comparing VIX_GJR_Garch.R")

start.time <- Sys.time()
C_VIX= Compa_vix(para_h1,Data.returns)
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken

C_VIX




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
