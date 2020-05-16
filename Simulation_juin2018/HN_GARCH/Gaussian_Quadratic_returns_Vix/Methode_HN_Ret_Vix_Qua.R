#####################################################
###              Load Data source             #######
#####################################################
setwd("C:/Users/fanir/Desktop/Simulation_juin2018/Data")  

#####################################################
###             Clean the repertoir           #######
#####################################################
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

#####################################################
###    Load both data.contract and data.ret   #######
#####################################################
load("DataPrice20092010.Rdata")

#####################################################
###                 Data set                  #######
#####################################################
#Data.N=Data.N2

Data.N=Data.N2[-c(506,1462,1638,1645),]

#####################################################
###         Source function to use            #######
#####################################################
source("C:/Users/fanir/Desktop/Simulation_juin2018/HN_GARCH/Gaussian_Quadratic_returns_Vix/Loglik_Return_HN_P.r")
source("C:/Users/fanir/Desktop/Simulation_juin2018/HN_GARCH/Gaussian_Quadratic_returns_Vix/Loglik_Mix_Ret_VIX HN.r")
source("C:/Users/fanir/Desktop/Simulation_juin2018/HN_GARCH/Gaussian_Quadratic_returns_Vix/Loglik_VIX_HN_Qua.r")



#####################################################
###         Parameters of the model           #######
#####################################################
###   a0=para_h[1]; a1=para_h[2]; gama=para_h[3];  b1= para_h[4] ;  lamda0= para_h[5]  ; Pi=para_h[6] ; ro=para_h[7]

###   Initial parameter  ####

para_h<-c(5.282379e-08, 2.252557e-04 ,8.143868e+00 ,9.154310e-01 ,0.626485e-00,1.207890e+00, 9.784247e-01) ##  RMSE2$rmse : 0.03390988 RMSE3$rmse : 0.01818576

para_h<-c(5.282379e-08, 2.252557e-04 ,8.143868e+00 ,9.154310e-01 ,0.626485e-00,1.37890e+00, 9.784247e-01) ##  RMSE2$rmse : 0.05826292 RMSE3$rmse : 0.01818576

para_h<-c(5.282379e-08, 2.252557e-04 ,8.143868e+00 ,9.154310e-01 ,0.826485e-00,1.17890e+00, 9.784247e-01) ##  RMSE2$rmse : 0.05397943 RMSE3$rmse : 0.07031177

para_h<-c(6.068393e-06, 2.584624e-04, 8.941684e+00 ,4.580279e-01 ,1.364058e+00 ,1.12000e+00, 9.312173e-01 )  ##  RMSE2$rmse :  0.04461888
###   Solution 
para_h<-c(6.068393e-06, 2.584624e-04, 8.941684e+00 ,4.580279e-01 ,1.364058e+00 ,1.001000e+00, 1.312173e-01 ) 



###
para_h<-c(5.282379e-08, 2.252557e-04 ,8.143868e+00 ,9.154310e-01 ,0.826485e-00,1.27890e+00, 9.784247e-01) ##  RMSE2$rmse : 0.05397943 RMSE3$rmse : 0.07031177

para_h<-c(5.282379e-08, 2.252557e-04 ,8.143868e+00 ,9.154310e-01 ,0.626485e-00,1.37890e+00, 9.784247e-01) ##  RMSE2$rmse : 0.05826292 RMSE3$rmse : 0.01818576

para_h=c(3.278319e-05, 1.969895e-04, 6.329591e+01, 1.419832e-01, 1.723114e-03, 1.672333e+00, 9.784247e-01)

#####################################################
###               Volatility                  #######
#####################################################

ts.vol= shape_vol_P(para_h, Data.returns) 
ts.plot(ts.vol, col = "steelblue", main = "IG Garch Model",xlab="2009",ylab="Volatility")
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
Heston_likelihood_ret(para_h,Data.returns)
Heston_likelihood_vix(para_h,Data.returns)


#####################################################
###      Optimization  of the model           #######
#####################################################
start.time <- Sys.time()
Sol=optim(para_h, Heston_likelihood_MixViX , Data.returns=Data.returns, method="Nelder-Mead",control = list(maxit = 5000))
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken

para_h1<-Sol$par
Sol

Hess=fdHess(para_h,Heston_likelihood_MixViX , Data.returns=Data.returns)
S_e <- sqrt(diag(solve(nearPD(Hess$Hessian)$mat)))
S_e

############################################################
####            In sample   RMSE                          ##
############################################################
source("C:/Users/fanir/Desktop/Simulation_juin2018/HN_GARCH/Gaussian_Quadratic_returns_Vix/RMSE_HN_Gaussian_Qua.r")

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
source("C:/Users/fanir/Desktop/Simulation_juin2018/HN_GARCH/Gaussian_Quadratic_returns_Vix/Comparing_VIX_HN_GARCH_Qua.r")

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
RMSE2$norm_rmse