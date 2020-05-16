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
source("C:/Users/fanir/Desktop/Simulation_juin2018/IG-GARCH/IG_Esscher_returns_option/LoglikMix.R")
source("C:/Users/fanir/Desktop/Simulation_juin2018/IG-GARCH/IG_Esscher_returns_option/LoglikOprion.r")
source("C:/Users/fanir/Desktop/Simulation_juin2018/IG-GARCH/IG_Esscher_returns_option/LoglikReturn.r")

#####################################################
###         Parameters of the model           #######
#####################################################
##w=para_h[1]; b=para_h[2]; a=para_h[3];  c= para_h[4]; neta=para_h[5] ; nu=para_h[6]  

###   Initial parameter  ####

para_h1<-c(4.238940e-06 , 2.058376e-03  ,3.317425e+03 , 5.058743e-05, -8.281782e-03 , 1.2584e+02  )  ## RMSE2$rmse :   RMSE3$rmse :  

para_h<-c(4.238940e-06 , 2.058376e-03  ,3.317425e+03 , 5.058743e-05, -8.281782e-03 , 1.2584e+02 )   ## RMSE2$rmse :0.05435837 RMSE3$rmse : 0.06742708
###   Solution 
para_h<-c( 9.832251e-06 , 1.215820e-03 , 3.317425e+03 , 4.543874e-05 , -7.531312e-03 , 1.25631e+02 ) 



para_h<-c(3.049840e-06 , 1.498376e-03  ,3.301217e+03 , 5.04573e-05, -8.278782e-03 , 1.25625e+02 )   ## RMSE2$rmse :0.05435837 RMSE3$rmse : 40.37993

para_h<-c(3.049840e-06 , 1.058376e-03  ,3.301217e+03 , 5.04573e-05, -8.278782e-03 , 1.25625e+02 )   ## RMSE2$rmse :0.05435837 RMSE3$rmse : 39.58521

para_h<-c(3.059840e-06 , 0.078376e-03  ,3.3013217e+03 , 5.04573e-05, -8.278782e-03 , 1.25625e+02 )   ## RMSE2$rmse :0.05435837 RMSE3$rmse : 38.67009

para_h<-c(3.249840e-06 , 0.078376e-03  ,3.3013217e+03 , 5.04573e-05, -8.278782e-03 , 1.25625e+02 )   ## RMSE2$rmse :0.05435837 RMSE3$rmse : 38.54657

para_h<-c(3.249840e-06 , 0.078376e-03  ,3.3013217e+03 , 5.04031e-05, -8.278782e-03 , 1.25625e+02 )   ## RMSE2$rmse :0.05435837 RMSE3$rmse : 38.35995

para_h<-c(3.249840e-06 , 0.077376e-03  ,3.3013217e+03 , 5.04031e-05, -8.278782e-03 , 1.25631e+02 )   ## RMSE2$rmse :0.05435837 RMSE3$rmse : 38.3262

#####################################################
###               Volatility                  #######
#####################################################

ts.vol=h(para_h,Data.ret)
ts.plot(ts.vol, col = "steelblue", main = "IG Garch Model",xlab="2009",ylab="Volatility")
grid()

#####################################################
###              LOg values                   #######
#####################################################
start.time <- Sys.time()
ILK=IGGARCH_likelihood_Mix(para_h,Data.ret, Data.N,Data.returns)
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken

ILK


#####################################################
###      Optimization  of the model           #######
#####################################################
start.time <- Sys.time()
Sol=optim(para_h,IGGARCH_likelihood_Mix ,Data.ret=Data.ret, Data.N = Data.N,Data.returns=Data.returns, method="Nelder-Mead",control = list(maxit = 5000))
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken

para_h1<-Sol$par

# Standard error
Hess=fdHess(para_h,IGGARCH_likelihood_Mix, Data.returns=Data.returns,Data.ret=Data.ret, Data.N = Data.N)
S_e <- sqrt(diag(solve(nearPD(Hess$Hessian)$mat)))
S_e

para_h1=para_h
############################################################
####                        RMSE                          ##
############################################################
source("C:/Users/fanir/Desktop/Simulation_juin2018/IG-GARCH/IG_Esscher_returns_option/RMSE_function.r")
start.time <- Sys.time()
RMSE1=RMSE(para_h1,Data.ret,Data.N)
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken

RMSE1$rmse
RMSE1$norm_rmse


#####################################################
###    Load both data.contract and data.ret   #######
#####################################################
load("DataPrice20112012.Rdata")

############################################################
####              RMSE    out-sample                      ##
############################################################
Data.N=Data.N2
start.time <- Sys.time()
RMSE2=RMSE(para_h1,Data.ret,Data.N)
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken

RMSE2$rmse
RMSE2$norm_rmse


############################################################
####                       Compare VIX                    ##
############################################################
source("C:/Users/fanir/Desktop/Simulation_juin2018/IG-GARCH/IG_Esscher_returns_option/Comparing_VIX_oti_ret_Ess.r")

start.time <- Sys.time()
C_VIX= Compa_vix(para_h1,Data.returns)
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken

C_VIX
