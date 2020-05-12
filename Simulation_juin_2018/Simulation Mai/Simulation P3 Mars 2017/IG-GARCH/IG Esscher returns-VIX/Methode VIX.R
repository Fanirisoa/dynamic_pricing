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
source("C:/Users/e0g411k03dt/Desktop/Simulation P3 Mars 2017/IG-GARCH/IG Esscher returns-VIX/LoglikReturn.R")
source("C:/Users/e0g411k03dt/Desktop/Simulation P3 Mars 2017/IG-GARCH/IG Esscher returns-VIX/Loglik VIX lin.r")
source("C:/Users/e0g411k03dt/Desktop/Simulation P3 Mars 2017/IG-GARCH/IG Esscher returns-VIX/LogMixte.r")


#####################################################
###         Parameters of the model           #######
#####################################################
###   Initial parameter  ####
##    w=para_h[1]; b=para_h[2]; a=para_h[3];  c= para_h[4]; neta=para_h[5] ; nu=para_h[6]; ro=para_h[7]

para_h<-c(4.238940e-06 , 2.058376e-03  ,3.317425e+03 , 5.058743e-05, -8.281782e-03 , 1.2584e+02,  9.84599e-01 )  ## RMSE2$rmse :0.04354643  RMSE3$rmse : 0.05756848

##   solution  ####
para_h1<-c(1.016620e-05,  2.041894e-03,  3.317506e+03,  4.501851e-05, -7.465712e-03,  1.258399e+02,  9.945561e-01) 

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


############################################################
####                        RMSE                          ##
############################################################
source("C:/Users/e0g411k03dt/Desktop/Simulation P3 Mars 2017/IG-GARCH/IG Esscher returns-VIX/RMSE function.R")

start.time <- Sys.time()
RMSE1=RMSE(para_h1,Data.ret,Data.N)
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken

RMSE1$rmse

############################################################
####                       Compare VIX                    ##
############################################################
source("C:/Users/e0g411k03dt/Desktop/Simulation P3 Mars 2017/IG-GARCH/IG Esscher returns-VIX/Comparing_VIX_oti_ret_Ess.R")

start.time <- Sys.time()
C_VIX= Compa_vix(para_h1,Data.returns)
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken

C_VIX


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



