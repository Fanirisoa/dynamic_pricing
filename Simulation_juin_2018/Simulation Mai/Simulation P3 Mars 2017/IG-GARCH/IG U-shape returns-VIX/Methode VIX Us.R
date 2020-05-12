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
###         Source function to use            #######
#####################################################
source("C:/Users/e0g411k03dt/Desktop/Simulation P3 Mars 2017/IG-GARCH/IG U-shape returns-VIX/LoglikReturn.R")
source("C:/Users/e0g411k03dt/Desktop/Simulation P3 Mars 2017/IG-GARCH/IG U-shape returns-VIX/Loglik VIX Us.r")
source("C:/Users/e0g411k03dt/Desktop/Simulation P3 Mars 2017/IG-GARCH/IG U-shape returns-VIX/LogMixte Us.r")

 


#####################################################
###                 Data set                  #######
#####################################################
Data.N=Data.N2

Data.N=Data.N2[-c(506,1462,1638,1645),]

#####################################################
###         Parameters of the model           #######
#####################################################
###   Initial parameter  ####
#   w=para_h[1]; b=para_h[2]; a=para_h[3];  c= para_h[4]; neta=para_h[5] ; nu=para_h[6] ; PI=para_h[7] ; ro=para_h[8]

para_h<-c(3.238940e-06 , 2.058376e-03  ,3.317425e+03 , 5.058743e-05, -8.281782e-03 , 1.2584e+02, 1.1003707e+00 ,  9.83599e-01 )  ## RMSE2$rmse :0.04287557  RMSE3$rmse : 0.05661406

para_h<-c(3.238940e-06 , 2.058376e-03  ,3.317425e+03 , 5.058743e-05, -8.281782e-03 , 1.2584e+02, 1.1003707e+00 ,  9.83599e-01 )   ## RMSE2$rmse :0.04620022  RMSE3$rmse : 0.06100658 

##   solution  ####
para_h<-c(9.876201e-06  8.626756e-03  3.317423e+03  4.491365e-05 -7.516695e-03  1.258370e+02  1.097198e+00  9.962589e-01 )   

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
source("C:/Users/e0g411k03dt/Desktop/Simulation P3 Mars 2017/IG-GARCH/IG U-shape returns-VIX/RMSE function Us.R")

start.time <- Sys.time()
RMSE1=RMSE(para_h1,Data.ret,Data.N)
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken

RMSE1$rmse

############################################################
####                       Compare VIX                    ##
############################################################
source("C:/Users/e0g411k03dt/Desktop/Simulation P3 Mars 2017/IG-GARCH/IG U-shape returns-VIX/Comparing VIX Us.R")

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





#para_h<-c(4.238940e-06 , 2.058376e-03  ,3.317425e+03 , 5.058743e-05, -8.281782e-03 , 1.2584e+02, 1.110707e+00 ,  9.84599e-01 )  ## RMSE2$rmse :0.04684522  RMSE3$rmse :  
#para_h<-c(4.238940e-06 , 2.058376e-03  ,3.317425e+03 , 5.058743e-05, -8.281782e-03 , 1.2584e+02, 1.100707e+00 ,  9.83599e-01 )  ## RMSE2$rmse :0.04617248  RMSE3$rmse :  
para_h<-c(3.238940e-06 , 2.058376e-03  ,3.317425e+03 , 5.058743e-05, -8.281782e-03 , 1.2584e+02, 1.1003707e+00 ,  9.83599e-01 )   ## RMSE2$rmse :0.04620022  RMSE3$rmse : 0.06100658 
#para_h<-c(3.238940e-06 , 2.058376e-03  ,3.317425e+03 , 5.058743e-05, -8.281782e-03 , 1.2584e+02, 1.80707e+00 ,  9.83599e-01 )  ## RMSE2$rmse :0.1153094 RMSE3$rmse :  
