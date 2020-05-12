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
source("C:/Users/e0g411k03dt/Desktop/Simulation P3 Mars 2017/HN-GARCH/Gaussian Esscher returns-VIX/Loglik Return HN sous P.R")
source("C:/Users/e0g411k03dt/Desktop/Simulation P3 Mars 2017/HN-GARCH/Gaussian Esscher returns-VIX/Loglik   VIX HN.R")
source("C:/Users/e0g411k03dt/Desktop/Simulation P3 Mars 2017/HN-GARCH/Gaussian Esscher returns-VIX/Loglik Mix Ret VIX HN.R")



#####################################################
###         Parameters of the model           #######
#####################################################
### a0=para_h[1]; a1=para_h[2]; gama=para_h[3];  b1= para_h[4] ;  lamda0= para_h[5] ; ro=para_h[6]

###   Initial parameter  ####
para_h<-c(1.4579e-07, 1.28809e-06, 3.89124e+02, 6.564333e-01, 7.99959e+00, 9.36550e-01)   ## RMSE2$rmse : 0.01548827 ## RMSE3$rmse : 0.01626734

para_h<-c(3.313135e-15, 1.366366e-06, 4.274284e+02, 7.324341e-01, 8.531595e+00, 9.95507e-01)  ## RMSE2$rmse : 0.0154167 ## RMSE3$rmse : 0.01615634  

para_h<-c(1.791603e-13, 1.366366e-06, 4.274284e+02, 7.323953e-01, 8.431636e+00, 9.9992e-01)   ## RMSE2$rmse :  0.01542918 ## RMSE3$rmse :  #0.0161758
###   Solution 

para_h<-c(1.791603e-11, 1.366366e-06, 3.274284e+02, 6.323953e-01, 8.141636e+00, 9.9999e-01)   ## RMSE2$rmse :  0.01542918 ## RMSE3$rmse :  #0.0161758

para_h<-c(1.302488e-11, 1.366365e-06, 4.275415e+02, 7.323849e-01, 8.431641e+00, 9.999201e-01) ## RMSE2$rmse : 0.0154288

para_h<-c(1.242516e-12, 1.366366e-06, 4.275503e+02, 7.323708e-01, 8.431643e+00,9.999247e-01)
para_h<-c(1.242516e-11, 1.366366e-06, 4.275503e+02, 7.323708e-01, 8.431643e+00,9.999247e-01) ## 0.01542917


para_h<-c(  5.881028e-07, 0.132e-08,  8.119e+00,  0.986,  0.080, 9.999247e-01)  ##  RMSE2$rmse : 0.02890324 RMSE3$rmse : 0.01818576


para_h<-c(  5.881028e-07, 0.132e-08,  8.119e+00,  0.986,  0.080, 9.999247e-01)  ##  RMSE2$rmse : 0.03124939 RMSE3$rmse : 0.01818576

para_h<-c(  5.881028e-04, 0.132e-06,  8.119e+01,  0.986,  0.060, 8.784247e-01)  ##  RMSE2$rmse : 0.02252047 RMSE3$rmse : 0.01818576

para_h<-c(5.282379e-08, 2.252557e-04 ,8.143868e+00 ,9.154310e-01 ,2.026485e-0, 8.784247e-01) ##  RMSE2$rmse : 0.04819471 RMSE3$rmse : 0.01818576

para_h<-c(5.282379e-08, 2.252557e-04 ,8.143868e+00 ,9.154310e-01 ,2.026485e-03, 8.784247e-01) ##  RMSE2$rmse : 0.04776296 RMSE3$rmse : 0.01818576

para_h<-c(5.282379e-08, 2.252557e-04 ,8.143868e+00 ,9.154310e-01 ,1.026485e-00, 8.784247e-01) ##  RMSE2$rmse : 0.05033635 RMSE3$rmse : 0.01818576

para_h<-c(5.282379e-08, 2.252557e-04 ,8.143868e+00 ,9.154310e-01 ,1.826485e-00, 9.784247e-01) ##  RMSE2$rmse : 0.05069971 RMSE3$rmse : 0.01818576

para_h<-c(5.282379e-08, 2.252557e-04 ,8.143868e+00 ,9.154310e-01 ,0.526485e-00, 9.784247e-01) ##  RMSE2$rmse : 0.05106537 RMSE3$rmse : 0.01818576

para_h<-c(5.282379e-08, 2.252557e-04 ,8.143868e+00 ,9.154310e-01 ,0.626485e-00, 9.784247e-01) ##  RMSE2$rmse : 0.05140848 RMSE3$rmse : 0.01818576

para_h<-c(5.282379e-08, 2.252557e-04 ,8.143868e+00 ,9.154310e-01 ,0.7426485e-00, 9.784247e-01) ##  RMSE2$rmse :0.05127656 RMSE3$rmse : 0.01818576

para_h<-c(5.282379e-08, 2.252557e-04 ,8.143868e+00 ,9.154310e-01 ,0.6226485e-00, 9.784247e-01) ##  RMSE2$rmse :0.05127656 RMSE3$rmse : 0.01818576

para_h<-c(2.176029e-13, 5.219194e-04, 8.622389e+00, 6.255603e-01, 0.7426485e-00,9.784247e-01) ##  RMSE2$rmse :0.08123455 RMSE3$rmse : 0.01818576

para_h<-c(2.176029e-10, 3.219194e-04, 8.622389e+00, 6.255603e-01, 0.7426485e-00,9.784247e-01) ##  RMSE2$rmse :0.0573787 RMSE3$rmse : 0.07463835

para_h<-c(2.176029e-12, 4.219194e-04, 8.622389e+00, 6.255603e-01, 0.7426485e-00,9.784247e-01) ##  RMSE2$rmse :0.0649297 RMSE3$rmse : 0.01818576


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

############################################################
####            In sample   RMSE                          ##
############################################################
source("C:/Users/e0g411k03dt/Desktop/Simulation P3 Mars 2017/HN-GARCH/Gaussian Esscher returns-VIX/RMSE HN-Gaussian.R")

start.time <- Sys.time()
RMSE1=RMSE(para_h1,Data.ret,Data.N)
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken

RMSE1$rmse


############################################################
####                       Compare VIX                    ##
############################################################
source("C:/Users/e0g411k03dt/Desktop/Simulation P3 Mars 2017/HN-GARCH/Gaussian Esscher returns-VIX/Comparing  VIX_HN_GARCH.R")

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
####           out sample   RMSE                          ##
############################################################
Data.N=Data.N2

start.time <- Sys.time()
RMSE2=RMSE(para_h1,Data.ret,Data.N)
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken

RMSE2$rmse

