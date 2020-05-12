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
source("C:/Users/e0g411k03dt/Desktop/Simulation P3 Mars 2017/HN-GARCH/Gaussian Quadratic-Esscher returns-option/Loglik Return HN sous P.r")
source("C:/Users/e0g411k03dt/Desktop/Simulation P3 Mars 2017/HN-GARCH/Gaussian Quadratic-Esscher returns-option/Loglik Option HN Qua.r")
source("C:/Users/e0g411k03dt/Desktop/Simulation P3 Mars 2017/HN-GARCH/Gaussian Quadratic-Esscher returns-option/Loglik Opt-return HN Qua.r")

#####################################################
###         Parameters of the model           #######
#####################################################
##a0=para_h[1]; a1=para_h[2]; gama=para_h[3];  b1= para_h[4] ;  lamda0= para_h[5]   ; Pi=para_h[6] 


###   a0=para_h[1]; a1=para_h[2]; gama=para_h[3];  b1= para_h[4] ;  lamda0= para_h[5]  ; Pi=para_h[6] ; ro=para_h[7]

para_h<-c(5.282379e-08, 2.252557e-04 ,8.143868e+00 ,9.154310e-01 ,0.826485e-00,1.17890e+00) ##  RMSE2$rmse : 0.02947573 RMSE3$rmse : 0.01818576



###   Initial parameter  ####
para_h<-c(1.180234e-12, 1.547729e-06, 4.550518e+02, 6.500111e-01 ,8.596182e+00,1.302)

para_h<-c(5.881028e-07,  1.506407e-06,  4.550518e+02,  6.500114e-01,  8.596182e+00,1.302)

###   Solution 

para_h<-c(2.278319e-05, 1.969895e-04, 9.964591e+00, 1.419832e-01, 1.723114e-09, 1.372333e+00)


para_h<-c(2.278319e-05, 1.969895e-04, 9.964591e+00, 1.419832e-01, 1.723114e-03, 1.372333e+00)  ##  RMSE2$rmse : 0.02956124


para_h<-c(2.278319e-05, 1.969895e-04, 9.964591e+00, 1.419832e-01, 1.723114e-03, 1.272333e+00)  ##  RMSE2$rmse : 0.02956124

para_h<-c(2.176029e-07, 3.219194e-04, 8.622389e+00, 6.255603e-01, 0.5426485e-01, 1.272333e+00) ##  RMSE2$rmse :0.08940102   RMSE3$rmse :  

para_h<-c(2.176029e-08, 3.219194e-04, 7.622389e+00, 6.255603e-01, 0.5426485e-00, 1.172333e+00) ##  RMSE2$rmse :0.08889326   RMSE3$rmse :  

para_h<-c(2.354299e-05, 3.345238e-04 ,7.542406e+00 ,1.124012e-01 ,1.573458e-02, 1.699999e+00) ##  RMSE2$rmse : 0.04460134   RMSE3$rmse :  

para_h<-c(2.354299e-08, 3.345238e-04 ,7.542406e+00 ,1.124012e-01 ,6.573458e-01, 1.38245e+00) ##  RMSE2$rmse : 0.04469531   RMSE3$rmse :  


para_h<-c(2.354299e-02, 3.345238e-04 ,7.542406e+00 ,1.124012e-01 ,6.573458e-01, 1.38245e+00) ##  RMSE2$rmse : 627195.9   RMSE3$rmse :  



para_h<-c(5.354299e-04, 3.345238e-04 ,1.542406e+00 ,1.124012e-01 ,6.573458e-01, 1.38245e+00) ##  RMSE2$rmse : 0.09862038  RMSE3$rmse :  

para_h<-c(2.354299e-04, 3.345238e-03 ,0.542406e+00 ,1.124012e-02 ,6.573458e-00, 1.18245e+00) ##  RMSE2$rmse : 724355.3  RMSE3$rmse :  

para_h<-c(2.354299e-04, 3.345238e-06 ,0.542406e+02 ,1.124012e-03 ,6.573458e-01, 1.18245e+00)  ##  RMSE2$rmse : 0.02779117  RMSE3$rmse :

para_h<-c(2.354299e-04, 3.345238e-05 ,0.542406e+01 ,1.124012e-03 ,6.573458e-01, 1.18245e+00) ##  RMSE2$rmse : 0.03059301  RMSE3$rmse :

para_h<-c(6.354299e-04, 3.345238e-05 ,0.842406e+01 ,1.124012e-03 ,6.573458e-01, 1.38245e+00) ##  RMSE2$rmse : 0.071117  RMSE3$rmse :


para_h<-c(5.354299e-04, 3.345238e-05 ,0.842406e+01 ,1.124012e-03 ,6.573458e-01, 1.38245e+00) ##  RMSE2$rmse : 0.06137047  RMSE3$rmse :


para_h<-c(4.354299e-04, 3.345238e-05 ,0.842406e+01 ,1.124012e-03 ,6.573458e-01, 1.38245e+00) ##  RMSE2$rmse : 0.05129914  RMSE3$rmse :

para_h<-c(4.554299e-04, 3.345238e-05 ,0.842406e+01 ,1.124012e-03 ,6.573458e-01, 1.38245e+00) ##  RMSE2$rmse : 0.05333775  RMSE3$rmse :


para_h<-c(4.574299e-04, 3.345238e-05 ,0.842406e+01 ,1.124012e-03 ,6.573458e-01, 1.38245e+00) ##  RMSE2$rmse : 0.05333775  RMSE3$rmse :



para_h<-c(4.454299e-04, 3.345238e-05 ,0.842406e+01 ,1.124012e-03 ,6.573458e-01, 1.38245e+00) ##  RMSE2$rmse :  0.05231986 RMSE3$rmse :


para_h<-c(4.454299e-04, 3.25618e-05 ,0.902316e+01 ,1.201012e-03 ,6.492458e-01, 1.35645e+00) ##  RMSE2$rmse :  0.05224534 RMSE3$rmse :


#####################################################
###               Volatility                  #######
#####################################################

ts.vol= shape_vol_P(para_h, Data.returns) 
ts.plot(ts.vol, col = "steelblue", main = "IG Garch Model",xlab="2009",ylab="Volatility")
grid()


#####################################################
###              LOg values                   #######
#####################################################
start.time <- Sys.time()
ILK=Heston_likelihood_Mix(para_h,Data.ret, Data.N,Data.returns)
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken

Heston_likelihood_opti(para_h, Data.ret, Data.N)
Heston_likelihood_ret(para_h, Data.returns)

#####################################################
###      Optimization  of the model           #######
#####################################################
start.time <- Sys.time()
Sol=optim(para_h,Heston_likelihood_Mix ,Data.ret=Data.ret, Data.N = Data.N,Data.returns=Data.returns, method="Nelder-Mead",control = list(maxit = 5000))
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken

para_h1<-Sol$par

############################################################
####            In sample   RMSE                          ##
############################################################
source("C:/Users/e0g411k03dt/Desktop/Simulation P3 Mars 2017/HN-GARCH/Gaussian Quadratic-Esscher returns-option/RMSE HN-Gaussian  Qua.r")

start.time <- Sys.time()
RMSE1=RMSE(para_h ,Data.ret,Data.N)
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken

RMSE1$rmse

para_h1<-para_h
############################################################
####                       Compare VIX                    ##
############################################################
source("C:/Users/e0g411k03dt/Desktop/Simulation P3 Mars 2017/HN-GARCH/Gaussian Quadratic-Esscher returns-VIX/Comparing  VIX_HN_GARCH_Qua.R")

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