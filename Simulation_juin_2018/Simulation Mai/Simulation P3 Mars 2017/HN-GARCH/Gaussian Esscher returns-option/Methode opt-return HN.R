#####################################################
###              Load Data source             #######
#####################################################
# source("C:/Users/e0g411k03dt/Desktop/Simulation P3 Mars 2017/Data/Data20092010.R")
# source("C:/Users/e0g411k03dt/Desktop/Simulation P3 Mars 2017/Data/Data20112012.R")



source("C:/Users/fanir/Desktop/Historique_thesis/Thesis_main/Simulation_juin_2018/Simulation Mai/Simulation P3 Mars 2017/Data/Data20092010.R")
source("C:/Users/fanir/Desktop/Historique_thesis/Thesis_main/Simulation_juin_2018/Simulation Mai/Simulation P3 Mars 2017/Data/Data20112012.R")
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
#Data.N=Data.N2

Data.N=Data.N2[-c(506,1462,1638,1645),]


#####################################################
###         Source function to use            #######
#####################################################
source("C:/Users/e0g411k03dt/Desktop/Simulation P3 Mars 2017/HN-GARCH/Gaussian Esscher returns-option/Loglik Return HN sous P.r")
source("C:/Users/e0g411k03dt/Desktop/Simulation P3 Mars 2017/HN-GARCH/Gaussian Esscher returns-option/Loglik Option HN.r")
source("C:/Users/e0g411k03dt/Desktop/Simulation P3 Mars 2017/HN-GARCH/Gaussian Esscher returns-option/Loglik Opt-return HN.r")


#####################################################
###         Parameters of the model           #######
#####################################################
##a0=para_h[1]; a1=para_h[2]; gama=para_h[3];  b1= para_h[4] ;  lamda0= para_h[5]   



para_h<-c(2.176029e-10, 3.219194e-04, 8.622389e+00, 6.255603e-01, 0.7426485e-00) ##  RMSE2$rmse :0.04552917 RMSE3$rmse : 0.07463835

para_h<-c(2.176029e-07, 3.219194e-04, 8.622389e+00, 6.255603e-01, 0.7426485e-00) ##  RMSE2$rmse :0.04552917 RMSE3$rmse : 0.07463835

para_h<-c(2.176029e-07, 3.219194e-04, 8.622389e+00, 6.255603e-01, 0.5426485e-01) ##  RMSE2$rmse : 0.08940102 RMSE3$rmse :  

para_h<-c(1.176029e-05, 3.219194e-04, 8.622389e+00, 6.255603e-01, 0.5426485e-01) ##  RMSE2$rmse : 0.0921043 RMSE3$rmse :  


para_h<-c(1.176029e-06, 3.219194e-04, 8.622389e+00, 6.255603e-01, 0.5426485e-00) ##  RMSE2$rmse : 0.09010405 RMSE3$rmse :  

para_h<-c(3.059001e-05, 3.154301e-04, 9.654901e+00 ,9.235399e-02, 4.323253e-08) ##  RMSE2$rmse : 0.04256372 RMSE3$rmse :  

para_h<-c(3.059001e-05, 3.154301e-04, 9.654901e+00 ,9.235399e-02, 4.323253e-01) ##  RMSE2$rmse : 0.04264276 RMSE3$rmse :  

para_h<-c(3.059001e-05, 3.154301e-04, 9.654901e+00 ,9.235399e-02, 4.323253e+01) ##  RMSE2$rmse : 0.3674997 RMSE3$rmse :  

para_h<-c(3.059001e-05, 3.154301e-04, 9.654901e+00 ,9.235399e-02, 4.323253e+00) ##  RMSE2$rmse : 0.04361686 RMSE3$rmse :  

para_h<-c(3.059001e-05, 3.154301e-04, 9.654901e+00 ,9.235399e-02, 4.323253e+00) ##    RMSE2$rmse : 0.04361686 RMSE3$rmse :  

para_h<-c(7.059001e-04, 3.154301e-04, 9.654901e+00 ,9.235399e-02, 4.323253e+00) ##  RMSE2$rmse : 1251952670  RMSE3$rmse :


para_h<-c(7.059001e-04, 3.154301e-04, 9.654901e+00 ,9.235399e-02, 4.323253e+00) ##  RMSE2$rmse : 0.1161005  RMSE3$rmse :


para_h<-c(7.059001e-04, 3.154301e-04, 9.654901e+00 ,9.235399e-02, 8.323253e-01) ##  RMSE2$rmse : 0.1161005  RMSE3$rmse :

para_h<-c(7.059001e-04, 3.154301e-04, 9.654901e+00 ,9.235399e-02, 8.323253e-01) ##  RMSE2$rmse : 0.1134656 RMSE3$rmse :

para_h<-c(7.059001e-03, 3.154301e-04, 9.654901e+00 ,9.235399e-02, 8.323253e-01) ##  RMSE2$rmse :0.1134656

###   Initial parameter  ####
para_h<-c(1.180234e-12, 1.547729e-06, 4.550518e+02, 6.500111e-01 ,8.596182e+00)    

para_h<-c(5.881028e-07,  1.506407e-06,  4.550518e+02,  6.500114e-01,  8.596182e+00)

###   Solution 

para_h<-c(1.650419e-07, 1.520909e-06, 4.624124e+02, 6.500333e-01, 8.598959e+00)

para_h<-c(1.359738e-12, 2.218148e-04, 8.529466e+00, 4.204922e-01, 1.988846e+00) ##  RMSE2$rmse : 0.04296962 RMSE3$rmse : 0.01818576

para_h<-c(1.359738e-12, 2.218148e-04, 8.529466e+00, 4.204922e-01, 0.988846e+00) ##  RMSE2$rmse : 0.04275584 RMSE3$rmse : 0.01818576

para_h<-c(1.359738e-12, 2.218148e-04, 8.529466e+00, 4.204922e-01, 0.588846e+00) ##  RMSE2$rmse : 0.04267984 RMSE3$rmse : 0.01818576

para_h<-c(7.059001e-03, 3.154301e-04, 9.654901e+00 ,9.235399e-02, 8.323253e-01) ##  RMSE2$rmse :0.752232


para_h<-c(7.059001e-07, 3.154301e-04, 9.654901e+00 ,9.235399e-02, 8.323253e-01) ##  RMSE2$rmse :0.0400939


para_h<-c(7.059001e-07, 3.154301e-04, 9.654901e+00 ,9.235399e-02, 5.323253e-01) ##  RMSE2$rmse : 0.04005725

para_h<-c(7.059001e-07, 3.154301e-04, 9.654901e+00 ,9.235399e-02, 1.323253e-00) ##  RMSE2$rmse : 0.04015634

para_h<-c(7.059001e-07, 3.154301e-04, 9.654901e+00 ,9.235399e-02, 5.323253e-00) ##  RMSE2$rmse : 0.04088381

para_h<-c(7.059001e-07, 3.154301e-04, 9.654901e+00 ,9.235399e-02, 5.323253e-00) ##  RMSE2$rmse : 0.04088381

para_h<-c(7.059001e-06, 3.154301e-04, 9.654901e+00 ,9.235399e-02, 5.323253e-00) ##  RMSE2$rmse : 0.04145688

para_h<-c(7.059001e-05, 3.154301e-04, 9.654901e+00 ,9.235399e-02, 5.323253e-00) ##  RMSE2$rmse : 0.04145688

para_h<-c(2.354299e-03, 3.345238e-04 ,0.142406e+01 ,1.124012e-03 ,6.573458e-01)  ##  RMSE2$rmse : 0.04860108

para_h<-c(3.354299e-04, 3.345238e-04 ,0.142406e+01 ,1.124012e-03 ,6.573458e-01)  ##  RMSE2$rmse : 0.07050789


para_h<-c(1.354299e-04, 3.345238e-04 ,0.142406e+01 ,1.124012e-03 ,6.573458e-01)  ##  RMSE2$rmse : 0.05090895

para_h<-c(1.754299e-04, 3.345238e-04 ,0.142406e+01 ,1.124012e-03 ,6.573458e-01)  ##  RMSE2$rmse : 0.05290983
para_h<-c(1.854299e-04, 3.345238e-04 ,0.142406e+01 ,1.124012e-03 ,6.573458e-01)  ##  RMSE2$rmse : 0.05589917


para_h<-c(1.874299e-04, 3.345238e-04 ,0.142406e+01 ,1.124012e-03 ,6.573458e-01)  ##  RMSE2$rmse : 0.05589917 ##  RMSE2$rmse : 0.07343432


para_h<-c(1.874299e-04, 3.345238e-04 ,0.942406e+01 ,1.124012e-03 ,6.573458e-01)  ##  RMSE2$rmse : 0.05709637 



para_h<-c(1.554299e-04, 3.345238e-04 ,0.142406e+01 ,1.124012e-03 ,6.573458e-01)  ##  RMSE2$rmse : 0.05090895

para_h<-c(2.354299e-04, 3.345238e-04 ,0.142406e+01 ,1.124012e-03 ,6.573458e-01)  ##  RMSE2$rmse : 0.06083725

para_h<-c(2.554299e-04, 3.345238e-04 ,0.142406e+01 ,1.124012e-03 ,6.573458e-01)  ##  RMSE2$rmse :  0.06279438

para_h<-c(2.554299e-04, 3.345238e-04 ,0.142406e+01 ,1.124012e-03 ,6.573458e-01)  ##  RMSE2$rmse :  0.06279438

#####################################################
###              LOg values                   #######
#####################################################
start.time <- Sys.time()
ILK=Heston_likelihood_Mix(para_h,Data.ret, Data.N,Data.returns)
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken

Heston_likelihood_ret(para_h, Data.returns)
Heston_likelihood_opti(para_h, Data.ret, Data.N)

#####################################################
###      Optimization  of the model           #######
#####################################################
start.time <- Sys.time()
Sol=optim(para_h,Heston_likelihood_Mix ,Data.ret=Data.ret, Data.N = Data.N,Data.returns=Data.returns, method="Nelder-Mead",control = list(maxit = 5000))
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken

para_h1<-Sol$par
Sol

############################################################
####                        RMSE                          ##
############################################################
source("C:/Users/e0g411k03dt/Desktop/Simulation P3 Mars 2017/HN-GARCH/Gaussian Esscher returns-option/RMSE HN-Gaussian.r")

start.time <- Sys.time()
RMSE1=RMSE(para_h,Data.ret,Data.N)
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken

RMSE1$rmse

para_h1<-para_h
############################################################
####                       Compare VIX                    ##
############################################################
source("C:/Users/e0g411k03dt/Desktop/Simulation P3 Mars 2017/HN-GARCH/Gaussian Esscher returns/Comparing  VIX_HN_GARCH.R")

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
RMSE2=RMSE(para_h,Data.ret,Data.N)
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken

RMSE2$rmse

