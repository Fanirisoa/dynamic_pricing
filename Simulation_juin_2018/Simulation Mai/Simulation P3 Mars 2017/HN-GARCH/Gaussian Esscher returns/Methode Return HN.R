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
#Data.N=Data.N2

Data.N=Data.N2[-c(506,1462,1638,1645),]


#####################################################
###         Source function to use            #######
#####################################################
##source("F:/Simulation class Janvier 2017/HN-GARCH/Gaussian Esscher returns/Loglik Return HN sous P.R")
##source("F:/Simulation class Janvier 2017/HN-GARCH/Gaussian Esscher returns/RMSE HN-Gaussian.r")
source("C:/Users/e0g411k03dt/Desktop/Simulation P3 Mars 2017/HN-GARCH/Gaussian Esscher returns/Loglik Return HN sous P.R")

#####################################################
###         Parameters of the model           #######
#####################################################
##a0=para_h[1]; a1=para_h[2]; gama=para_h[3];  b1= para_h[4] ;  lamda0= para_h[5]   

###   Initial parameter  ####
para_h<-c( 5.881028e-07,  1.506407e-06,  4.550518e+02,  6.500114e-01,  8.596182e+00)   ## RMSE2$rmse : 0.01527964 RMSE3$rmse : 0.01589852
 
para_h<-c( 5.881028e-07,  1.506407e-06,  4.550518e+02,  6.500114e-01,  8.596182e-01)  ##  RMSE2$rmse : 0.01685706 RMSE3$rmse : 0.01818576

para_h<-c(1.881976e-07, 1.519118e-06, 4.624124e+02, 6.500325e-01, 8.598959e+00)  ##  RMSE2$rmse :  0.01527864  RMSE3$rmse : 0.0158968



para_h<-c(1.650419e-07, 1.520909e-06, 4.624124e+02, 6.500333e-01, 8.598959e+00)  ## RMSE2$rmse : 0.01527879  RMSE3$rmse : 0.01589698
 




para_h<-c(  5.881028e-07, 0.132e-06,  8.119e+00,  0.986,  0.080)  ##  RMSE2$rmse : 0.01685706 RMSE3$rmse : 0.01818576


para_h<-c(5.282379e-13, 2.252557e-05 ,8.143868e+00 ,9.154310e-01 ,1.026485e-0) ##  RMSE2$rmse : 0.02233918 RMSE3$rmse : 0.01818576

para_h<-c(5.282379e-13, 2.252557e-07 ,8.143868e+00 ,9.154310e-01 ,5.026485e-01) ##  RMSE2$rmse : 0.01890444 RMSE3$rmse : 0.01818576

para_h<-c(5.282379e-08, 2.252557e-04 ,8.143868e+00 ,9.154310e-01 ,2.026485e-0) ##  RMSE2$rmse : 0.06282301 RMSE3$rmse : 0.01818576


###   Solution 
para_h<-c(1.359738e-11, 2.218148e-04, 8.229466e+00, 6.204922e-01, 1.988846e+00)


para_h<-c(1.359738e-12, 2.218148e-04, 8.529466e+00, 4.204922e-01, 1.988846e+00) ##  RMSE2$rmse : 0.06380394 RMSE3$rmse : 0.01818576

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
ILK=Heston_likelihood_ret(para_h, Data.returns) 
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken

ILK


#####################################################
###      Optimization  of the model           #######
#####################################################
start.time <- Sys.time()
Sol=optim(para_h, Heston_likelihood_ret, Data.returns=Data.returns, method="Nelder-Mead",control = list(maxit = 5000))
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken

para_h1<-Sol$par
Sol

############################################################
####            In sample   RMSE                          ##
############################################################
source("C:/Users/e0g411k03dt/Desktop/Simulation P3 Mars 2017/HN-GARCH/Gaussian Esscher returns/RMSE HN-Gaussian.R")

start.time <- Sys.time()
RMSE1=RMSE(para_h1,Data.ret,Data.N)
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken

RMSE1$rmse


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
RMSE2=RMSE(para_h1,Data.ret,Data.N)
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken

RMSE2$rmse













############################################################
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
