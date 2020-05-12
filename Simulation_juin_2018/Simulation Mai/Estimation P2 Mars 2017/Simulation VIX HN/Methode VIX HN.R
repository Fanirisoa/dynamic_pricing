#####################################################
###              Load Data source             #######
#####################################################
##source("C:/Users/e0g411k03dt/Desktop/Simulation P3 Mars 2017/Data/Data20092010.R")
##source("C:/Users/e0g411k03dt/Desktop/Simulation P3 Mars 2017/Data/Data20112012.R")

setwd("C:/Users/Leacalin/Desktop/New Simulation June/CalibrationOption2009/Dataset chorro") 

source("I:/Simulation Mai/Simulation P3 Mars 2017/Data/Data20092010.R") 
source("I:/Simulation Mai/Simulation P3 Mars 2017/Data/Data20112012.R") 

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
###         Part 1 Mixt-estimation            #######
#####################################################
###         Source function to use            #######
#####################################################

source("C:/Users/e0g411k03dt/Desktop/Estimation P2 Mars 2017/Simulation VIX HN/Loglik Ret VIX HN.r")
source("C:/Users/e0g411k03dt/Desktop/Estimation P2 Mars 2017/Simulation VIX HN/Loglik Opt VIX HN.r")
source("C:/Users/e0g411k03dt/Desktop/Estimation P2 Mars 2017/Simulation VIX HN/Loglik Mix VIX HN.r")

#####################################################
###         Parameters of the model           #######
#####################################################
###   Initial parameter  para_h<-c() set up the parameters of the model   ####
### a0=para_h[1]; a1=para_h[2]; gama=para_h[3];  b1= para_h[4] ;  lamda0= para_h[5] ; ro=para_h[6]

para_h<-c(4.27e-07, 1.51e-06, 462.6, 0.662,0.64, 9.646950e-01) ## RMSE2$rmse :   ## RMSE3$rmse :  

para_h<-c(3.313135e-15, 1.366366e-06, 4.274284e+02, 7.324341e-01, 8.531595e+00, 9.95507e-01)  ## RMSE2$rmse : 0.0154167 ## RMSE3$rmse : 0.01615634


para_h<-c(3.788371e-13, 1.520721e-06, 4.649298e+02, 6.619980e-01, 4.400007e-01, 9.646967e-01)  ## RMSE2$rmse : 0.01082776 ## RMSE3$rmse : 0.03113633

###   Solution   para_h<-c() set up the parameters of the model   ####

para_h<-c(3.788371e-13, 1.520721e-06, 4.649298e+02, 6.619980e-01, 6.400007e-01, 9.646967e-01)  ## RMSE2$rmse : 0.0154167 ## RMSE3$rmse : 0.03113633


para_h<-c(1.905978e-08, 1.522443e-06, 4.642720e+02, 6.620030e-01, 6.399963e-01, 9.646983e-01) ## 0.01083887



para_h1<-c(5.282379e-08, 2.252557e-04 ,8.143868e+00 ,9.154310e-01 ,0.7426485e-00, 9.784247e-01) ##  RMSE2$rmse :0.05127656 RMSE3$rmse : 0.01818576
#####################################################
###         Volatility and  Price             #######
#####################################################
ts.vol= shape_vol_P(para_h, Data.returns) 
ts.plot(ts.vol, col = "steelblue", main = "IG Garch Model",xlab="2009",ylab="Volatility")
grid()

ts.vol= shape_vol_Q(para_h, Data.returns) 
ts.plot(ts.vol, col = "steelblue", main = "IG Garch Model",xlab="2009",ylab="Volatility")
grid()


#####################################################
###              LOg values                   #######
#####################################################
start.time <- Sys.time()
ILK=Heston_likelihood_MixViX(para_h,Data.returns)
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
Sol=optim(para_h,Heston_likelihood_MixViX ,Data.returns=Data.returns, method="Nelder-Mead",control = list(maxit = 10000))
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken
Sol

para_h1<-Sol$par


#####################################################
###          Part 2 QML-estimation            #######
#####################################################
###         Source function to use            #######
#####################################################

source("C:/Users/e0g411k03dt/Desktop/Estimation P2 Mars 2017/Simulation VIX HN/QMLNIG VIX HN.r")

#####################################################
###         Parameters of the model           #######
#####################################################
###   Initial parameter  para_h<-c() set up the parameters of the model   ####
### alpha=para_distribution[1];  beta=para_distribution[2];  delta=para_distribution[3];  mu=para_distribution[4];

para_distribution<-c(23.254077016, -14.397610234,   0.007012446,  -0.286886696)

para_distribution<-c(5, 1.397610234,   0.007012446,  -0.286886696)

###   Solution   para_h<-c() set up the parameters of the model   ####
para_h1<-c(2.275545e-13, 1.496457e-06, 4.634467e+02, 6.713043e-01, 1.799994e-01, 9.646982e-016)

###   Solution   para_distribution<-c() set up the parameters of the distribution NIG   ####

para_distribution1<-c(200.237095e+01 ,-1.047143e+01,  7.135328e-03 , 80.048087e-04) #### RMSE2$rmse: 0.01082957


##########################################################
#                QML estimation  NIG                     # 
##########################################################
start.time <- Sys.time()
QMLSol=optim(para_distribution,NIG_likelihood_dens_QML ,para_h =para_h1,Data.returns=Data.returns, method="Nelder-Mead",control = list(maxit = 5000))
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken
QMLSol

para_distribution1= QMLSol$par

parametres_qml=c(para_h1,para_distribution1)

#####################################################
###          Part 3 QML-RMSE Simulation       #######
#####################################################
###         Source function to use            #######
#####################################################
##source("C:/Users/e0g411k03dt/Desktop/Estimation  Paper 2 Mars 2016/Simulation VIX HN/VIX Heston N/Simulation MC VIX HN.r")

source("C:/Users/e0g411k03dt/Desktop/Estimation P2 Mars 2017/Simulation VIX HN/Simulation MC VIX HN 2.r")
source("C:/Users/e0g411k03dt/Desktop/Estimation P2 Mars 2017/Simulation VIX HN/Function Pricer VIX HN.r")
source("C:/Users/e0g411k03dt/Desktop/Estimation P2 Mars 2017/Simulation VIX HN/RMSE VIX HN.r")

##source("C:/Users/e0g411k03dt/Desktop/Estimation paper 2, juillet 2016/Inovation.r")

N= 10

############################################################
####                        RMSE                          ##
############################################################

start.time <- Sys.time()
RMSE2=RMSEsim(para_h1,para_distribution1,Data.ret,Data.N)
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken

RMSE2$rmse

#####################################################
###    Load both data.contract and data.ret   #######
#####################################################
load("DataPrice20112012.Rdata")

Data.N=Data.N2
############################################################
####                        RMSE                          ##
############################################################


start.time <- Sys.time()
RMSE2010=RMSEsim(para_h1,para_distribution1,Data.ret,Data.N)
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken

RMSE2010$rmse


############################################################
####                     RMSE Gaussian                    ##
############################################################
source("C:/Users/e0g411k03dt/Desktop/Estimation P2 Mars 2017/Simulation VIX HN/RMSE Gaussian VIX HN.r")

start.time <- Sys.time()
RMSE3=RMSEGaus(para_h1,Data.ret,Data.N)
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken

RMSE3

############################################################
####                Simulation Price                      ##
############################################################    

start.time <- Sys.time()
P_T_SimMC1=Pricer(N,para_h1,para_distribution1,Data.N)
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken

P_T_SimMC1$P
P_T_SimMC1$Yt
P_T_SimMC1$St
P_T_SimMC1$St_Mar

###################################################################
###        Part 3 Pertinance - Test -Standard_errors        #######
###################################################################
###         Source function to use            #######
#####################################################
source("C:/Users/e0g411k03dt/Desktop/Estimation paper 2, juillet 2016/Simulation VIX GJR/Pertinance VIX GJR.r")
source("C:/Users/e0g411k03dt/Desktop/Estimation paper 2, juillet 2016/Simulation VIX GJR/Test VIX GJR.r")
source("C:/Users/e0g411k03dt/Desktop/Estimation paper 2, juillet 2016/Simulation VIX GJR/Standard_errors GJR.r")

##############################################################################
#     Fit the datareturns with NIG distribution based on GH distribution     # 
##############################################################################
X_2 = E_errorHN(para_h1,Data.returns)

fitdistr(X_2, ds, start=list(a=23.254077016, b=-14.397610234, d=0.007012446,  mu=-0.286886696))
fgu <- fitdist(X_2, "s", start=para_distribution)
fgu
summary(fgu)

##########################
#    Goodness-of-fit     # 
##########################

gofstat(fgu)

plot(fgu, demp = TRUE)
plot(fgu, histo = FALSE, demp = TRUE)

##########################
#    q-q plot, p-p plot  # 
##########################
windows(width=1, height=1) 

cdfcomp(fgu, addlegend=FALSE)
denscomp(fgu, addlegend=FALSE)
ppcomp(fgu, addlegend=FALSE)
qqcomp(fgu, addlegend=FALSE)

##########################
#    jarqueberaTest      # 
##########################

jbTest(Data.returns$ret, title = NULL, description = NULL)

jarqueberaTest(Data.returns$ret)


#########################################
#  Two-sample kolmogorov-Smirnov (K-S)  # 
#########################################

ks=ks_test(para_distribution1,X)

#########################################
#             AIC and BIC               # 
#########################################
AB = Info_criteria(para_distribution,QMLSol)
AB
############################################################
####               Hessian Matrix                         ##
############################################################
start.time <- Sys.time()
Stand_errorGJR= Standard_errorsHN(para_h1)
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken
Stand_errorGJR

############################################################
####               Hessian Matrix                         ##
############################################################
start.time <- Sys.time()
Stand_errorNIG= Standard_errorsNIG(para_distribution1,para_h1)
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken
Stand_errorNIG



############################################################
####                     RMSE Gaussian                    ##
############################################################
start.time <- Sys.time()
RMSE2=RMSE(para_h1,Data.ret,Data.N)
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken

RMSE2
