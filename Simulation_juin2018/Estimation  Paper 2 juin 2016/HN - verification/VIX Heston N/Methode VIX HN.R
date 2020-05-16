rm(list=ls())
gc()
library(compiler)
enableJIT(1)
enableJIT(3)
library("fBasics")
#####################################################
###    Load both data.contract and data.ret   #######
#####################################################
load("DataPrice2009.Rdata")

## load("Dataprice2010.Rdata")


#####################################################
###         Source function to use            #######
#####################################################
source("C:/Users/e0g411k03dt/Desktop/Estimation  Paper 2 April 2016/Simulation VIX HN/VIX Heston N/Loglik Ret VIX HN.r")
source("C:/Users/e0g411k03dt/Desktop/Estimation  Paper 2 April 2016/Simulation VIX HN/VIX Heston N/Loglik Opt VIX HN.r")
source("C:/Users/e0g411k03dt/Desktop/Estimation  Paper 2 April 2016/Simulation VIX HN/VIX Heston N/Loglik Mix VIX HN.r")
source("C:/Users/e0g411k03dt/Desktop/Estimation  Paper 2 April 2016/Simulation VIX HN/VIX Heston N/QMLNIG VIX HN.r")
source("C:/Users/e0g411k03dt/Desktop/Estimation  Paper 2 April 2016/Simulation VIX HN/VIX Heston N/Simulation MC VIX HN 2.r")
source("C:/Users/e0g411k03dt/Desktop/Estimation  Paper 2 April 2016/Simulation VIX HN/VIX Heston N/Function Pricer VIX HN.r")
source("C:/Users/e0g411k03dt/Desktop/Estimation  Paper 2 April 2016/Simulation VIX HN/VIX Heston N/RMSE VIX HN.r")
source("C:/Users/e0g411k03dt/Desktop/Estimation  Paper 2 April 2016/Simulation VIX HN/VIX Heston N/Pertinance VIX HN.r")
source("C:/Users/e0g411k03dt/Desktop/Estimation  Paper 2 April 2016/Simulation VIX HN/VIX Heston N/Test VIX HN.r")
source("C:/Users/e0g411k03dt/Desktop/Estimation  Paper 2 April 2016/Simulation VIX HN/VIX Heston N/Standard_error VIX HN.r")
source("C:/Users/e0g411k03dt/Desktop/Estimation  Paper 2 April 2016/Inovation.r")
#####################################################
###         Parameters of the model           #######
#####################################################
###   Initial parameter  para_h<-c() set up the parameters of the model   ####
### a0=para_h[1]; a1=para_h[2]; gama=para_h[3];  b1= para_h[4] ;  lamda0= para_h[5] ; ro=para_h[6]

para_h<-c(4.27e-07, 1.51e-06, 462.6, 0.662,0.64)

###   Initial parameter  para_h<-c() set up the parameters of the model   ####
### alpha=para_distribution[1];  beta=para_distribution[2];  delta=para_distribution[3];  mu=para_distribution[4];

para_distribution<-c(23.254077016, -14.397610234,   0.007012446,  -0.286886696)

para_distribution<-c(5, 1.397610234,   0.007012446,  -0.286886696)

###   Solution   para_h<-c() set up the parameters of the model   ####
para_h<-c(2.275545e-13, 1.496457e-06, 4.634467e+02, 6.713043e-01, 1.799994e-01)

###   Solution   para_distribution<-c() set up the parameters of the distribution NIG   ####

para_distribution1<-c(2.237095e+01 ,-1.047143e+01,  7.135328e-03 , 8.048087e-04)

#####################################################
###         Volatility and  Price             #######
#####################################################

ts.vol=ht(para_h,Data.returns)
ts.plot(ts.vol, col = "steelblue", main = "IG Garch Model",xlab="2009",ylab="Volatility")
grid()

ts.vol1=hstar(para_h,Data.returns)
ts.plot(ts.vol1, col = "steelblue", main = "IG Garch Model",xlab="2009",ylab="Volatility")
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
                 
############################################################
####                Simulation Price                      ##
############################################################    
Sl=Seuil(para_h1,para_distribution1)
Sl

N= 5

  
start.time <- Sys.time()
P_T_SimMC1=Pricer(N,para_h1,para_distribution1,Data.N)
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken

P_T_SimMC1$P
#P_T_SimMC1$Yt
#P_T_SimMC1$St
#P_T_SimMC1$St_Mar

############################################################
####                        RMSE                          ##
############################################################

start.time <- Sys.time()
RMSE2=RMSEsim(para_h1,para_distribution1,Data.ret,Data.N)
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken

RMSE2$rmse
#RMSE2$P
RMSE2$error



#####################################################
###           MLE HN-Gaussian model           #######
#####################################################

start.time <- Sys.time()
Sol2=optim(para_h,Heston_likelihood_MLEGauss ,Data.returns=Data.returns, method="Nelder-Mead",control = list(maxit = 10000))
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken
Sol2

para_h2<-Sol2$par



start.time <- Sys.time()
RMSE2=RMSE(para_h2,Data.ret,Data.N)
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken

RMSE2$rmse
#RMSE2$P
RMSE2$Er


start.time <- Sys.time()
RMSE3=RMSEsim(para_h2,Data.ret,Data.N)
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken

RMSE3$rmse
#RMSE3$P
RMSE3$error




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





