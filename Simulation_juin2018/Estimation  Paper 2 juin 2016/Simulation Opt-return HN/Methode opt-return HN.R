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

### load("Dataprice2010.Rdata")

### load("DataPrice2009test.Rdata")

#####################################################
###         Source function to use            #######
#####################################################
##source("C:/Users/e0g411k03dt/Desktop/Estimation  Paper 2 Mars 2016/Simulation VIX HN/VIX Heston N/Simulation MC VIX HN.r")
source("C:/Users/e0g411k03dt/Desktop/Estimation  Paper 2 April 2016/Simulation Opt-return HN/Simulation MC opt-return HN.r")
source("C:/Users/e0g411k03dt/Desktop/Estimation  Paper 2 April 2016/Simulation Opt-return HN/Loglik Option HN.r")
source("C:/Users/e0g411k03dt/Desktop/Estimation  Paper 2 April 2016/Simulation Opt-return HN/Loglik Opt-return HN.r")
source("C:/Users/e0g411k03dt/Desktop/Estimation  Paper 2 April 2016/Simulation Opt-return HN/Loglik Return HN.r")
source("C:/Users/e0g411k03dt/Desktop/Estimation  Paper 2 April 2016/Simulation Opt-return HN/QMLNIG opt-return HN.r")
source("C:/Users/e0g411k03dt/Desktop/Estimation  Paper 2 April 2016/Simulation Opt-return HN/Function Pricer opt-return HN.r")
source("C:/Users/e0g411k03dt/Desktop/Estimation  Paper 2 April 2016/Simulation Opt-return HN/RMSE  Opt-return HN.r")
#source("C:/Users/e0g411k03dt/Desktop/Estimation  Paper 2 April 2016/Simulation Opt-return HN/Table RMSE Opt-ret.r")
source("C:/Users/e0g411k03dt/Desktop/Estimation  Paper 2 April 2016/Simulation Opt-return HN/Pertinance Opt-ret HN.r")
source("C:/Users/e0g411k03dt/Desktop/Estimation  Paper 2 April 2016/Simulation Opt-return HN/Test Op-ret HN.r")
source("C:/Users/e0g411k03dt/Desktop/Estimation  Paper 2 April 2016/Simulation Opt-return HN/Standard_error Opt-ret HN.r")

#####################################################
###         Parameters of the model           #######
#####################################################
##a0=para_h[1]; a1=para_h[2]; gama=para_h[3];  b1= para_h[4] ;  lamda0= para_h[5]   

###   Initial parameter  ####
para_h<-c(1.180234e-12, 1.547729e-06, 4.550518e+02, 6.500111e-01 ,8.596182e+00)

###   Solution 

para_h<-c( 5.881028e-07,  1.506407e-06,  4.550518e+02,  6.500114e-01,  8.596182e+00)

###   Initial parameter  para_h<-c() set up the parameters of the model   ####
### alpha=para_distribution[1];  beta=para_distribution[2];  delta=para_distribution[3];  mu=para_distribution[4];

para_distribution<-c(2.237095e+01, -1.047143e+01,  7.135328e-03,  8.048087e-04)
###   Solution   para_distribution<-c() set up the parameters of the distribution NIG   ####

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
####                     RMSE Gaussian                    ##
############################################################
start.time <- Sys.time()
RMSE2=RMSE(para_h1,Data.ret,Data.N)
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken

RMSE2
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

N= 1200
Data.N=Data.N[1:10,]


start.time <- Sys.time()
P_T_SimMC1=Pricer(N,para_h1,para_distribution1,Data.N)
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken

P_T_SimMC1$P
P_T_SimMC1$Yt
P_T_SimMC1$St
P_T_SimMC1$St_Mar

############################################################
####                        RMSE                          ##
############################################################
start.time <- Sys.time()
RMSE2=RMSEsim(para_h1,para_distribution1,Data.ret,Data.N)
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken

RMSE2$rmse
RMSE2$P


############################################################################
###    Persistence, Leverage coefficient, Annualized volatility      #######
############################################################################
PLAL(para_h1)

##############################################################################
#     Fit the datareturns with NIG distribution based on GH distribution     # 
##############################################################################
X= E_error(para_distribution1,para_h1,Data.returns)

ts.plot(X, col = "steelblue", main = "IG Garch Model",xlab="2009",ylab="Volatility")
grid()

fitdistr(X, ds, start=list(a=31.954024652, b=13.890396113, d=0.008067635,  mu=-0.001510164))
fgu <- fitdist(X, "s", start=para_distribution)
fgu
summary(fgu)
para_distribution<-c(1.785, 0.175, 1.1749, 0.174)
(-15905.68-8)/(-2)
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
ks


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






