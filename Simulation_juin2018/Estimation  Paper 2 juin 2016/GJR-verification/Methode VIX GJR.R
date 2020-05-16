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
###         Parameters of the model           #######
#####################################################
###   Initial parameter  para_h<-c() set up the parameters of the model   ####
### a0=para_h[1]; a1=para_h[2]; a2=para_h[3];  b1= para_h[4] ;  lamda0= para_h[5] ; ro=para_h[6]   
para_h<-c(5.192896e-06, 1.240918e-01, 2.314273e-02, 8.504267e-01, 1.821112e-01)

###   Initial parameter  para_h<-c() set up the parameters of the model   ####
### alpha=para_distribution[1];  beta=para_distribution[2];  delta=para_distribution[3];  mu=para_distribution[4];

para_distribution<-c(31.954024652, 13.890396113,  0.008067635, -0.001510164)


###   Solution   para_h<-c() set up the parameters of the model   ####

###   Solution   para_distribution<-c() set up the parameters of the distribution NIG   ####


#####################################################
###      Optimization  of the model           #######
#####################################################

start.time <- Sys.time()
Sol=optim(para_h,Heston_likelihood_GJRNIG ,Data.returns=Data.returns, method="Nelder-Mead",control = list(maxit = 10000))
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
P_T_SimMC1$Yt
P_T_SimMC1$St
P_T_SimMC1$St_Mar

############################################################
####                        RMSE                          ##
############################################################

start.time <- Sys.time()
RMSE2=RMSEsim (para_h1,para_distribution1,Data.ret,Data.N)
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken

RMSE2$rmse
RMSE2$P

Data.N=Data.Not
start.time <- Sys.time()
RMSE2=RMSEsim (para_h1,para_distribution1,Data.ret,Data.Not)
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken

RMSE2$rmse
RMSE2$P

##############################################################################
#     Fit the datareturns with NIG distribution based on GH distribution     # 
##############################################################################
X= E_error(para_h1,Data.returns)
  
fitdistr(X, ds, start=list(a=31.954024652, b=13.890396113, d=0.008067635,  mu=-0.001510164))
fgu <- fitdist(X, "s", start=para_distribution)
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
denscomp(fgu, addlegend=FALSE,demp = TRUE)
ppcomp(fgu, addlegend=FALSE)
qqcomp(fgu, addlegend=FALSE)


##########################
#    jarqueberaTest      # 
##########################

Test_jb_1=jbTest(X, title = NULL, description = NULL)
Test_jb_1@test

Test_jb=jarqueberaTest(X)
Test_jb@test
#########################################
#  Two-sample Kolmogorov-Smirnov (K???S)  # 
#########################################

ks=ks_testGJR(para_distribution1,X)

#########################################
#             AIC and BIC               # 
#########################################
AB = Info_criteria(para_distribution,QMLSol)
AB

############################################################
####               Hessian Matrix                         ##
############################################################
start.time <- Sys.time()
Stand_errorGJR= Standard_errorsGJR(para_h1)
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








