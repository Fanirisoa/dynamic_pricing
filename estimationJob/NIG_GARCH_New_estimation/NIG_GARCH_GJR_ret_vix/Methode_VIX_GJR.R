###################################################################################
###               Clean the repertoir and laod all the library used         #######
###################################################################################
rm(list=ls())
gc()
library(compiler)
enableJIT(1)
enableJIT(3)
library("fBasics")
#library("pracma")
library("numDeriv")
library("nlme")
library("Matrix")
library(xts)


##################################################################################################
###              Load : Data source,    Parameters of the model,  function to use          #######
##################################################################################################
setwd("/Users/leafanirisoa/Documents/projetGit/dynamic_pricing/data_used")  
path = "/Users/leafanirisoa/Documents/projetGit/dynamic_pricing/estimationJob/NIG_GARCH_New_estimation/NIG_GARCH_GJR_ret_vix"

source(paste(path,"/parameters_set.R",sep=""))
source(paste(path,"/Loglik_Return_HN.R",sep=""))




source(paste(path,"/Fun_Pricer_opt_ret_HN.R",sep=""))
source(paste(path,"/MCSim_opt_ret_HN.R",sep=""))
source(paste(path,"/Loglik_Option_HN.R",sep=""))
source(paste(path,"/Loglik_Opt_ret_HN.R",sep=""))






#####################################################
###              Load Data source             #######
#####################################################
setwd("C:/Users/fanir/Desktop/Simulation_juin2018/Data")  

#####################################################
###             Clean the repertoir           #######
#####################################################
rm(list=ls())
gc()
library(compiler)
enableJIT(1)
enableJIT(3)
library("fBasics")
#library("pracma")
library("numDeriv")
library("nlme")
library("Matrix")


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
###         Part 1 Mixt-estimation            #######
#####################################################
###         Source function to use            #######
#####################################################
source("C:/Users/fanir/Desktop/Simulation_juin2018/NIG_GARCH_GJR/NIG_GARCH_GJR_ret/Loglik_Ret_VIX_GJR.r")
source("C:/Users/fanir/Desktop/Simulation_juin2018/NIG_GARCH_GJR/NIG_GARCH_GJR_ret/Loglik_Opt_VIX_GJR.r")
source("C:/Users/fanir/Desktop/Simulation_juin2018/NIG_GARCH_GJR/NIG_GARCH_GJR_ret/Loglik_Mix_VIX_GJR.r")

#####################################################
###         Parameters of the model           #######
#####################################################
###   Initial parameter  para_h<-c() set up the parameters of the model   ####

### a0=para_h[1]; a1=para_h[2]; a2=para_h[3];  b1= para_h[4] ;  lamda0= para_h[5] ; ro=para_h[6]   
 para_h<-c(5.192896e-06, 1.240918e-01, 2.314273e-02, 8.504267e-01, 1.821112e-01, 9.657773e-01)

para_h<-c(5.987174e-06,  1.240911e-01,  2.314265e-02,  8.504269e-01,  3.784983e-02, 9.546611e-01)  ## RMSE1$rmse : 0.06265758 RMSE3$rmse :0.07367674

para_h1<-c(2.275545e-13, 1.496457e-06, 4.634467e+02, 6.713043e-01, 1.799994e-01, 9.646982e-016)

###   Solution   para_h<-c() set up the parameters of the model   ####

#####################################################
###               Volatility  shape           #######
#####################################################

ts.vol_P= shape_vol_P (para_h, Data.returns) 
ts.plot(ts.vol_P , col = "steelblue", main = "IG Garch Model",xlab="2009",ylab="Volatility")
grid()


ts.vol_Q= shape_vol_Q (para_h, Data.returns) 
ts.plot(ts.vol_Q, col = "steelblue", main = "IG Garch Model",xlab="2009",ylab="Volatility")
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
Sol=optim(para_h,Heston_likelihood_RET_solo ,Data.returns=Data.returns, method="Nelder-Mead",control = list(maxit = 10000))
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken
Sol

para_h1<-Sol$par

# Standard error
Hess=fdHess(para_h1,Heston_likelihood_RET_solo, Data.returns=Data.returns)
S_e <- sqrt(diag(solve(nearPD(Hess$Hessian)$mat)))
S_e


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
source("C:/Users/fanir/Desktop/Simulation_juin2018/NIG_GARCH_GJR/NIG_GARCH_GJR_ret/QMLNIG_VIX_GJR.r")

#####################################################
###         Parameters of the model           #######
#####################################################
###   Initial parameter  para_h<-c() set up the parameters of the model   ####
### alpha=para_distribution[1];  beta=para_distribution[2];  delta=para_distribution[3];  mu=para_distribution[4];

###   Solution   para_distribution<-c() set up the parameters of the distribution NIG   ####

para_distribution<-c(50.9889567263, -4.2128767496 , 0.0095076256 , 0.0001846889)

para_distribution<-c(48.4956802775, -5.5025899512 , 0.0091739853 , 0.0003863926)

para_distribution<-c(23.254077016, -14.397610234,   0.007012446,  -0.286886696)

para_distribution<-c(5, 1.397610234,   0.007012446,  -0.286886696)

para_distribution<-c(200.237095e+01 ,-1.047143e+01,  7.135328e-03 , 80.048087e-04)
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

# Standard error
Hess2=fdHess(para_distribution1,NIG_likelihood_dens_QML,para_h =para_h1, Data.returns=Data.returns)
S_e2 <- sqrt(diag(solve(nearPD(Hess2$Hessian)$mat)))
S_e2

#####################################################
###          Part 3 QML-RMSE Simulation       #######
#####################################################
###         Source function to use            #######
#####################################################
source("C:/Users/e0g411k03dt/Desktop/Sim_nov_2017_paper_2/Simulation VIX GJR/Normalisation_parametrization/MCSim_VIX_GJR.r")
source("C:/Users/e0g411k03dt/Desktop/Sim_nov_2017_paper_2/Simulation VIX GJR/Normalisation_parametrization/Fun_Pricer_VIX_GJR.r")
source("C:/Users/e0g411k03dt/Desktop/Sim_nov_2017_paper_2/Simulation VIX GJR/Normalisation_parametrization/RMSE_VIX_GJR.r")

N= 2

############################################################
####                        RMSE                          ##
############################################################
start.time <- Sys.time()
RMSE2=RMSEsim (para_h1,para_distribution1,Data.ret,Data.N)
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken

RMSE2$rmse


#####################################################
###          Part 4 VRP      Simulation       #######
#####################################################
#####################################################
###    Load both data.contract and data.ret   #######
#####################################################
Date.ts=Data.returns$date
###############################
###    Load fonctions   #######
###############################

source("C:/Users/e0g411k03dt/Desktop/Sim_nov_2017_paper_2/Simulation VIX GJR/Normalisation_parametrization/VRP_VIX_GJR_NIG.r")

#####################################################
###           plot                                ###
#####################################################
### vix-returns

ts.vol_p_vix_NIG=h_p_vix_NIG(para_h_NIG,Data.returns)
ts.plot(ts.vol_p_vix_NIG, col = "steelblue",xlab="Time",ylab="h variance")
grid()

M= max(ts.vol_p_vix_NIG)
m= min(ts.vol_p_vix_NIG)
#####################################################
###           plot ts.vol  h*  sous q         #######
#####################################################
### lineaire vix-returns
ts.vol_q_vix_NIG=h_q_vix_NIG(para_h_NIG,Data.returns)
ts.plot(ts.vol_q_vix_NIG, col = "steelblue",xlab="Time",ylab="h variance")
grid()

M_q= max(ts.vol_q_vix_NIG)
m_q= min(ts.vol_q_vix_NIG)

Born = c(m,m_q,M,M_q)
Born

M_B= max(Born)
m_B= min(Born)

#####################################################
###                 plot ts.VRP               #######
#####################################################
### lineaire vix-returns
ts.VRP_vix_NIG=VRP_vix_NIG(para_h_NIG,Data.returns)
ts.plot(ts.VRP_vix_NIG, col = "steelblue", main = "IG Garch Model",xlab="2009",ylab="Volatility")
grid()


#####################################################
###        Average Volatility Risk Premium    #######
#####################################################

MVRP = mean(ts.VRP_vix_NIG) 
MVRP
100*MVRP


#####################################################
###              plot VRP                     #######
#####################################################
### lineaire vix-returns
Data.plot_p_vix_NIG<-data.frame(date=Date.ts,ts.vol_p_vix_NIG)
Data.plot_q_vix_NIG<-data.frame(date=Date.ts,ts.vol_q_vix_NIG)
Data.plot_VRP_vix_NIG<-data.frame(date=Date.ts,ts.VRP_vix_NIG)


df_p_vix_NIG <- tryCatch(Data.plot_p_vix_NIG, error = function(e) NA)
df_q_vix_NIG <- tryCatch(Data.plot_q_vix_NIG, error = function(e) NA)
df_VRP_vix_NIG<- tryCatch(Data.plot_VRP_vix_NIG, error = function(e) NA)


plot(as.Date(df_VRP_vix_NIG$date), Data.plot_p_vix_NIG$ts.vol_p_vix_NIG, xlab= "Time", ylab= "(h,h*) values", type='l', col='red', xlim=c(as.Date('1999-10-01'),as.Date('2009-9-30')), ylim=c(m,M)) 
lines(as.Date(Data.plot_VRP_vix_NIG$date), Data.plot_q_vix_NIG$ts.vol_q_vix_NIG,  type='l', col='blue') 
title(main="(h and h*)_vix_NIG", col.main="black", font.main=4)
legend(as.Date('2003-10-01'),0.0848,  legend=c("h_p_Lin_VIX_Ret", "h*_q_Lin_VIX_Ret") ,col=c("red","blue"), lty=1, cex=0.8)
grid()




















































#####################################################
###    Load both data.contract and data.ret   #######
#####################################################
load("DataPrice20112012.Rdata")

Data.N=Data.N2

############################################################
####                        RMSE                          ##
############################################################
start.time <- Sys.time()
RMSE2010=RMSEsim (para_h1,para_distribution1,Data.ret,Data.N)
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken

RMSE2010$rmse


############################################################
####                     RMSE Gaussian                    ##
############################################################
source("C:/Users/e0g411k03dt/Desktop/Estimation P2 Mars 2017/Simulation VIX GJR/Simulation MC  gaussian.r")
source("C:/Users/e0g411k03dt/Desktop/Estimation P2 Mars 2017/Simulation VIX GJR/Function Pricer Gaussian.r")
source("C:/Users/e0g411k03dt/Desktop/Estimation P2 Mars 2017/Simulation VIX GJR/RMSE VIX Gauss.r")

start.time <- Sys.time()
RMSE3=RMSEsimGaus(para_h1,Data.N)
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken

RMSE3$rmse

############################################################
####                Simulation Price                      ##
############################################################    
Sl=Seuil(para_h1,para_distribution1)
Sl

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








