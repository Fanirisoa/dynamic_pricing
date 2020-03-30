#####################################################
###              Load Data source             #######
#####################################################

getwd()

source("C:/Users/e0g411k03dt/Desktop/Sim_nov_2017_paper_2/New data/Data20092010.R") 
source("C:/Users/e0g411k03dt/Desktop/Sim_nov_2017_paper_2/New data/Data20112012.R") 


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
source("C:/Users/e0g411k03dt/Desktop/Sim_nov_2017_paper_2/Simulation VIX GJR/Badescu_parametrization/Loglik_Ret_VIX_GJR.r")
source("C:/Users/e0g411k03dt/Desktop/Sim_nov_2017_paper_2/Simulation VIX GJR/Badescu_parametrization/Loglik_Opt_VIX_GJR.r")
source("C:/Users/e0g411k03dt/Desktop/Sim_nov_2017_paper_2/Simulation VIX GJR/Badescu_parametrization/Loglik_Mix_VIX_GJR.r")

#####################################################
###         Parameters of the model           #######
#####################################################
###   Initial parameter  para_h<-c() set up the parameters of the model   ####

### a0=para_h[1]; a1=para_h[2]; a2=para_h[3];  b1= para_h[4] ;  lamda0= para_h[5] ; ro=para_h[6]   
#para_h<-c(5.192896e-06, 1.240918e-01, 2.314273e-02, 8.504267e-01, 1.821112e-01, 9.657773e-01)

para_h<-c(5.987174e-06,  1.240911e-01,  2.314265e-02,  8.504269e-01,  3.784983e-02, 9.546611e-01)  ## RMSE1$rmse : 0.06265758 RMSE3$rmse :0.07367674


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
Sol=optim(para_h,Heston_likelihood_MixViX ,Data.returns=Data.returns, method="Nelder-Mead",control = list(maxit = 10000))
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken
Sol

para_h1<-Sol$par


#####################################################
###      Optimization  of  only return        #######
#####################################################

start.time <- Sys.time()
Sol=optim(para_h,Heston_likelihood_RET_solo ,Data.returns=Data.returns, method="Nelder-Mead",control = list(maxit = 10000))
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
source("C:/Users/e0g411k03dt/Desktop/Sim_nov_2017_paper_2/Simulation VIX GJR/Badescu_parametrization/QMLNIG_VIX_GJR_badescu.r")

#####################################################
###         Parameters of the model           #######
#####################################################
###   Initial parameter  para_h<-c() set up the parameters of the model   ####
### alpha=para_distribution[1];  beta=para_distribution[2];  delta=para_distribution[3];  mu=para_distribution[4];
para_distribution<-c(5, 1.397610234)


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
source("C:/Users/e0g411k03dt/Desktop/Sim_nov_2017_paper_2/Simulation VIX GJR/Badescu_parametrization/MCSim_VIX_GJR.r")
source("C:/Users/e0g411k03dt/Desktop/Sim_nov_2017_paper_2/Simulation VIX GJR/Badescu_parametrization/Fun_Pricer_VIX_GJR.r")
source("C:/Users/e0g411k03dt/Desktop/Sim_nov_2017_paper_2/Simulation VIX GJR/Badescu_parametrization/RMSE_VIX_GJR.r")



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

source("C:/Users/e0g411k03dt/Desktop/Sim_nov_2017_paper_2/Simulation VIX GJR/Badescu_parametrization/VRP_VIX_GJR_NIG.r")

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
ts.VRP_vix_NIG=VRP_vix_NIG(para_h_NIG,Data.returns)+0.002
ts.plot(ts.VRP_vix_NIG, col = "steelblue", main = "IG Garch Model",xlab="2009",ylab="Volatility")
grid()

M_V= max(ts.VRP_vix_NIG)
m_V= min(ts.VRP_vix_NIG)


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


plot(as.Date(df_VRP_vix_NIG$date), Data.plot_p_vix_NIG$ts.vol_p_vix_NIG, xlab= "Time", ylab= "(h,h*) values", type='l', col='red', xlim=c(as.Date('2000-6-01'),as.Date('2009-11-30')), ylim=c(m_B,M_B)) 
lines(as.Date(Data.plot_VRP_vix_NIG$date), Data.plot_q_vix_NIG$ts.vol_q_vix_NIG,  type='l', col='blue') 
title(main="(h and h*)_vix_NIG", col.main="black", font.main=4)
legend(as.Date('2003-10-01'),0.017,  legend=c("h", "h*") ,col=c("red","blue"), lty=1, cex=0.8)
grid()



plot(as.Date(df_VRP_vix_NIG$date), Data.plot_VRP_vix_NIG$ts.VRP_vix_NIG, xlab= "Time", ylab= "(h,h*) values", type='l', col='blue', xlim=c(as.Date('2000-6-01'),as.Date('2009-11-30')), ylim=c(m_V,-0.025)) 
lines(as.Date(Data.plot_VRP_vix_NIG$date), Data.plot_q_vix_NIG$ts.vol_q_vix_NIG,  type='l', col='blue') 
title(main="(h and h*)_vix_NIG", col.main="black", font.main=4)
legend(as.Date('2004-05-17'),-0.051,  legend=c("VRP") ,col=c( "blue"), lty=1, cex=0.8)
grid()










#####################################################
###     lot ts.vol  h*  sous q   Gaussian     #######
#####################################################
### vix-returns
ts.vol_q_vix_Gaus=h_q_vix_Gaus(para_h_NIG,Data.returns)
ts.plot(ts.vol_q_vix_Gaus, col = "steelblue",xlab="Time",ylab="h variance")
grid()

#####################################################
###                 plot ts.VRP               #######
#####################################################
### lineaire vix-returns
ts.VRP_vix_Gaus=VRP_vix_Gaus(para_h_NIG,Data.returns)
ts.plot(ts.VRP_vix_Gaus, col = "steelblue", main = "IG Garch Model",xlab="2009",ylab="Volatility",xlim=c(0,100))
grid()

M_V_G= max(ts.VRP_vix_Gaus)
m_V_G= min(ts.VRP_vix_Gaus)
#####################################################
###        Average Volatility Risk Premium    #######
#####################################################

MVRP = mean(ts.VRP_vix_Gaus) 
MVRP
100*MVRP


#####################################################
###              plot VRP                     #######
#####################################################
### lineaire vix-returns
Data.plot_p_vix_NIG<-data.frame(date=Date.ts,ts.vol_p_vix_NIG)
Data.plot_q_vix_Gaus<-data.frame(date=Date.ts,ts.vol_q_vix_Gaus)
Data.plot_VRP_vix_Gaus<-data.frame(date=Date.ts,ts.VRP_vix_Gaus)


df_p_vix_NIG <- tryCatch(Data.plot_p_vix_NIG, error = function(e) NA)
df_q_vix_Gaus <- tryCatch(Data.plot_q_vix_Gaus, error = function(e) NA)
df_VRP_vix_Gaus<- tryCatch(Data.plot_VRP_vix_Gaus, error = function(e) NA)


plot(as.Date(df_VRP_vix_Gaus$date), Data.plot_p_vix_NIG$ts.vol_p_vix_NIG, xlab= "Time", ylab= "(h,h*) values", type='l', col='red', xlim=c(as.Date('2000-6-01'),as.Date('2009-11-30')), ylim=c(0.001,0.00650)) 
lines(as.Date(Data.plot_VRP_vix_Gaus$date), Data.plot_q_vix_Gaus$ts.vol_q_vix_NIG,  type='l', col='blue') 
title(main="(h and h*)_vix_NIG", col.main="black", font.main=4)
legend(as.Date('2003-10-01'),0.00548,  legend=c("h", "h*") ,col=c("red","blue"), lty=1, cex=0.8)
grid()


plot(as.Date(df_VRP_vix_NIG$date), Data.plot_VRP_vix_Gaus$ts.VRP_vix_Gaus, xlab= "Time", ylab= "(h,h*) values", type='l', col='blue', xlim=c(as.Date('2000-1-01'),as.Date('2009-11-30')), ylim=c(m_V_G,M_V_G)) 
lines(as.Date(Data.plot_VRP_vix_NIG$date), Data.plot_q_vix_NIG$ts.vol_q_vix_NIG,  type='l', col='blue') 
title(main="(h and h*)_vix_NIG", col.main="black", font.main=4)
legend(as.Date('2007-05-17'), 0.000285,  legend=c("VRP") ,col=c( "blue"), lty=1, cex=0.8)
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







