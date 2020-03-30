#####################################################
###              Load Data source             #######
#####################################################

getwd()

source("C:/Users/e0g411k03dt/Desktop/Sim_nov_2017_paper_2/New data/Data20092010.R") 
source("C:/Users/e0g411k03dt/Desktop/Sim_nov_2017_paper_2/New data/Data20112012.R") 


source("I:/Mai MSE 2017/Estimation P2 Mars 2017/New data/Data20092010.R")
source("I:/Mai MSE 2017/Estimation P2 Mars 2017/New data/Data20112012.R")

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


source("C:/Users/e0g411k03dt/Desktop/Sim_nov_2017_paper_2/Simulation VIX HN/Normalisation_parametrization/Loglik Ret VIX HN.r")
source("C:/Users/e0g411k03dt/Desktop/Sim_nov_2017_paper_2/Simulation VIX HN/Normalisation_parametrization/Loglik Opt VIX HN.r")
source("C:/Users/e0g411k03dt/Desktop/Sim_nov_2017_paper_2/Simulation VIX HN/Normalisation_parametrization/Loglik Mix VIX HN.r")



source("I:/Sim_nov_2017_paper_2/Simulation VIX HN/Normalisation_parametrization/Loglik Ret VIX HN.r")
source("I:/Sim_nov_2017_paper_2/Simulation VIX HN/Normalisation_parametrization/Loglik Opt VIX HN.r")
source("I:/Sim_nov_2017_paper_2/Simulation VIX HN/Normalisation_parametrization/Loglik Mix VIX HN.r")


#####################################################
###         Parameters of the model           #######
#####################################################
###   Initial parameter  para_h<-c() set up the parameters of the model   ####
### a0=para_h[1]; a1=para_h[2]; gama=para_h[3];  b1= para_h[4] ;  lamda0= para_h[5] ; ro=para_h[6]


para_h<-c(5.282379e-08, 2.252557e-04 ,8.143868e+00 ,9.154310e-01 ,0.7426485e-00, 9.784247e-01) ##  RMSE2$rmse :0.05127656 RMSE3$rmse : 0.01818576



para_h<-c(3.285e-05, 3.602e-04, 9.253e+00, 2.510e-01, 1:353e-06, 0.9720)  ## RMSE2$rmse : 0.0154167 ## RMSE3$rmse : 0.01615634




para_h<-c(4.27e-07, 1.51e-06, 462.6, 0.662,0.64, 9.646950e-01) ## RMSE2$rmse :   ## RMSE3$rmse :



para_h<-c(3.313135e-15, 1.366366e-06, 4.274284e+02, 7.324341e-01, 8.531595e+00, 9.95507e-01)  ## RMSE2$rmse : 0.0154167 ## RMSE3$rmse : 0.01615634
para_h<-c(3.788371e-13, 1.520721e-06, 4.649298e+02, 6.619980e-01, 4.400007e-01, 9.646967e-01)  ## RMSE2$rmse : 0.01082776 ## RMSE3$rmse : 0.03113633
###   Solution   para_h<-c() set up the parameters of the model   ####
para_h<-c(3.788371e-13, 1.520721e-06, 4.649298e+02, 6.619980e-01, 6.400007e-01, 9.646967e-01)  ## RMSE2$rmse : 0.0154167 ## RMSE3$rmse : 0.03113633
para_h<-c(3.788371e-13, 1.520721e-06, 4.649298e+02, 6.619980e-01, 6.400007e-01, 9.646967e-01)  ## RMSE2$rmse : 0.0154167 ## RMSE3$rmse : 0.03113633
para_h<-c(1.905978e-08, 1.522443e-06, 4.642720e+02, 6.620030e-01, 6.399963e-01, 9.646983e-01) ## 0.01083887


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
###              LOg values                   #######
#####################################################
start.time <- Sys.time()
ILK=Heston_likelihood_RET_solo(para_h,Data.returns)
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken
ILK


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

source("C:/Users/e0g411k03dt/Desktop/Sim_nov_2017_paper_2/Simulation VIX HN/Normalisation_parametrization/QMLNIG_VIX_HN.r")


source("I:/Sim_nov_2017_paper_2/Simulation VIX HN/Normalisation_parametrization/QMLNIG_VIX_HN.r")


#####################################################
###         Parameters of the model           #######
#####################################################
###   Initial parameter  para_h<-c() set up the parameters of the model   ####
### alpha=para_distribution[1];  beta=para_distribution[2];  delta=para_distribution[3];  mu=para_distribution[4];

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

source("C:/Users/e0g411k03dt/Desktop/Sim_nov_2017_paper_2/Simulation VIX HN/Normalisation_parametrization/MCSim_VIX HN_Normalised.r")
source("C:/Users/e0g411k03dt/Desktop/Sim_nov_2017_paper_2/Simulation VIX HN/Normalisation_parametrization/Fun_Pricer_VIX_HN.r")
source("C:/Users/e0g411k03dt/Desktop/Sim_nov_2017_paper_2/Simulation VIX HN/Normalisation_parametrization/RMSE_VIX_HN.r")

N= 2

source("I:/Sim_nov_2017_paper_2/Simulation VIX HN/Normalisation_parametrization/MCSim_VIX HN_Normalised.r")
source("I:/Sim_nov_2017_paper_2/Simulation VIX HN/Normalisation_parametrization/Fun_Pricer_VIX_HN.r")
source("I:/Sim_nov_2017_paper_2/Simulation VIX HN/Normalisation_parametrization/RMSE_VIX_HN.r")


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
###          Part 4 VRP      Simulation       #######
#####################################################
#####################################################
###    Load both data.contract and data.ret   #######
#####################################################
Date.ts=Data.returns$date
###############################
###    Load fonctions   #######
###############################

source("C:/Users/e0g411k03dt/Desktop/Sim_nov_2017_paper_2/Simulation VIX HN/Normalisation_parametrization/VRP_VIX_HN_NIG_normalise.R")


source("I:/Sim_nov_2017_paper_2/Simulation VIX HN/Normalisation_parametrization/VRP_VIX_HN_NIG_normalise.r")

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
### vix-returns
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
ts.VRP_vix_NIG=VRP_vix_NIG(para_h_NIG,Data.returns)+0.005
ts.plot(ts.VRP_vix_NIG , col = "steelblue", main = "IG Garch Model",xlab="2009",ylab="Volatility")
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


plot(as.Date(df_VRP_vix_NIG$date), Data.plot_p_vix_NIG$ts.vol_p_vix_NIG, xlab= "Time", ylab= "(h,h*) values", type='l', col='red', xlim=c(as.Date('1999-10-01'),as.Date('2009-9-30')), ylim=c(m_B,M_B)) 
lines(as.Date(Data.plot_VRP_vix_NIG$date), Data.plot_q_vix_NIG$ts.vol_q_vix_NIG,  type='l', col='blue') 
title(main="(h and h*)_vix_NIG", col.main="black", font.main=4)
legend(as.Date('2002-05-17'),0.0105,  legend=c("h", "h*") ,col=c("red","blue"), lty=1, cex=0.8)
grid()


plot(as.Date(df_VRP_vix_NIG$date), Data.plot_VRP_vix_NIG$ts.VRP_vix_NIG, xlab= "Time", ylab= "(h,h*) values", type='l', col='blue', xlim=c(as.Date('1999-10-01'),as.Date('2009-9-30')), ylim=c(m_V,M_V)) 
lines(as.Date(Data.plot_VRP_vix_NIG$date), Data.plot_q_vix_NIG$ts.vol_q_vix_NIG,  type='l', col='blue') 
title(main="(h and h*)_vix_NIG", col.main="black", font.main=4)
legend(as.Date('2007-05-17'),-0.0385,  legend=c("VRP") ,col=c( "blue"), lty=1, cex=0.8)
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


plot(as.Date(df_VRP_vix_Gaus$date), Data.plot_p_vix_NIG$ts.vol_p_vix_NIG, xlab= "Time", ylab= "(h,h*) values", type='l', col='red', xlim=c(as.Date('1999-10-01'),as.Date('2009-9-30')), ylim=c(0,0.005)) 
lines(as.Date(Data.plot_VRP_vix_Gaus$date), Data.plot_q_vix_Gaus$ts.vol_q_vix_NIG,  type='l', col='blue') 
title(main="(h and h*)_vix_NIG", col.main="black", font.main=4)
legend(as.Date('2003-10-01'),0.00448,  legend=c("h", "h*") ,col=c("red","blue"), lty=1, cex=0.8)
grid()


plot(as.Date(df_VRP_vix_NIG$date), Data.plot_VRP_vix_Gaus$ts.VRP_vix_Gaus, xlab= "Time", ylab= "(h,h*) values", type='l', col='blue', xlim=c(as.Date('1999-10-01'),as.Date('2009-9-30')), ylim=c(m_V_G,M_V_G)) 
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
