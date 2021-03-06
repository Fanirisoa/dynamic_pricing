####################################################
######        The volatility Risk Premium         ##
####################################################

#####################################################
###              Load Data source             #######
#####################################################
##source("C:/Users/e0g411k03dt/Desktop/Simulation P3 juin2017/Data/Data20092010.R")


source("I:/Mai MSE 2017/Estimation P2 Mars 2017/New data/Data20092010.R")


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


Date.ts=Data.returns$date

###############################
###    Load fonctions   #######
###############################
source("F:/Mai MSE 2017/Estimation P2 Mars 2017/VRP/GAUSSIAN/VRP_VIX_HN/VRP_VIX_HN_Gauss.R")

source("I:/Sim_nov_2017_paper_2/VRP/GAUSSIAN/VRP_VIX_HN/VRP_VIX_HN_Gauss.R")


#####################################################
###           plot                                ###
#####################################################
### lineaire vix-returns
ts.vol_p_vix_gauss=h_p_vix_gauss(para_h_Gauss,Data.returns)
ts.plot(ts.vol_p_vix_gauss, col = "steelblue",xlab="Time",ylab="h variance")
grid()

M= max(ts.vol_p_vix_gauss)
m= min(ts.vol_p_vix_gauss)
#####################################################
###           plot ts.vol  h*  sous q         #######
#####################################################
### lineaire vix-returns
ts.vol_q_vix_gauss=h_q_vix_gauss(para_h_Gauss,Data.returns)
ts.plot(ts.vol_q_vix_gauss, col = "steelblue",xlab="Time",ylab="h variance")
grid()


#####################################################
###                 plot ts.VRP               #######
#####################################################
### lineaire vix-returns
ts.VRP_vix_gauss=VRP_vix_gauss(para_h_Gauss,Data.returns)
ts.plot(ts.VRP_vix_gauss, col = "steelblue", main = "IG Garch Model",xlab="2009",ylab="Volatility")
grid()


#####################################################
###        Average Volatility Risk Premium    #######
#####################################################

MVRP = mean(ts.VRP_vix_gauss) 
MVRP
100*MVRP


#####################################################
###              plot VRP                     #######
#####################################################
### lineaire vix-returns
Data.plot_p_vix_gauss<-data.frame(date=Date.ts,ts.vol_p_vix_gauss)
Data.plot_q_vix_gauss<-data.frame(date=Date.ts,ts.vol_q_vix_gauss)
Data.plot_VRP_vix_gauss<-data.frame(date=Date.ts,ts.VRP_vix_gauss)


df_p_vix_gauss <- tryCatch(Data.plot_p_vix_gauss, error = function(e) NA)
df_q_vix_gauss <- tryCatch(Data.plot_q_vix_gauss, error = function(e) NA)
df_VRP_vix_gauss<- tryCatch(Data.plot_VRP_vix_gauss, error = function(e) NA)


plot(as.Date(df_VRP_vix_gauss$date), Data.plot_p_vix_gauss$ts.vol_p_vix_gauss, xlab= "Time", ylab= "(h,h*) values", type='l', col='red', xlim=c(as.Date('1999-10-01'),as.Date('2009-9-30')), ylim=c(m,M)) 
lines(as.Date(Data.plot_VRP_vix_gauss$date), Data.plot_q_vix_gauss$ts.vol_q_vix_gauss,  type='l', col='blue') 
title(main="(h and h*)_vix_gauss", col.main="black", font.main=4)
legend(as.Date('2003-10-01'),0.0848,  legend=c("h_p_Lin_VIX_Ret", "h*_q_Lin_VIX_Ret") ,col=c("red","blue"), lty=1, cex=0.8)
grid()

