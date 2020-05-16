####################################################
######        The volatility Risk Premium         ##
####################################################

#####################################################
###              Load Data source             #######
#####################################################
##source("C:/Users/e0g411k03dt/Desktop/Papier 1/Estimation Paper 1,juillet 2016/New data/DataPrice2009.R")
setwd("C:/Users/fanir/Desktop/Simulation_juin2018/Data")  

source("C:/Users/fanir/Desktop/Simulation_juin2018/Data/DataPrice2009.R")


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
load("DataPrice2009.Rdata")



Date.ts=Data.returns$date

###############################
###    Load fonctions   #######
###############################
source("C:/Users/fanir/Desktop/Simulation_juin2018/IG-GARCH/Estimation_Paper_1_2018/VRP_lin.R")
source("C:/Users/fanir/Desktop/Simulation_juin2018/IG-GARCH/Estimation_Paper_1_2018/VRP_lin_opt.R")
source("C:/Users/fanir/Desktop/Simulation_juin2018/IG-GARCH/Estimation_Paper_1_2018/VRP_qua.R")
source("C:/Users/fanir/Desktop/Simulation_juin2018/IG-GARCH/Estimation_Paper_1_2018/VRP_qua_opt.R")

############################################# 
###                 VRP               #######
############################################# 
Data.ret= Data.returns

### lineaire vix-returns
VRP_lin=VRP_lin(para_h_lin,Data.returns)

### lineaire option-returns
VRP_lin_opt=VRP_lin_opt(para_h_lin_opt,Data.returns)

### quadratique vix-returns
VRP_qua=VRP_qua(para_h_qua,Data.returns)

### quadratique option-returns
VRP_qua_opt=VRP_qua_opt(para_h_qua_opt,Data.returns)

#####################################################
###        Average Volatility Risk Premium    #######
#####################################################
MVRP = c(mean(VRP_lin),mean(VRP_lin_opt),mean(VRP_qua),mean(VRP_qua_opt))
MVRP
100*MVRP

#####################################################
###           plot    
#####################################################
### lineaire vix-returns
ts.vol_p_lin=h_p_lin(para_h_lin,Data.returns)
ts.plot(ts.vol_p_lin, col = "steelblue",xlab="Time",ylab="h variance")
grid()

### lineaire option-returns
ts.vol_p_lin_opt=h_p_lin_opt(para_h_lin_opt,Data.returns)
ts.plot(ts.vol_p_lin_opt, col = "steelblue",xlab="Time",ylab="h variance")
grid()

### quadratique vix-returns
ts.vol_p_qua=h_p_qua(para_h_qua,Data.returns)
ts.plot(ts.vol_p_qua, col = "steelblue",xlab="Time",ylab="h variance")
grid()

### quadratique option-returns
ts.vol_p_qua_opt=h_p_qua_opt(para_h_qua_opt,Data.returns)
ts.plot(ts.vol_p_qua_opt, col = "steelblue",xlab="Time",ylab="h variance")
grid()


#####################################################
###           plot ts.vol  h*  sous q         #######
#####################################################

### lineaire vix-returns
ts.vol_q_lin=h_q_lin(para_h_lin,Data.returns)
ts.plot(ts.vol_q_lin, col = "steelblue",xlab="Time",ylab="h variance")
grid()

### lineaire option-returns
ts.vol_q_lin_opt=h_q_lin_opt(para_h_lin_opt,Data.returns)
ts.plot(ts.vol_q_lin_opt, col = "steelblue",xlab="Time",ylab="h variance")
grid()

### quadratique vix-returns
ts.vol_q_qua=h_q_qua(para_h_qua,Data.returns)
ts.plot(ts.vol_q_qua, col = "steelblue",xlab="Time",ylab="h variance")
grid()

### quadratique option-returns
ts.vol_q_qua_opt=h_q_qua_opt(para_h_qua_opt,Data.returns)
ts.plot(ts.vol_q_qua_opt, col = "steelblue",xlab="Time",ylab="h variance")
grid()


#####################################################
###                 plot ts.VRP               #######
#####################################################

### lineaire vix-returns
ts.VRP_lin=VRP_lin(para_h_lin,Data.returns)
ts.plot(ts.VRP_lin, col = "steelblue", main = "IG Garch Model",xlab="2009",ylab="Volatility")
grid()


### lineaire option-returns
ts.VRP_lin_opt=VRP_lin_opt(para_h_lin_opt,Data.returns)
ts.plot(ts.VRP_lin_opt, col = "steelblue", main = "IG Garch Model",xlab="2009",ylab="Volatility")
grid()

### quadratique vix-returns
ts.VRP_qua=VRP_qua(para_h_qua,Data.returns)
ts.plot(ts.VRP_qua, col = "steelblue", main = "IG Garch Model",xlab="2009",ylab="Volatility")
grid()

### quadratique option-returns
ts.VRP_qua_opt=VRP_qua_opt(para_h_qua_opt,Data.returns)
ts.plot(ts.VRP_qua_opt, col = "steelblue", main = "IG Garch Model",xlab="2009",ylab="Volatility")
grid()

#####################################################
###              plot VRP                     #######
#####################################################

### lineaire vix-returns
Data.plot_p_lin<-data.frame(date=Date.ts,ts.vol_p_lin)
Data.plot_q_lin<-data.frame(date=Date.ts,ts.vol_q_lin)
Data.plot_VRP_lin<-data.frame(date=Date.ts,ts.VRP_lin)


df_p_lin <- tryCatch(Data.plot_p_lin, error = function(e) NA)
df_q_lin <- tryCatch(Data.plot_q_lin, error = function(e) NA)
df_VRP_lin<- tryCatch(Data.plot_VRP_lin, error = function(e) NA)



### lineaire optin-returns
Data.plot_p_lin_opt<-data.frame(date=Date.ts,ts.vol_p_lin_opt)
Data.plot_q_lin_opt<-data.frame(date=Date.ts,ts.vol_q_lin_opt)
Data.plot_VRP_lin_opt<-data.frame(date=Date.ts,ts.VRP_lin_opt)


df_p_lin_opt <- tryCatch(Data.plot_p_lin_opt, error = function(e) NA)
df_q_lin_opt <- tryCatch(Data.plot_q_lin_opt, error = function(e) NA)
df_VRP_lin_opt<- tryCatch(Data.plot_VRP_lin_opt, error = function(e) NA)



### Quadrarique vix-returns
Data.plot_p_qua<-data.frame(date=Date.ts,ts.vol_p_qua)
Data.plot_q_qua<-data.frame(date=Date.ts,ts.vol_q_qua)
Data.plot_VRP_qua<-data.frame(date=Date.ts,ts.VRP_qua)


df_p_qua <- tryCatch(Data.plot_p_qua, error = function(e) NA)
df_q_qua <- tryCatch(Data.plot_q_qua, error = function(e) NA)
df_VRP_qua<- tryCatch(Data.plot_VRP_qua, error = function(e) NA)



### Quadrarique optin-returns
Data.plot_p_qua_opt<-data.frame(date=Date.ts,ts.vol_p_qua_opt)
Data.plot_q_qua_opt<-data.frame(date=Date.ts,ts.vol_q_qua_opt)
Data.plot_VRP_qua_opt<-data.frame(date=Date.ts,ts.VRP_qua_opt)


df_p_qua_opt <- tryCatch(Data.plot_p_qua_opt, error = function(e) NA)
df_q_qua_opt <- tryCatch(Data.plot_q_qua_opt, error = function(e) NA)
df_VRP_qua_opt<- tryCatch(Data.plot_VRP_qua_opt, error = function(e) NA)




plot(as.Date(df_VRP_lin$date), Data.plot_p_lin$ts.vol_p_lin, xlab= "Time", ylab= "(h,h*) values", type='l', col='red', xlim=c(as.Date('1999-10-01'),as.Date('2009-9-30')), ylim=c(0.065,0.085)) 
lines(as.Date(Data.plot_VRP_lin_opt$date), Data.plot_q_lin$ts.vol_q_lin,  type='l', col='blue') 
title(main="(h and h*)_Lin_VIX_Ret", col.main="black", font.main=4)
legend(as.Date('2003-10-01'),0.0848,  legend=c("h_p_Lin_VIX_Ret", "h*_q_Lin_VIX_Ret") ,col=c("red","blue"), lty=1, cex=0.8)
grid()


plot(as.Date(df_VRP_lin$date), Data.plot_p_lin_opt$ts.vol_p_lin_opt, xlab= "Time", ylab= "(h,h*) values", type='l', col='red', xlim=c(as.Date('1999-10-01'),as.Date('2009-9-30')), ylim=c(0.0575,0.0775)) 
lines(as.Date(Data.plot_VRP_lin_opt$date), Data.plot_q_lin_opt$ts.vol_q_lin_opt,  type='l', col='blue') 
title(main="(h and h*)_Lin_opt_Ret", col.main="black", font.main=4)
legend(as.Date('2003-10-01'),0.077,  legend=c("h_p_Lin_opt_Ret", "h*_q_Lin_opt_Ret") ,col=c("red","blue"), lty=1, cex=0.8)
grid()


plot(as.Date(df_VRP_lin$date), Data.plot_p_qua$ts.vol_p_qua, xlab= "Time", ylab= "(h,h*) values", type='l', col='red', xlim=c(as.Date('2000-06-01'),as.Date('2009-6-30')), ylim=c(0.08,0.117)) 
lines(as.Date(Data.plot_VRP_lin_opt$date), Data.plot_q_qua$ts.vol_q_qua,  type='l', col='blue') 
title(main="(h and h*)_qua_VIX_Ret", col.main="black", font.main=4)
legend(as.Date('2003-10-01'),0.1,  legend=c("h_p_qua_VIX_Ret", "h*_q_qua_VIX_Ret") ,col=c("red","blue"), lty=1, cex=0.8)
grid()




plot(as.Date(df_VRP_lin$date), Data.plot_p_qua_opt$ts.vol_p_qua_opt, xlab= "Time", ylab= "(h,h*) values", type='l', col='red', xlim=c(as.Date('2000-06-01'),as.Date('2009-6-30')), ylim=c(0.046,0.069)) 
lines(as.Date(Data.plot_VRP_lin_opt$date), Data.plot_q_qua_opt$ts.vol_q_qua_opt,  type='l', col='blue') 
title(main="(h and h*)_qua_opt_Ret", col.main="black", font.main=4)
legend(as.Date('2003-10-01'),0.069,  legend=c("h_p_qua_opt_Ret", "h*_q_qua_opt_Ret") ,col=c("red","blue"), lty=1, cex=0.8)
grid()


### VPR_All

plot(as.Date(df_VRP_lin$date), Data.plot_VRP_lin$ts.VRP_lin, xlab= "Time", ylab= "VRP values", type='l', col='red', xlim=c(as.Date('2000-6-01'),as.Date('2009-6-30')), ylim=c(0.001,0.045)) 
lines(as.Date(Data.plot_VRP_lin_opt$date), Data.plot_VRP_lin_opt$ts.VRP_lin_opt,  type='l', col='blue') 
lines(as.Date(Data.plot_VRP_qua$date), Data.plot_VRP_qua$ts.VRP_qua,  type='l', col='green') 
lines(as.Date(Data.plot_VRP_qua_opt$date), Data.plot_VRP_qua_opt$ts.VRP_qua_opt,  type='l', col='black') 
title(main="Volatility Risk Premium", col.main="black", font.main=4)
legend(as.Date('2004-01-01'),0.022,  legend=c("Lin_VIX_Ret", "Lin_Option_Ret","qua_VIX_Ret", "qua_Option_Ret") ,col=c("red","blue","green","black"), lty=1, cex=0.7)
grid()


#####################################################
###        Average Volatility Risk Premium    #######
#####################################################


MVRP = c(mean(ts.VRP_lin),mean(ts.VRP_lin_opt),mean(ts.VRP_qua),mean(ts.VRP_qua_opt))
MVRP
100*MVRP

