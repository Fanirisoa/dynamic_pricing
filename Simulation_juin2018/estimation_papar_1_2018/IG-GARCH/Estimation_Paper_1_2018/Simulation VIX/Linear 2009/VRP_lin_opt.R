####################################################
######        The volatility Risk Premium         ##
####################################################

#####################################################
###              Load Data source             #######
#####################################################
source("C:/Users/e0g411k03dt/Desktop/Papier 1/Estimation Paper 1,juillet 2016/New data/DataPrice2009.R")

##source("I:/Papier_1_oct_2017/Estimation Paper 1,juillet 2016/New data/DataPrice2009.R")

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


#####################################################
###         Parameters of the model           #######
#####################################################
##   w=para_h[1]; b=para_h[2]; a=para_h[3];  c= para_h[4]; neta=para_h[5] ; nu=para_h[6];
para_h<-c(6.5759e-06 , 1.4019e-03  ,3.0174e+03 , 5.1407e-05, -8.252e-03 , 1.2122e+02)


###########################################################
###  Conditional variance without risk netral Proba    ####
###########################################################

h_p <- function(para_h, Data.returns) {
  ret=Data.returns$ret   
  rt=Data.returns$rt/250        
  Z1=length(rt)
  
  ## set up the parameters of the model : para_h
  w=para_h[1]; b=para_h[2]; a=para_h[3];  c= para_h[4]; neta=para_h[5] ; nu=para_h[6]
  
  h = c()                                                        ####  A vector containing h from the model,
  h[1]=(w + a*(neta^4))/(1 - a*(neta^2) - b - (c*(neta^(-2))))   ####  The first value for h, Unconditional Variance

  for (i in 2:Z1){
    h[i]=w+b*h[i-1]+ c*(neta^(-1))*(ret[i-1]-rt[i-1]-(nu*h[i-1]))+((a*neta*(h[i-1])^2)/(ret[i-1]-rt[i-1]-(nu*h[i-1])))
  }
  
  
  drapeau=0
  
  if (w<=0){drapeau=1}
  if (b<=0){drapeau=1}
  if (a<=0){drapeau=1}
  if (c<=0){drapeau=1}
  if (nu<=0){drapeau=1}
  if (neta>=0){drapeau=1}

  
  if (drapeau==0){
    resultat=h
  }else{
    resultat=rep(NA, Z1)
  }
  
  return(resultat)  
}

##############################################################
######     Conditional variance with risk netral Proba      ##
##############################################################
h_q<-function(para_h,Data.returns){
  rt=Data.returns$rt/250        #### Interest rate Data : Data.BSJ$rt
  ret=Data.returns$ret         #### Returns : Data.BSJ$ret
  Z1=length(ret)
  
  # para_h<-c() set up the parameters of the model 
  w=para_h[1]; b=para_h[2]; a=para_h[3];  c= para_h[4]; neta=para_h[5] ; nu=para_h[6] 
  
  # Variable of risk neutral
  theta0= (1/2)*((neta)^(-1) - (1/((nu^2)*(neta^3)))*(1 + ((nu^2)*(neta^3))/2)^2)
  neta0=neta/(1 - 2*neta*theta0)
  w0=w*(neta0/neta)^(3/2); b0=b; a0=a*(neta0/neta)^(-5/2);  c0=c*(neta0/neta)^(5/2)
  
  # The volatility under the physical probability
  nu0=(1/(neta0^2))*(((1-2*neta0)^(1/2))-1)
  
  h_star = c()                                                                ####  A vector containing h from the model,
  h_star[1]=(w0 + a0*(neta0^4))/(1 - a0*(neta0^2) - b0 - (c0*(neta0^(-2))))   ####  The first value for h,
  for (i in 2:Z1){
    h_star[i]=w0+b0*h_star[i-1]+ c0*(neta0^(-1))*(ret[i-1]-rt[i-1]-(nu0*h_star[i-1]))+((a0*neta0*(h_star[i-1])^2)/(ret[i-1]-rt[i-1]-(nu0*h_star[i-1])))
  }
  
  drapeau=0
  
  if (w<=0){drapeau=1}
  if (b<=0){drapeau=1}
  if (a<=0){drapeau=1}
  if (c<=0){drapeau=1}
  if (nu<=0){drapeau=1}
  if (neta>=0){drapeau=1}

  if (drapeau==0){
    resultat=h_star
  }else{
    resultat=rep(NA, Z1)
  }
  return(resultat)
}



##############################################################
######     Conditional variance with risk netral Proba      ##
##############################################################
VRP<-function(para_h,Data.returns){
  rt=Data.returns$rt/250        #### Interest rate Data : Data.BSJ$rt
  ret=Data.returns$ret         #### Returns : Data.BSJ$ret
  Z1=length(ret)
  
  # para_h<-c() set up the parameters of the model 
  w=para_h[1]; b=para_h[2]; a=para_h[3];  c= para_h[4]; neta=para_h[5] ; nu=para_h[6]  
  
  # Variable of risk neutral
  theta0= (1/2)*((neta)^(-1) - (1/((nu^2)*(neta^3)))*(1 + ((nu^2)*(neta^3))/2)^2)
  neta0=neta/(1 - 2*neta*theta0)
  w0=w*(neta0/neta)^(3/2); b0=b; a0=a*(neta0/neta)^(-5/2);  c0=c*(neta0/neta)^(5/2)
  
  # The volatility under the physical probability
  nu0=(1/(neta0^2))*(((1-2*neta0)^(1/2))-1)
  
  
  h_1=h_p(para_h,Data.returns)
  h_2=h_q(para_h,Data.returns)
  
  
  
  VRP = c()                                                                ####  A vector containing h from the model,
  VRP[1]=  sqrt(h_1[1])-sqrt(h_2[1])                ####  The first value for VRP,
  for (i in 2:Z1){
    VRP[i]=  sqrt(h_1[i])-sqrt(h_2[i]) 
  }
  
  drapeau=0
  
  if (w<=0){drapeau=1}
  if (b<=0){drapeau=1}
  if (a<=0){drapeau=1}
  if (c<=0){drapeau=1}
  if (nu<=0){drapeau=1}
  if (neta>=0){drapeau=1}
  if (drapeau==0){
    resultat=-VRP
  }else{
    resultat=rep(NA, Z1)
  }
  return(resultat)
}

#####################################################
###              plot h* and h                #######
#####################################################
### h
ts.vol_p=h_p(para_h,Data.returns)
ts.plot(ts.vol_p, col = "steelblue", main = "IG Garch Model",xlab="2009",ylab="Volatility")
grid()

### h*
ts.vol_q=h_q(para_h,Data.returns)
ts.plot(ts.vol_q, col = "steelblue", main = "IG Garch Model",xlab="2009",ylab="Volatility")
grid()

### both
set.seed(1)
x = ts(h_p(para_h,Data.returns))
y = ts(h_q(para_h,Data.returns))
ts.plot(x, y, gpars = list(col = c("steelblue", "red")))
grid()

### VRP
ts.VRP=VRP(para_h,Data.returns)
ts.plot(ts.VRP, col = "steelblue", main = "IG Garch Model",xlab="2009",ylab="Volatility")
grid()


#####################################################
###              plot h* , h avd VRP          #######
#####################################################
Date.ts=Data.returns$date
Date.ts[1:5]

Data.plot_p<-data.frame(date=Date.ts,ts.vol_p)
Data.plot_q<-data.frame(date=Date.ts,ts.vol_q)
Data.plot_VRP<-data.frame(date=Date.ts,ts.VRP)


df_p <- tryCatch(Data.plot_p, error = function(e) NA)
df_q <- tryCatch(Data.plot_q, error = function(e) NA)
df_VRP <- tryCatch(Data.plot_VRP, error = function(e) NA)


plot(as.Date(df_q$date), Data.plot_q$ts.vol_q, xlab= "Periodes 1000/2009", ylab= "Variance (h* and h)", type='l', col='blue',xlim=c(as.Date('2000-01-01'),as.Date('2009-12-01'))) 
lines(as.Date(df_p$date), Data.plot_p$ts.vol_p, xlab= "Years", ylab= "sss", type='l', col='red') 
grid()


plot(as.Date(df_VRP$date), Data.plot_VRP$ts.VRP, xlab= "Periodes 1999/2009", ylab= "Variance (h* and h)", type='l', col='red', xlim=c(as.Date('1999-10-01'),as.Date('2009-12-31')), ylim=c(0,0.002)) 
lines(as.Date(df_q$date), Data.plot_q$ts.vol_q,  type='l', col='blue') 
lines(as.Date(df_p$date), Data.plot_p$ts.vol_p,  type='l', col='green') 
title(main="Volatility Risk Premium", col.main="black", font.main=4)
grid()

