####################################################
######        The volatility Risk Premium         ##
####################################################

#####################################################
###              Load Data source             #######
#####################################################
##source("C:/Users/e0g411k03dt/Desktop/Papier 1/Estimation Paper 1,juillet 2016/New data/DataPrice2009.R")

source("I:/Papier_1_oct_2017/Estimation Paper 1,juillet 2016/New data/DataPrice2009.R")


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
Returns=Data.returns$ret


#####################################################
###         plot daily log-returns            #######
#####################################################

Data.plot_returns<-data.frame(date=Date.ts,Returns)


plot_returns <- tryCatch(Data.plot_returns, error = function(e) NA)

plot(as.Date(plot_returns$date), plot_returns$Returns, xlab= "Time", ylab= "Returns (Yt)", type='l', col='red', xlim=c(as.Date('1999-5-01'),as.Date('2009-9-1')), ylim=c(-0.08,0.08)) 
title(main="S&P500 daily log-returns", col.main="black", font.main=4)
grid()



#####################################################
###             plot volatility               #######
#####################################################
Vol_cal <- function(i,n, Data.returns) {
    ret =Data.returns$ret 
    
    X=c()
    
    for (j in 1:n){
      X[j] = ret[i-j]
    }
    m=mean(X)
    
    Y=c()
    
    for (k in 1:n){
      Y[k]=(X[k]- m)^2
    }
    vol=sqrt((1/(n-1))*(sum(Y))) 
    
    return(vol)  
}
  
Vol_Hist_an <- function(n, Data.returns) {
  ret =Data.returns$ret   
  Z1=length(ret)
  
  vol=c()
  for (i in 1:Z1){
   
    if(i > n){
      vol[i] = Vol_cal(i,n, Data.returns)
    } else {
      vol[i] = 0
    }
  }
  
  return(vol*sqrt(252)) 
}
  

Vol=Vol_Hist_an(30, Data.returns)


Data.plot_vol<-data.frame(date=Date.ts,Vol)
plot_Vol <- tryCatch(Data.plot_vol, error = function(e) NA)

plot(as.Date(plot_Vol$date), plot_Vol$Vol, xlab= "Time", ylab= "Volatility", type='l', col='red', xlim=c(as.Date('1999-9-01'),as.Date('2009-10-1'))) 
title(main="Annualised Historical Volatility", col.main="black", font.main=4)
grid()
