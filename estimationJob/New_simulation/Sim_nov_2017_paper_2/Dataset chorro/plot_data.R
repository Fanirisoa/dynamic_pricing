#####################################################
###              Load Data source             #######
#####################################################

#####################################################
###             Clean the repertoir           #######
#####################################################
rm(list=ls())
gc()

getwd()

setwd("G:/Historique/Sim_nov_2017_paper_2/New data")

#####################################################
###              Load Data source             #######
#####################################################
##source("C:/Users/e0g411k03dt/Desktop/Papier 1/Estimation Paper 1,juillet 2016/New data/DataPrice2009.R")


source("G:/Historique/Sim_nov_2017_paper_2/New data/DataPrice2009.R")

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
St=Data.returns$St
VIX=Data.returns$VIX

#####################################################
###         plot daily log-returns            #######
#####################################################

Data.plot_returns<-data.frame(date=Date.ts,Returns)
Data.plot_St<-data.frame(date=Date.ts,St)
Data.plot_VIX<-data.frame(date=Date.ts,VIX)

plot_returns <- tryCatch(Data.plot_returns, error = function(e) NA)
plot_St <- tryCatch(Data.plot_St, error = function(e) NA)
plot_VIX <- tryCatch(Data.plot_VIX, error = function(e) NA)

plot(as.Date(plot_returns$date), plot_returns$Returns, xlab= "Time", ylab= "Returns (Yt)", type='l', col='blue', xlim=c(as.Date('1999-5-01'),as.Date('2009-9-1')), ylim=c(-0.08,0.08)) 
title(main="Daily log-returns", col.main="black", font.main=4)
grid()

plot(as.Date(plot_returns$date), plot_St$St, xlab= "Time", ylab= "Values (St)", type='l', col='blue', xlim=c(as.Date('1999-5-01'),as.Date('2009-9-1')), ylim=c(600,1550)) 
title(main="(a) S&P500 daily index", col.main="black", font.main=2)
grid()

plot(as.Date(plot_returns$date), plot_VIX$VIX, xlab= "Times", ylab= "VIX Values", type='l', col='blue', xlim=c(as.Date('1999-5-01'),as.Date('2009-9-1'))) 
title(main="(a) VIX daily index", col.main="black", font.main=2)
grid()

plot(as.Date(plot_Vol$date), plot_Vol$Vol, xlab= "Time", ylab= "Annualised Volatility", type='l', col='red', xlim=c(as.Date('1999-9-01'),as.Date('2009-10-1'))) 
title(main=" (b) Annualised Historical Volatility", col.main="black", font.main=2)
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
Data.returns[1:10,]

Data.plot_vol<-data.frame(date=Date.ts,Vol)
plot_Vol <- tryCatch(Data.plot_vol, error = function(e) NA)

plot(as.Date(plot_Vol$date), plot_Vol$Vol, xlab= "Time", ylab= "Volatility", type='l', col='red', xlim=c(as.Date('1999-9-01'),as.Date('2009-10-1'))) 
title(main="Annualised Historical Volatility", col.main="black", font.main=4)
grid()

mean(plot_returns$Returns)

qqnorm(plot_returns$Returns,col = "blue", xlab= "Normal Quantile", ylab= "Quantiles of daily returns"); qqline(plot_returns$Returns, col = "red")


chart.Histogram(plot_returns$Returns, ylim=c(0,50), methods = c( "add.density", "add.normal") , colorset= c("grey67","blue","red"))
legend(0.03, 44, legend=c("S&P500 returns", "Normal"),  col=c("blue","red"), lty=1, cex=0.7)

title(main="Histogram plot", col.main="black", font.main=3)
