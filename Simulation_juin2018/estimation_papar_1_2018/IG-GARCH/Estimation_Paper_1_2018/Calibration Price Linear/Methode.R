rm(list=ls())
gc()
library(compiler)
enableJIT(1)
enableJIT(3)

#####################################################
###    Load both data.contract and data.ret   #######
#####################################################
##load("DataPrice20092010.Rdata")
##load("DataPrice20112012.Rdata")

 load("DataPrice2009.Rdata")
## load("Dataprice2010.Rdata")

#####################################################
###         Source function to use            #######
#####################################################
source("C:/Users/e0g411k03dt/Desktop/Estimation Paper 1,juillet 2016/Calibration Price Linear/Calibration function.r")


source("C:/Users/e0g411k03dt/Desktop/Estimation Paper 1,juillet 2016/Calibration Price Linear/Lin TESTGMM.r")
source("C:/Users/e0g411k03dt/Desktop/Estimation Paper 1,juillet 2016/Calibration Price Linear/qua TESTGMM.r")
source("C:/Users/e0g411k03dt/Desktop/Estimation Paper 1,juillet 2016/Calibration Price Linear/Table RMSE.r")

#####################################################
###                 Data set                  #######
#####################################################
Data.N=Data.N3



#####################################################
###         Parameters of the model           #######
#####################################################
###   Initial parameter  ##
##   w=para_h[1]; b=para_h[2]; a=para_h[3];  c= para_h[4]; neta=para_h[5]
para_h<-c(2.181817e-04, 9.872674e-02 , 2.478763e+01,  6.332386e-05, -9.564263e-03)

###   Solution 
para_h <-c(5.196270e-05, 9.876680e-02,   2.478770e+01,   1.352510e-09,  -9.594271e-03)
para_h <-c(9.317716e-03,  2.061124e-05 , 3.317425e+02,  4.959148e-02, -8.275156e-03)

para_h<-c(1.115818e-04,  9.876121e-02,  2.478762e+01 , 5.375749e-05, -9.434003e-03 )
#####################################################
###               Volatility                  #######
#####################################################

ts.vol=h(para_h,Data.ret)
ts.plot(ts.vol, col = "steelblue", main = "IG Garch Model",xlab="2009",ylab="Volatility")
grid()


############################################################
####               NLS estimation                         ##
############################################################
start.time <- Sys.time()
NLSMSE=optim(par=para_h,fn=RMSE,Data.ret=Data.ret,Data.N=Data.N,method="Nelder-Mead",control = list(maxit = 1000))
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken

para_h1<- NLSMSE$par

############################################################
####                        RMSE                          ##
############################################################
start.time <- Sys.time()
RMSE1=RMSE(para_h,Data.ret,Data.N)
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken


start.time <- Sys.time()
RMSE2=RMSE(para_h1,Data.ret,Data.N)
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken

############################################################
####               Hessian Matrix                         ##
############################################################
start.time <- Sys.time()
hess = hessian(func=RMSE, x=para_h,Data.ret=Data.ret,Data.contract=Data.contract)
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken

hessc <- hessian(func=RMSE, x=para_h, "complex",Data.ret=Data.ret,Data.contract=Data.contract)
all.equal(hess, hessc, tolerance = .Machine$double.eps)



#####################################################
###         Implicite volatility              #######
#####################################################

Ip <- function(para_h,Data.ret,Data.contract)
{ 
  T=Data.contract$T       ####  Time to maturity expressed in terms of years in terms of days
  S=Data.contract$S       ####  Prix du sous-jacent: Data.contract$S
  K=Data.contract$K       ####  Strike  Prix d'exercice: data$strike
  r=Data.contract$r       ####  Interest rate Data.contract$r
  C=Data.contract$C       ####  Call price
  d=Data.contract$d       ####  Call dividende
  
  Ip <- rep(NA, length(C))
  for (i in 1:length(C)){
    Ip[i] = implied.vol(S[i], K[i], T[i], r[i], C[i],d[i], type="C")
  }
  return(Ip)
}
start.time <- Sys.time()
Ipv=Ip(para_h1,Data.ret,Data.N)
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken

############################################################
####                    Vega                              ##
############################################################
start.time <- Sys.time()
Vega1=Vega(Data.N, type="C")
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken


#####################################################
###         Volatility and  Price             #######
#####################################################

ts.vol=h(para_h,Data.ret)
ts.plot(ts.vol, col = "steelblue", main = "IG Garch Model",xlab="2009",ylab="Volatility")
grid()
legend(350, 0.000946, c("h*(t)"), col =c("steelblue"), lty = 1)

l=100


ts.vol=FC_Q(1,para_h,Data.ret,Data.N)
ts.plot(ts.vol, col = "steelblue", main = "IG Garch Model",xlab="2009",ylab="Volatility")
grid()
legend(350, 0.000946, c("h*(t)"), col =c("steelblue"), lty = 1)


start.time <- Sys.time()
P=Price_fft(para_h=para_h,Data.ret=Data.ret,Data.contract=Data.N[1:l,])
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken
C=Data.N$C
Ca=C[1:l]
Pa=P[1:l]

ts.plot(Pa,ts(Ca), col =c("steelblue","red"), main = "Valuation with IG Garch Model",xlab="2009",ylab="Prices")
legend(175, 250,  c("C(t)","C*(t)"), col =c("steelblue","red"), lty = c(1, 1))

