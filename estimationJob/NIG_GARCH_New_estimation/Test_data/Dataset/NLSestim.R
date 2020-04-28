
############################################################
#### Function that returns Root Mean Squared Error        ##
############################################################
rmse1 <- function(par,Data.contract)
{  
  C=Data.contract$C       ####  Call price
  P<-Price_fft(para_h=par,Data.contract=Data.contract)
  V<-vega_call(Data.contract=Data.contract,type="C")
  error <- rep(NA, length(C))
  for (i in 1:length(C)){
    error[i] = ((P[i]  -  C[i])/V[i])^2
  }
  rmse<-100*sqrt(mean(error))
  return(rmse)
}
start.time <- Sys.time()
rmse1(par=para_h,Data.contract=Data.contract)
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken

############################################################
#### NLS estimation                                       ##
############################################################
para_h<-c(-1,0.001,0,-0.5,0,-0.5,-0.1,-0.2)

start.time <- Sys.time()
NLS_estim=optim(par=para_h,fn=rmse1,Data.contract=Data.contract,method="L-BFGS-B",lower=c(-1,-1,-1,-1,-1,-1,-1,-1), upper=c(1,1,1,1,1,1,1,1))
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken

parametres_NLS=NLS_estim$par
warnings()



############################################################
#### NLS estimation                                       ##
############################################################
para_h<-c(0.074,0.001,0.1,0.5,0.9,0.95,-0.1)
para_h<-c(-0.001,0.1,-0.5,0.4,-0.5,-0.5,0.001)


start.time <- Sys.time()
NLS_estim=optim(par=para_h,fn=rmse1,Data.contract=Data.contract,method="Nelder-Mead")
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken


parametres_NLS=NLS_estim$par
warnings()



