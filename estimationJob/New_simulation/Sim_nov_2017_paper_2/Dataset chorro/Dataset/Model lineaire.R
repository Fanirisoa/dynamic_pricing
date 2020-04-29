rm(list=ls())
gc()
library(zoo)

###########################################
### Code to transforme the data set #######
###########################################
## Set working directory
setwd("/Users/leafanirisoa/Documents/projetGit/dynamic_pricing/estimationJob/NIG_GARCH_New_estimation/Test_data")  


path_data=paste(getwd(),"/Dataset/",sep="")
dates_ini=as.Date("07/01/2009","%d/%m/%Y")
dates_fini=as.Date("18/04/2012","%d/%m/%Y")
dates_courantes=seq(dates_ini,dates_fini,7)
n_dates=length(dates_courantes)
dataset=list() # Liste des donn?es
index_list=1
for (i in 1:n_dates){
  nom=paste(path_data,dates_courantes[i],".csv",sep="")
  x=read.delim(nom,header=FALSE,sep=";")
  # Traitement des donn?es
  strike=x[,3]
  Maturity=as.matrix(x[,4])
  SJ=x[,5]
  tsr=x[,6]/100
  prix=x[,7]  
  div=x[,8]
  ttm=x[,9] 
  
  #R?cup?ration des prix
  dataset[[index_list]]=""
  dataset[[index_list]]$strike=strike          
  dataset[[index_list]]$Maturity=Maturity
  dataset[[index_list]]$SJ=SJ
  dataset[[index_list]]$tsr=tsr
  dataset[[index_list]]$prix=prix
  dataset[[index_list]]$div=div
  dataset[[index_list]]$ttm=ttm
  index_list=index_list+1
} 
# Export des donn?es au bon format pour le pricer 
exportation<-function(dataset){
  n=length(dataset)
  strike=list()
  prix=list()
  ttm=c()
  SJ=c()
  tsr=c()
  div=c()
  index=1
  for (i in 1:n){
    strike[[index]]=dataset[[i]]$strike
    prix[[index]]=dataset[[i]]$prix
    ttm[[index]]=dataset[[i]]$ttm
    SJ[[index]]=dataset[[i]]$SJ
    tsr[[index]]=dataset[[i]]$tsr
    div[[index]]=dataset[[i]]$div
    index=index+1
  }
  return(list(prix=prix,strike=strike,ttm=ttm,SJ=SJ,tsr=tsr,div=div))
}
data=exportation(dataset)

price_C <- data$prix     #### Prix du call: data$prix .... Call_market price
Strike_k <-data$strike   #### Prix d'exercice: data$strike
T<-data$ttm              #### Time to maturity: data$ttm
S<- data$SJ              #### Prix du sous-jacent: data$SJ
r<- data$tsr             #### Taux d'interet sans risque: data$tsr
div_d<-data$div          #### dividende: data$div

S_0 <- c()
S_0[1]=903.25
for (i in 2:172){
  S_0[i]=data$SJ[[i-1]][1]
}


###############################################
###  Transforming the data to data frame ######
###############################################
Data.function <- function(s) {data.frame(C=data$prix[[s]],K=data$strike[[s]],S =data$SJ[[s]], T =data$ttm[[s]], r=data$tsr[[s]], d=data$div[[s]], ret=log(data$SJ[[s]])-log(S_0[s])) }
Serie.data <- setNames(lapply(1:10, function(s) Data.function(s)),paste0("d_", 1:10))  

Data.contract <- data.frame()
for(i in 1:length(Serie.data)) {
  Data.contract <- rbind(Data.contract, cbind(Serie.data[[i]]))
}

Data.contract[1:10,] 


####################################################
######          The caracterisation function      ##
####################################################
######    Under the risk neutral probability:     ##
####################################################
###### The moment generating function at tau      ##
####################################################

FC_Q<-function(x,para_h,Data.contract){
  T=Data.contract$T       ####  Time to maturity expressed in terms of years in terms of days
  S=Data.contract$S       ####  Prix du sous-jacent: Data.contract$S
  K=Data.contract$K       ####  Strike  Prix d'exercice: data$strike
  r=Data.contract$r/250   ####  Interest rate Data.contract$r
  ret=Data.contract$ret   ####  Interest rate Data.contract$r
  
  u=1i*x 
  Z=length(S)
  Z1=length(S)+1
  
  # para_h<-c() set up the parameters of the model 
  w=para_h[1]
  b=para_h[2]
  c=para_h[3]
  neta= para_h[4]
  nu=para_h[5]
  a=para_h[6]
  
  # para_h_star<-c()  set up the parameters of the model 
  Pi=para_h[7]
  neta_star=para_h[8]
  
  
  #  The volatility under the physical probability
  
  h_star = c()                                                                            ####  A vector containing h from the model,
  h_star[1]=(w*Pi+a*Pi*(neta^4))/(1-a*(Pi^(-1/3))*(neta^2)-b-(c*(Pi^(1/3))/(neta^2)))     ####  The first value for h,
  # h_star[2]=w*Pi+b*h_star[1]+ (c*Pi/neta)*(ret[i] - r[i] - (nu*h_star[1])/Pi)+(((a*neta/Pi)*(h_star[1])^2)/(ret[i] - r[i]- ((nu*h_star[1]/Pi))))
    
  
  
  # Recursion back to time t
  FC_Q <- rep(NA, Z)
  for (i in 1:Z){
    # Terminal condition for the A and B at time T
    A_Q=0
    B_Q=0
    steps<-round(T[i]*250,0)  #### Time to maturity expressed in terms of years in terms of days
    for (j in 1:steps){
      A_Q= A_Q+ r[i]*u + w*Pi*B_Q-(1/2)*log(1-2*(a*Pi*(neta^4))*B_Q)
      B_Q= b*B_Q+u*(nu/Pi)+ (1/((Pi^(4/3))*(neta^2)))*(1-sqrt((1-2*a*Pi*(neta^4)*B_Q)*( 1- 2*c*(Pi^(5/3))*B_Q - 2*u*neta*Pi^(2/3))))
    }
    FC_Q[i]= exp(log(S[i])*u + A_Q + B_Q*((w*Pi+a*Pi*(neta^4))/(1-a*(Pi^(-1/3))*(neta^2)-b-(c*(Pi^(1/3))/(neta^2)))))
  }
  return(FC_Q)
}

FC_Q(x=1,para_h=para_h, Data.contract=Data.contract)


####################################################
######           Option pricing using FFT         ##
####################################################
######  The call option from the model using FFT  ##
####################################################

Price_fft<-function(para_h,Data.contract){
  T=Data.contract$T       ####  Time to maturity expressed in terms of years in terms of days
  S=Data.contract$S       ####  Prix du sous-jacent: Data.contract$S
  K=Data.contract$K       ####  Strike  Prix d'exercice: data$strike
  r1=Data.contract$r      ####  Interest rate Data.contract$r
  
  T<-round(T*250,0)
  r1<-r1/250
  N=2^10           # Number of subdivision in [0,a]
  alpha=2          # alpha is the parameter to make C square-integrable 
  delta= 0.25      # delta= a/N  where a is the up value of w (w in [0,a])
  lambda=(2*pi)/(N*delta)
  
  j=seq(1,N,1)
  k=seq(1,N,1)
  b=(lambda*N)/2
  strike= -b+(k-1)*lambda
  strike= exp(strike)
  
  
  res=c()
  
  for (i in 1:N){
    w=delta*(i-1)      #  w= j*delta but from 1 to N so w=(i-1)*delta
    w_FC=w-(alpha+1)*1i
    phi= FC_Q(w_FC, para_h,Data.contract)
    phi=phi*exp(-r1*(T))
    phi=phi/(alpha^2+alpha-w^2+1i*(2*alpha+1)*w)
    phi=phi*exp(1i*w*b)
    res=rbind(res,phi)
  }  
  
  option_prices=Re(fft(res))*exp(-alpha*(-b+(k-1)*lambda))/pi
  
  #lookup for the strikes of interest
  result=c() 
  
  for (i in 1:length(K)){
    index=which(strike<=K[i])
    index=index[length(index)]
    result=rbind(result,option_prices[index])
  }  
  
  return(result)
}


start.time <- Sys.time()
Price_fft(para_h=para_h,Data.contract=Data.contract)
end.time <- Sys.time()
time.taken <- end.time - start.time


####################################################
######  Step 4 : Compution the RMSR               ##
####################################################
######   Computation of the Vega                  ##
####################################################
######   Black-Scholes Function for call          ##
####################################################
C_BS <-  function(S, K, T, r, sig, type="C"){
  d1 <- (log(S/K) + (r + sig^2/2)*T) / (sig*sqrt(T))
  d2 <- d1 - sig*sqrt(T)
  if(type=="C"){
    value <- S*pnorm(d1) - K*exp(-r*T)*pnorm(d2)
  }
  if(type=="P"){
    value <- K*exp(-r*T)*pnorm(-d2) - S*pnorm(-d1)
  }
  return(value)
}

## C_BS(100,110,45,.05,.2)
## C_BS(S=Data.contract$S,K=Data.contract$K,T=Data.contract$T,r=Data.contract$r,sig=0.5,"C")
##  mapply(function(S, K, T, r) C_BS(S, K, T, r, sig=0.5, type="C") ,S=Data.contract$S,K=Data.contract$K,T=Data.contract$T,r=Data.contract$r)

####################################################
######   BS Implied Vol using Bisection Method    ##
####################################################
implied.vol <-   function(S, K, T, r, C, type="C"){
  sig <- 0.20
  sig.up <- 1
  sig.down <- 0.001
  count <- 0
  C_market <- C
  err <- C_BS(S, K, T, r, sig, type="C") - C_market 
  
  ## repeat until error is sufficiently small or counter hits 1000
  while(abs(err) > 0.00001 && count<100000){
    if(err < 0){
      sig.down <- sig
      sig <- (sig.up + sig)/2
    }else{
      sig.up <- sig
      sig <- (sig.down + sig)/2
    }
    err <- C_BS(S, K, T, r, sig, type) - C_market
    count <- count + 1
  }
  
  ## return NA if counter hit 1000
  if(count==100000){
    return(NA)
  }else{
    return(sig)
  }
}

### implied.vol(100,95,0.19,0.1,13.7,type="C")

####################################################
######   To compute vega                          ##
####################################################
V<-function(S, K, T, r, C,d, type="C"){ 
  sig<-implied.vol(S, K, T, r, C, type="C")    ## Function to find BS Implied Vol using Bisection Method
  d1 <- (log(S/K) + (r-d + sig^2/2)*T) / (sig*sqrt(T))
  V <-(1.0/sqrt(2*pi))*(S*exp(-r*T))*(exp(-((d1^2))))*sqrt(T) 
  return(V)
}
### V(100,95,0.19,0.1,13.7,0.5,type="C")


Vega <- function(Data.contract, type="C")
{  
  T=Data.contract$T       ####  Time to maturity expressed in terms of years in terms of days
  S=Data.contract$S       ####  Prix du sous-jacent: Data.contract$S
  K=Data.contract$K       ####  Strike  Prix d'exercice: data$strike
  r=Data.contract$r       ####  Interest rate Data.contract$r
  C=Data.contract$C       ####  Call price
  d=Data.contract$d       ####  Call price
  
  vega <- rep(NA, length(C))
  for (i in 1:length(C)){
    vega[i] = V(S[i], K[i], T[i], r[i], C[i], d[i], type="C")
  }
  return(vega)
}

### start.time <- Sys.time()
### Vega(Data.contract=Data.contract[1,], type="C")
### end.time <- Sys.time()
### time.taken <- end.time - start.time
### time.taken



############################################################
#### Function that returns Root Mean Squared Error        ##
############################################################
MSE <- function(para_h,Data.contract)
{  
  C=Data.contract$C       ####  Call price
  P<-Price_fft(para_h=para_h, Data.contract=Data.contract)
  error <- rep(NA, length(C))
  for (i in 1:length(C)){
    error[i] = (P[i]  -  C[i])^2
  }
  MSE<-100*sqrt(mean(error))
  return(MSE)
}
## start.time <- Sys.time()
## MSE(par=para_h,Data.contract=Data.contract[1:4,])
## end.time <- Sys.time()
## time.taken <- end.time - start.time
## time.taken


RMSE <- function(para_h,Data.contract)
{  
  C=Data.contract$C       ####  Call price
  P<-Price_fft(para_h=para_h, Data.contract=Data.contract)
  V<-Vega(Data.contract=Data.contract, type="C")
  error <- rep(NA, length(C))
  for (i in 1:length(C)){
    error[i] = ((P[i]  -  C[i])/V[i])^2
  }
  rmse<-100*sqrt(mean(error))
  return(rmse)
}

## start.time <- Sys.time()
## RMSE(par=para_h,Data.contract=Data.contract)
## end.time <- Sys.time()
## time.taken <- end.time - start.time
## time.taken


############################################################
#### NLS estimation                                       ##
############################################################
para_h<-c(-0.001,0.1,-0.5,0.4,-0.5,-0.5,0.001)
para_h<-c(0.0325364932,  0.1129258110, -0.5088757785,  0.4335364932, -0.5150441004, -0.5055162330,  0.0007259111)

start.time <- Sys.time()
NLS_estim=optim(par=para_h,fn=MSE,Data.contract=Data.contract,method="Nelder-Mead")
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken


start.time <- Sys.time()
NLS_estim=optim(par=para_h,fn=RMSE,Data.contract=Data.contract,method="Nelder-Mead")
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken
NLS_estim
parametres_NLS=NLS_estim$par
parametres_NLS







