rm(list=ls())
gc()
library(zoo)

###########################################
### Code to transforme the data set #######
###########################################
setwd("C:/Users/Leacalin/Desktop/Simulation 2015 chorro/Dataset chorro") 

path_data=paste(getwd(),"/Dataset/",sep="")
dates_ini=as.Date("07/01/2009","%d/%m/%Y")
dates_fini=as.Date("18/04/2012","%d/%m/%Y")
dates_courantes=seq(dates_ini,dates_fini,7)
n_dates=length(dates_courantes)
dataset=list() # Liste des donnèes
index_list=1
for (i in 1:n_dates){
  nom=paste(path_data,dates_courantes[i],".csv",sep="")
  x=read.delim(nom,header=FALSE,sep=";")
  # Traitement des donnÈes
  strike=x[,3]
  Maturity=as.matrix(x[,4])
  SJ=x[,5]
  tsr=x[,6]/100
  prix=x[,7]  
  div=x[,8]
  ttm=x[,9] 
  
  #Récupèration des prix
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
# Export des donnèes au bon format pour le pricer 
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

Mat <- list()
T_Mat <- list()
for (i in 1:172){
  Mat[[i]] <- dataset[[i]]$Maturity
  T_Mat[[i]] <- as.Date(Mat[[i]])-as.Date(dates_courantes[i])
}
T_Mat                       # Diference between T-t

price_C <- data$prix     #### Prix du call: data$prix .... Call_market price
Strike_k <-data$strike   #### Prix d'exercice: data$strike
T<-data$ttm      #### Time to maturity: data$ttm
S<- data$SJ       #### Prix du sous-jacent: data$SJ
r<- data$tsr      #### Taux d'interet sans risque: data$tsr
div_d<-data$div      #### dividende: data$div


#############################################
###Transforming the data to data frame ######
#############################################
Data.function <- function(s) {data.frame(C=data$prix[[s]],K=data$strike[[s]],S =data$SJ[[s]], T =data$ttm[[s]], r=data$tsr[[s]], d=data$div[[s]]) }
Serie.data <- setNames(lapply(1:52, function(s) Data.function(s)),paste0("d_", 1:52))  

Data.contract <- data.frame()
for(i in 1:length(Serie.data)) {
  Data.contract <- rbind(Data.contract, cbind(Serie.data[[i]]))
}

Data.contract


##########################################
### Code to transforme the base_SJ #######
##########################################

setwd("C:/Users/Leacalin/Desktop/Simulation 2015 chorro/Dataset chorro/Dataset") 

x=read.table("Base_SJ.csv",header=FALSE,sep=";")

date=x[,1]
prix_SJ=x[,2]               #### Prix du SJ:
tsr=x[,3]/100               #### Taux d'interet sans risque:
VIX_t=x[,4]                 #### VIX
ret <- diff(log(prix_SJ))   #### returns

prix_SJ=prix_SJ[-1]
tsr=tsr[-1]      
VIX_t=VIX_t[-1]  


Data.ret<-data.frame(St=prix_SJ,rt=tsr,VIX =VIX_t,ret=ret)
Data.ret

####################################################
###### Constant of the model                      ##
####################################################
h1<- sd(Data.ret$ret)       # The first value for h 
h1_star<- sd(Data.ret$ret)  # The first value for h_star 


####################################################
######  Step 1 : The volatility updating rule     ##
####################################################
###### A- Under the historical probability        ##
####################################################

h<-function(para_h,Data.ret){
  ret<-Data.ret$ret   #  returns, 
  r<-Data.ret$rt      #  Taux d'interet sans risque
  
  h = c()     #  A vector containing h from the model,
  h[1]=h1     #  The first value for h,
  
  #para_h<-c() set up the parameters of the model 
  w=para_h[1]
  b=para_h[2]
  c=para_h[3]
  neta= para_h[4]
  nu=para_h[5]
  a=para_h[6]
  
  for (i in 2:length(ret)){
    h[i]=w+ (b*h[i-1])+ ((c/neta)*(ret[i-1] - r[i-1] - (nu*h[i-1])))+((a*neta)*(h[i-1])^2)/(ret[i-1] - r[i-1]- (nu*h[i-1]))
  }
  
  return(h)
}

## h(para_h,Data.ret)

####################################################
###### B- Under the risk neutral probability      ##
####################################################

h_star<-function(para_h,Data.ret){
  ret<-Data.ret$ret   #  returns, 
  r<-Data.ret$rt      #  Taux d'interet sans risque
  
  #para_h<-c() set up the parameters of the model 
  w=para_h[1]
  b=para_h[2]
  c=para_h[3]
  neta= para_h[4]
  nu=para_h[5]
  a=para_h[6]
  
  #para_h_star<-c()  set up the parameters of the model 
  Pi=para_h[7]
  
  
  h_star = c()      #  A vector containing h_star from the model 
  h_star[1]=h1_star # The first value for h_star 
  
  for (i in 2:length(ret)){
    h_star[i]=w*Pi+ (b*h_star[i-1])+ ((c*Pi/neta)*(ret[i-1] - r[i-1] - ((nu/Pi)*h_star[i-1])))+((a*neta/Pi)*(h_star[i-1])^2)/(ret[i-1] - r[i-1]- ((nu/Pi)*h_star[i-1]))
  }
  return(h_star)
}

## para_h<-c(1,0.1,-0.5,0.4,-0.5,-0.5,-0.1,0.2)
## h_star(para_h,Data.ret)


####################################################
######  Step 2 :The caracterisation function      ##
####################################################
###### 1- Under the historical probability        ##
####################################################
###### The moment generating function at tau      ##
####################################################

MGF_P<-function(u,para_h ,Data.ret,Data.contract){
  T=Data.contract$T       ####  Time to maturity expressed in terms of years in terms of days
  S=Data.contract$S       ####  Prix du sous-jacent: Data.contract$S
  K=Data.contract$K       ####  Strike  Prix d'exercice: data$strike
  r1=Data.contract$r      ####  Interest rate Data.contract$r
  
  Z=length(S)
  #para_h<-c() set up the parameters of the model 
  w=para_h[1]
  b=para_h[2]
  c=para_h[3]
  neta= para_h[4]
  nu=para_h[5]
  a=para_h[6]
  
  #  The volatility under the physical probability
  h=h1
  
  # Recursion back to time t
  MGF_P <- rep(NA, Z)
  for (i in 1:Z){
    # Terminal condition for the A and B at time T
    A=0
    B=0
    steps<-round(T[i]*250,0)  #### Time to maturity expressed in terms of years in terms of days
    for (j in 1:steps){
      A= A+ r1[i]*u +w*B-(1/2)*log(1-2*a*(neta^4)*B)
      B= b*B+u*nu+ (neta^(-2))*( 1- sqrt((1-2*a*(neta^4)*B)*(1-2*c*B-2*u*neta))) 
    }
    
    MGF_P[i]= exp(log(S[i])*u + A + B*h )
  }
  return(MGF_P)
  
}

## para_h<-c(1,-0.001,0,0.5,0,0,-0.1,0.2)
## M = MGF_P(u=0.002, para_h=para_h, Data.ret=Data.ret,Data.contract=Data.contract)
## M
####################################################
###### The Caracteristic function                 ##
####################################################

FC_P<-function(u,para_h ,Data.ret ,Data.contract){
  # Complex number : 
  u_Complex=1i*u 
  
  FC_P= MGF_P(u_Complex, para_h=para_h, Data.ret=Data.ret,Data.contract=Data.contract)
  
  return(FC_P)
  
}

## FC_P(u=0.002,para_h=para_h, Data.ret=Data.ret,Data.contract=Data.contract)

####################################################
###### 2- Under the risk neutral probability:     ##
####################################################
###### The moment generating function at tau      ##
####################################################

MGF_Q<-function(u,para_h,Data.ret,Data.contract){
  T=Data.contract$T       ####  Time to maturity expressed in terms of years in terms of days
  S=Data.contract$S       ####  Prix du sous-jacent: Data.contract$S
  K=Data.contract$K       ####  Strike  Prix d'exercice: data$strike
  r1=Data.contract$r      ####  Interest rate Data.contract$r  
  
  Z=length(S)
  
  #para_h<-c() set up the parameters of the model 
  w=para_h[1]
  b=para_h[2]
  c=para_h[3]
  neta= para_h[4]
  nu=para_h[5]
  a=para_h[6]
  
  #para_h_star<-c()  set up the parameters of the model 
  Pi=para_h[7]
  neta_star=para_h[8]
  
  #  The volatility under the physical probability
  h=h1
  h_star=h1
  
  # Recursion back to time t
  MGF_Q <- rep(NA, Z)
  for (i in 1:Z){
    # Terminal condition for the A and B at time T
    A_Q=0
    B_Q=0
    steps<-round(T[i]*250,0)  #### Time to maturity expressed in terms of years in terms of days
    for (j in 1:steps){
      A_Q= A_Q+ r1[i]*u + w*Pi*B_Q-(1/2)*log(1-2*(a/Pi)*neta*(neta_star^3)*B_Q)
      B_Q= b*B_Q+u*(nu/Pi)+ ((neta_star)^(-2))*( 1- sqrt((1-2*(a/Pi)*neta*(neta_star^3)*B_Q)*(1-2*c*Pi*(neta_star/neta)*B_Q-2*u*neta_star))) 
    }
    MGF_Q[i]= exp(log(S[i])*u + A_Q + B_Q*h_star )
  }
  return(MGF_Q)
}

## para_h<-c(1,-0.001,0,0.5,0,0,-0.1,0.2)
## W3<-MGF_Q(u=0.08, para_h=para_h,Data.re=Data.ret,Data.contract=Data.contract)

####################################################
###### The Caracteristic function                 ##
####################################################
FC_Q<-function(u, para_h , Data.ret ,Data.contract){
  # Complex number : 
  u_Complex=1i*u 
  
  FC_Q=  MGF_Q(u_Complex, para_h=para_h, Data.ret=Data.ret,Data.contract=Data.contract)
  
  return(FC_Q)
  
}

## W<-FC_Q(u=0.002, para_h ,Data.ret ,Data.contract)

## start.time <- Sys.time()
## FC_Q(u=0.002, para_h ,Data.ret ,Data.contract)
## end.time <- Sys.time()
## time.taken <- end.time - start.time
## time.taken

####################################################
######  Step 3 : Option pricing using FFT         ##
####################################################
######  The call option from the model using FFT  ##
####################################################

Price_fft<-function(para_h,Data.ret,Data.contract){
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
    phi= FC_Q(w_FC, para_h ,Data.ret ,Data.contract)
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

## start.time <- Sys.time()
## Price_fft(para_h=para_h,Data.ret=Data.ret,Data.contract=Data.contract)
## end.time <- Sys.time()
## time.taken <- end.time - start.time
## time.taken

####################################################
######  Step 4 : Compution the RMSR               ##
####################################################
######   Computation of the Vega                  ##
####################################################
######   Black-Scholes Function for call          ##
####################################################
C_BS <-  function(sig,Data.contract, type="C"){
  T=Data.contract$T       ####  Time to maturity expressed in terms of years in terms of days
  S=Data.contract$S       ####  Prix du sous-jacent: Data.contract$S
  K=Data.contract$K       ####  Strike  Prix d'exercice: data$strike
  r=Data.contract$r       ####  Interest rate Data.contract$r
  #C=Data.contract$C      ####  Call price
  
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

## C_BS(sig=0.5,Data.contract=Data.contract,"C")

####################################################
######   BS Implied Vol using Bisection Method    ##
####################################################
implied.vol <-   function(Data.contract, type="C"){
  T=Data.contract$T       ####  Time to maturity expressed in terms of years in terms of days
  S=Data.contract$S       ####  Prix du sous-jacent: Data.contract$S
  K=Data.contract$K       ####  Strike  Prix d'exercice: data$strike
  r=Data.contract$r       ####  Interest rate Data.contract$r
  C=Data.contract$C      ####  Call price
  
  #T<-T/250
  T<-round(T*250,0)
  C_market <- C
  
  Sigma <- rep(NA, length(C))
  for (i in 1:length(C)){
    sig <- 0.20
    sig.up <- 1
    sig.down <- 0.001
    count <- 0
    err <- c()
    err<- C_BS(sig=sig,Data.contract=Data.contract, type="C")- C_market
    ## repeat until error is sufficiently small or counter hits 1000
    while(abs(err[i]) > 0.00001 && count<100000){
      if(err [i]< 0){
        sig.down <- sig
        sig <- (sig.up + sig)/2
      }else{
        sig.up <- sig
        sig <- (sig.down + sig)/2
      }
      err[i]<- C_BS(sig=sig,Data.contract=Data.contract, type="C")[i]- C_market[i]
      count <- count + 1
    }
    
    ## return NA if counter hit 1000
    if(count==100000){
      return(NA)
    }else{
      Sigma[i]=sig
    }
  }
  return(Sigma)
}

## I<-implied.vol(Data.contract=Data.contract,type="C")

####################################################
######   To compute vega                          ##
####################################################
vega_call<-function(Data.contract,type="C"){ 
  T=Data.contract$T       ####  Time to maturity expressed in terms of years in terms of days
  S=Data.contract$S       ####  Prix du sous-jacent: Data.contract$S
  K=Data.contract$K       ####  Strike  Prix d'exercice: data$strike
  r=Data.contract$r       ####  Interest rate Data.contract$r
  C=Data.contract$C       ####  Call price
  d=Data.contract$d       ####  Call price
  
  sig<-implied.vol(Data.contract=Data.contract,type="C")    ## Function to find BS Implied Vol using Bisection Method
  
  d1 <- (log(S/K) + (r-d + sig^2/2)*T) / (sig*sqrt(T))
  vega_c <-(1.0/sqrt(2*pi))*(S*exp(-r*T))*(exp(-((d1^2))))*sqrt(T) 
  return(vega_c)
}

## V<-vega_call(Data.contract=Data.contract,type="C")

############################################################
#### Function that returns Root Mean Squared Error        ##
############################################################
rmse1 <- function(para_h,Data.contract)
{  
  C=Data.contract$C       ####  Call price
  P<-Price_fft(para_h=para_h,Data.ret=Data.ret,Data.contract=Data.contract)
  V<-vega_call(Data.contract=Data.contract,type="C")
  error <- rep(NA, length(C))
  for (i in 1:length(C)){
    error[i] = ((P[i]  -  C[i])/V[i])^2
  }
  rmse<-100*sqrt(mean(error))
  return(rmse)
}
## start.time <- Sys.time()
## rmse1(par=para_h,Data.contract=Data.contract)
## end.time <- Sys.time()
## time.taken <- end.time - start.time
## time.taken

############################################################
#### NLS estimation                                       ##
############################################################
para_h<-c(0.074,0.001,0.1,0.5,0.9,0.95,-0.1,-0.2)

start.time <- Sys.time()
NLS_estim=optim(par=para_h,fn=rmse1,Data.contract=Data.contract[1,],method="Nelder-Mead")
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken


parametres_NLS=NLS_estim$par
warnings()