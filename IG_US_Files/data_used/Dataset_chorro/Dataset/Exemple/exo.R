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
  
  #para_h<-c() set up the parameters of the model para_h[1]=w, para_h[2]=b, para_h[3]=c, para_h[4]=neta, para_h[5]=nu, para_h[6]=a
  
  for (i in 2:length(ret)){
    h[i]=para_h[1]+ (para_h[2]*h[i-1])+ ((para_h[3]/para_h[4])*(ret[i-1] - r[i-1] - (para_h[5]*h[i-1])))+((para_h[6]*para_h[4])*(h[i-1])^2)/(ret[i-1] - r[i-1]- (para_h[5]*h[i-1]))
  }
  
  return(h)
}

para_h<-c(1,0.1,-0.5,0.4,-0.5,-0.5)
h(para_h,Data.ret)

####################################################
###### B- Under the risk neutral probability      ##
####################################################

h_star<-function(para_h,para_h_star,Data.ret){
  ret<-Data.ret$ret   #  returns, 
  r<-Data.ret$rt      #  Taux d'interet sans risque
  
  h_star = c()      #  A vector containing h_star from the model 
  h_star[1]=h1_star # The first value for h_star 
  
  #para_h_star<-c()  set up the parameters of the model para_h_star[1]=Pi, para_h_star[2]=neta_star
  r<-tsr
  
  for (i in 2:length(ret)){
    h_star[i]=para_h[1]*para_h_star[1]+ (para_h[2]*h_star[i-1])+ ((para_h[3]*para_h_star[1]/para_h[4])*(ret[i-1] - r[i-1] - ((para_h[5]/para_h_star[1])*h_star[i-1])))+((para_h[6]*para_h[4]/para_h_star[1])*(h_star[i-1])^2)/(ret[i-1] - r[i-1]- ((para_h[5]/para_h_star[1])*h_star[i-1]))
  }
  return(h_star)
}

para_h_star<-0.9
h_star(para_h,para_h_star,Data.ret)


####################################################
######  Step 2 :The caracterisation function      ##
####################################################
###### 1- Under the historical probability        ##
####################################################
###### The moment generating function at tau      ##
####################################################

MGF_P<-function(u,tau, para_h ,Data.ret ,S ,T){
  ret         ####  returns, 
  r<-tsr      ####  Taux d'interet sans risque
  
  
  steps<-round(T*250,0)   #### Time to maturity expressed in terms of years in terms of days
  S<- S      #### Prix du sous-jacent: Data.contract$S
  
  
  #  The volatility under the physical probability
  h <- h(para_h,Data.ret)  # Obtain from updating rule
  
  # Terminal condition for the A and B at time T
  A_T=0
  B_T=0
  A=c()
  B=c()
  
  #  A and B a time T-1
  
  A[1]= r[steps]*u 
  B[1]= u*para_h[5]+ ((para_h[4])^(-2))*( 1- sqrt(1-2*u*para_h[4])) 
  
  
  # Recursion back to time t
  
  for (i in 2:steps){
    A[i]= A[i-1]+ r[steps-i+1]*u + para_h[1]*B[i-1]-(1/2)*log(1-2*para_h[6]*(para_h[4]^4)*B[i-1])
    B[i]= para_h[2]*B[i-1]+u*para_h[5]+ (para_h[4]^(-2))*( 1- sqrt((1-2*para_h[6]*(para_h[4]^4)*B[i-1])*(1-2*para_h[3]*B[i-1]-2*u*para_h[4]))) 
  }
  
  MGF_P = exp(log(S)*u + A[tau] + B[tau]*h[tau] )
  
  return(MGF_P)
  
}

mapply(function(S,T) MGF_P(u=1,tau=3, para_h=para_h, Data.ret=Data.ret,S,T) ,S=Data.contract$S,T=Data.contract$T)

#MGF <- list()
#S=data$SJ[[1]]
#T=data$ttm[[1]]
#D<-data.frame(S, T)
#mapply(function(S,T) MGF_P(u=1,tau=2,para_h=para_h, Data.ret=Data.ret,S,T),S=Data.contract$S,T=Data.contract$T)

####################################################
###### The Caracteristic function                 ##
####################################################

FC_P<-function(u,tau, para_h ,Data.ret ,S ,T){
  # Complex number : 
  u_Complex=1i*u 
  
  FC_P= MGF_P(u,tau, para_h ,Data.ret ,S ,T)
  
  return(FC_P)
  
}

## mapply(function(S,T) FC_P(u=1,tau=3, para_h=para_h, Data.ret=Data.ret,S,T) ,S=Data.contract$S,T=Data.contract$T)

####################################################
###### 2- Under the risk neutral probability:     ##
####################################################
###### The moment generating function at tau      ##
####################################################
MGF_Q<-function(u,tau, para_h,para_h_star,Data.ret,S ,T){
  ret         ####  returns, 
  r<-tsr      ####  Taux d'interet sans risque
  
  steps<-round(T*250,0)    #### Time to maturity expressed in terms of years in terms of days
  S<- S                    #### Prix du sous-jacent: Data.contract$S
  
  #  The volatility under the risk neutral probability, Obtain from updating rule
  h_star<- h_star(para_h,para_h_star,Data.ret)     
  
  # Terminal condition for the A and B at time T
  A_Q_T=0
  B_Q_T=0
  A_Q=c()
  B_Q=c()
  
  #  A and B a time T-1
  
  A_Q[1]= r[steps]*u 
  B_Q[1]= u*(para_h[5]/para_h_star[1])  
  # Recursion back to time t
  
  for (i in 2:steps){
    A_Q[i]= A_Q[i-1]+ r[steps-i+1]*u + para_h[1]*para_h_star[1]*B_Q[i-1]-(1/2)*log(1-2*(para_h[6]/para_h_star[1])*para_h[4]*(para_h_star[2]^3)*B_Q[i-1])
    B_Q[i]= para_h[2]*B_Q[i-1]+u*(para_h[5]/para_h_star[1])+ ((para_h_star[2])^(-2))*( 1- sqrt((1-2*(para_h[6]/para_h_star[1])*para_h[4]*(para_h_star[2]^3)*B_Q[i-1])*(1-2*para_h[3]*para_h_star[1]*(para_h_star[2]/para_h[4])*B_Q[i-1]-2*u*para_h_star[2]))) 
    
  }
  
  MGF_Q = (log(S)*u + A_Q[tau] + B_Q[tau]*h_star[tau] )
  
  return(MGF_Q)
  
}

## mapply(function(S,T) MGF_Q(u=1,tau=3, para_h=para_h,para_h_star=para_h_star, Data.ret=Data.ret,S,T) ,S=Data.contract$S,T=Data.contract$T)


####################################################
###### The Caracteristic function                 ##
####################################################

FC_Q<-function(u,tau, para_h ,para_h_star,Data.ret ,S ,T){
  # Complex number : 
  u_Complex=1i*u 
  
  FC_Q= MGF_Q(u,tau, para_h ,para_h_star,Data.ret ,S ,T)
  
  return(FC_Q)
  
}

## mapply(function(S,T) FC_Q(u=1,tau=3, para_h=para_h,para_h_star=para_h_star, Data.ret=Data.ret,S,T) ,S=Data.contract$S,T=Data.contract$T )



####################################################
######  Step 3 : Option pricing using FFT         ##
####################################################
######  The call option from the model using FFT  ##
####################################################

Price_fft<-function(tau,para_h,para_h_star,Data.ret,S,T,K,r1){
  ## r<-tsr      ####  Taux d'interet sans risque
  ## steps<-T    ####  Time to maturity expressed in terms of years in terms of days
  ## S<- S       ####  Prix du sous-jacent: Data.contract$S
  ## K<- K       ####  Strike  Prix d'exercice: data$strike
  ## r1<-        ####  Interest rate Data.contract$r
  
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
    phi=FC_Q(w_FC,tau, para_h ,para_h_star,Data.ret ,S ,T)
    phi=phi*exp(-r1*(T-tau))
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
Price_fft(tau=1, para_h=para_h,para_h_star=para_h_star,Data.ret=Data.ret,S=Data.contract$S,T=Data.contract$T,K=Data.contract$K,r1=Data.contract$r)
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken

#start.time <- Sys.time()
#mapply(function(S,T,K,r1) Price_fft(tau=3,para_h=para_h,para_h_star=para_h_star, Data.ret=Data.ret,S,T,K,r1) ,S=Data.contract$S,T=Data.contract$T,K=Data.contract$K,r1=Data.contract$r)
#end.time <- Sys.time()
#time.taken <- end.time - start.time
#time.taken

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

C_BS(100,110,1,.05,.2)

## C_BS(S=Data.contract$S,K=Data.contract$K,T=Data.contract$T,r=Data.contract$r,sig=0.5,"C")

##  mapply(function(S, K, T, r) C_BS(S, K, T, r, sig=0.5, type="C") ,S=Data.contract$S,K=Data.contract$K,T=Data.contract$T,r=Data.contract$r)

####################################################
######   BS Implied Vol using Bisection Method    ##
####################################################
implied.vol <-   function(S, K, T, r, C, type="C"){
  T<-T/250
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

## implied.vol(100,95,3*12,0.1,13.7,type="C")

## mapply(function(S, K, T, r, C) implied.vol(S, K, T, r, C, type="C") ,S=Data.contract$S,K=Data.contract$K,T=Data.contract$T,r=Data.contract$r,C=Data.contract$C)

####################################################
######   To compute vega                          ##
####################################################
vega_call<-function(S, K, T, r, C,d, type="C"){ 
  
  #T=T/250                        ####   Maturity in years                  
  # C                              ####   Prix du call: data$prix, Call_market price
  # d                              ####   dividende: data$div
  
  # sig<-implied.vol(S, K, T, r, C, type="C")    ## Function to find BS Implied Vol using Bisection Method
  
  sig<-mapply(function(S, K, T, r, C) implied.vol(S, K, T, r, C, type="C") ,S=Data.contract$S,K=Data.contract$K,T=(Data.contract$T)/250,r=Data.contract$r,C=Data.contract$C)
  
  d1 <- (log(S/K) + (r-d + sig^2/2)*T) / (sig*sqrt(T))
  vega_c <-(1.0/sqrt(2*pi))*(S*exp(-r*T))*(exp(-((d1^2))))*sqrt(T) 
  return(vega_c)
}

vega_call(S=Data.contract$S, K=Data.contract$K, T=Data.contract$T, r=Data.contract$r, C=Data.contract$C,d=Data.contract$d, type="C")
## mapply(function(S, K, T, r, C,d) vega_call(S, K, T, r, C,d, type="C") ,S=Data.contract$S, K=Data.contract$K, T=Data.contract$T, r=Data.contract$r, C=Data.contract$C,d=Data.contract$d)

############################################################
#### Function that returns Root Mean Squared Error        ##
############################################################
rmse <- function(tau,para_h,para_h_star,Data.ret,Data.contract)
{  
  
  C=Data.contract$C                       #### Prix du call: data$prix .... Call_market price
  K=Data.contract$K                       #### Prix d'exercice: data$strike
  T=Data.contract$T                       #### Time to maturity: data$ttm
  S=Data.contract$S                       #### Prix du sous-jacent: data$SJ
  r1=Data.contract$r                      #### Taux d'interet sans risque: data$tsr
  d=Data.contract$d                       #### dividende: data$div
  
  P<-Price_fft(tau=1, para_h=para_h,para_h_star=para_h_star,Data.ret=Data.ret,S=Data.contract$S,T=Data.contract$T,K=Data.contract$K,r1=Data.contract$r)
  V<-vega_call(S, K, (Data.contract$T)/250, r1, C,d, type="C")
  error<- ((P  -  C)/V)^2
  rmse<-100*sqrt(mean(error))
  return(rmse)
}

start.time <- Sys.time()
rmse(tau=1, para_h=para_h,para_h_star=para_h_star,Data.ret=Data.ret,Data.contract=Data.contract)
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken


############################################################
#### NLS estimation                                       ##
############################################################
para=c(para_h,para_h_star)
NLS_estim=optim(para_h,rmse,para_h_star=0.9,tau=1,Data.ret=Data.ret, Data.contract=Data.contract,method="L-BFGS-B",lower=c(-11,-12,-13,-14,-15,-16,-17), upper=c(11,12,13,14,15,16,17))

parametres_NLS=NLS_estim$par



para_h<-c(0.1,0.2,0.3,0.4,0.5,0.7)
para_h_star<-0.9

warnings()




