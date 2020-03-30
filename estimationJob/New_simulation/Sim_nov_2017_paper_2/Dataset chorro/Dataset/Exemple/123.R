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
T<-data$ttm      #### Time to maturity: data$ttm
S<- data$SJ       #### Prix du sous-jacent: data$SJ
r<- data$tsr      #### Taux d'interet sans risque: data$tsr
div_d<-data$div      #### dividende: data$div


###############################################
###  Transforming the data to data frame ######
###############################################
Pe=c(0,5,4,5,5,5,4,5,5,5,5,5,5,5,3,5,5,4,5,5,4,5,5,5,5,5,4,5,5,5,5,5,5,5,4,4,5,5,5,5,5,5,5,5,5,5,5,4,5,5,5,3)
d=c(0,5,4,5,5,5,4,5,5,5,5,5,5,5,3,5,5,4,5,5,4,5,5,5,5,5,4,5,5,5,5,5,5,5,4,4,5,5,5,5,5,5,5,5,5,5,5,4,5,5,5,3)
d2=c()
d2[1]=250
for(i in 2:length(d)) {
  d2[i]<- d2[i-1]+d[i]
}
Pe
d2

d2Data.function <- function(s) {data.frame(C=data$prix[[s]],K=data$strike[[s]],S =data$SJ[[s]], T =data$ttm[[s]], r=data$tsr[[s]], d=data$div[[s]],Pe=Pe[s],Per=Per[s]) }
Serie.data <- setNames(lapply(1:52, function(s) Data.function(s)),paste0("d_", 1:52))  

Data.contract <- data.frame()
for(i in 1:length(Serie.data)) {
  Data.contract <- rbind(Data.contract, cbind(Serie.data[[i]]))
}

Data.contract
length(Data.contract$S)
##########################################
### Code to extracte the base_SJ   #######
##########################################

setwd("C:/Users/Leacalin/Desktop/Simulation 2015 chorro/Dataset chorro/Dataset") 

x=read.table("Base_SJ.csv",header=FALSE,sep=";")

date=x[,1]
prix_SJ=x[,2]               #### Prix du SJ:
tsr=x[,3]/100               #### Taux d'interet sans risque:
VIX_t=x[,4]                 #### VIX
ret <- diff(log(prix_SJ))   #### returns

date=date[-1]
prix_SJ=prix_SJ[-1]
tsr=tsr[-1]      
VIX_t=VIX_t[-1]  


Data.BSJ<-data.frame(date=date,St=prix_SJ,rt=tsr,VIX =VIX_t,ret=ret)
Data.BSJ
L<-length(date)
##########################################
###    To transform  the base_SJ   #######
##########################################

Data.transf <- function(s) {data.frame(date=Data.BSJ$date[[s]],St=Data.BSJ$St[[s]],rt=Data.BSJ$rt[[s]],VIX=Data.BSJ$VIX[[s]], ret =Data.BSJ$ret[[s]] ) }
Serie.dataBSJ <- setNames(lapply(4391:4886, function(s) Data.transf(s)),paste0("d_", 4391:4886))  

Data.ret <- data.frame()
for(i in 1:length(Serie.dataBSJ)) {
  Data.ret <- rbind(Data.ret, cbind(Serie.dataBSJ[[i]]))
}
Data.ret[250+5+4,]
length(Data.ret$date)
Data.ret$date[496]

dim(Data.ret)

## Data.ret$St[250+5+4+5+5+5+4+5+5+5+5+5+5+5+3+5+5+4+5+5+4+5+5+5+5+5+4+5+5+5+5+5+5+5+4+4+5+5+5+5+5+5+5+5+5+5+5+4+5+5+5+3]
## d=c(0,5,4,5,5,5,4,5,5,5,5,5,5,5,3,5,5,4,5,5,4,5,5,5,5,5,4,5,5,5,5,5,5,5,4,4,5,5,5,5,5,5,5,5,5,5,5,4,5,5,5,3)




####################################################
######  Step 1 : The volatility updating rule     ##
####################################################
###### A- Under the historical probability        ##
####################################################

h<-function(para_h,Data.ret){
  S=Data.ret$St        #### Prix du sous-jacent: Data.BSJ$St
  r=Data.ret$rt        #### Interest rate Data : Data.BSJ$rt
  VIX=Data.ret$VIX     #### VIX : Data.BSJ$VIX
  ret =Data.ret$ret    #### Returns : Data.BSJ$ret
  
  Z=length(S)+1
  
  #  para_h<-c() set up the parameters of the model 
  w=para_h[1]
  b=para_h[2]
  c=para_h[3]
  neta= para_h[4]
  nu=para_h[5]
  a=para_h[6]
  
  ### para_h_star<-c()  set up the parameters of the model 
  Pi=para_h[7]
  
  ###  A vector containing h from the model,
  h = c()     
  h[1]=(w+(neta^4)*a)/(1-a*(neta^2)-b-(c/(neta^2)))    #  The first value for h,
  
  for (i in 2:Z){
    h[i]=w+(b*h[i-1])+ ((c/neta)*(ret[i-1] - r[i-1] - (nu*h[i-1])))+((a*neta)*(h[i-1])^2)/(ret[i-1] - r[i-1]- (nu*h[i-1]))
  }
  return(h)
}

para_h<-c(1,0.1,-0.5,0.4,-0.5,-0.5,-0.1,0.2)
h(para_h,Data.ret)

####################################################
###### B- Under the risk neutral probability      ##
####################################################

h_star<-function(para_h,Data.ret){
  S=Data.ret$St        #### Prix du sous-jacent: Data.BSJ$St
  r=Data.ret$rt        #### Interest rate Data : Data.BSJ$rt
  VIX=Data.ret$VIX     #### VIX : Data.BSJ$VIX
  ret =Data.ret$ret    #### Returns : Data.BSJ$ret
  
  Z=length(S)+1
  
  # para_h<-c() set up the parameters of the model 
  w=para_h[1]
  b=para_h[2]
  c=para_h[3]
  neta= para_h[4]
  nu=para_h[5]
  a=para_h[6]
  
  #para_h_star<-c()  set up the parameters of the model 
  Pi=para_h[7]
  
  #  The volatility under the physical probability
  
  h_star = c()     #  A vector containing h from the model,
  h_star[1]=(w*Pi+a*Pi*(neta^4))/(1-a*(Pi^(-1/3))*(neta^2)-b-(c*(Pi^(1/3))/(neta^2)))   #  The first value for h,
  for (i in 2:Z){
    h_star[i]=w*Pi+b*h_star[i-1]+ (c*Pi/neta)*(ret[i-1] - r[i-1] - (nu*h_star[i-1])/Pi)+(((a*neta/Pi)*(h_star[i-1])^2)/(ret[i-1] - r[i-1]- ((nu*h_star[i-1]/Pi))))
  }
  return(h_star)
}
para_h<-c(1,0.1,-0.5,0.4,-0.5,-0.5,0.1)

H=h_star(para_h,Data.ret)
H[1]


####################################################
######  Step 2 :The caracterisation function      ##
####################################################
###### 1- Under the historical probability        ##
####################################################
###### The moment generating function at tau      ##
####################################################

FC_P<-function(x,para_h,Data.ret ,Data.contract){
  T=Data.contract$T       ####  Time to maturity expressed in terms of years in terms of days
  S=Data.contract$S       ####  Prix du sous-jacent: Data.contract$S
  K=Data.contract$K       ####  Strike  Prix d'exercice: data$strike
  r=Data.contract$r/250   ####  Interest rate Data.contract$r
  Per=Data.contract$Per     ####  Interest rate Data.contract$r
  
  u=1i*x 
  Z=length(S)
  
  rt=Data.ret$rt        #### Interest rate Data : Data.BSJ$rt
  ret =Data.ret$ret     #### Returns : Data.BSJ$ret
  
  Z1=length(S)+1
  
  # para_h<-c() set up the parameters of the model 
  w=para_h[1]
  b=para_h[2]
  c=para_h[3]
  neta= para_h[4]
  nu=para_h[5]
  a=para_h[6]
  
  # The volatility updating rule 
  h = c()     
  h[1]=(w+(neta^4)*a)/(1-a*(neta^2)-b-(c/(neta^2)))    #  The first value for h,
  
  for (i in 2:Z1){
    h[i]=w+(b*h[i-1])+ ((c/neta)*(ret[i-1] - rt[i-1] - (nu*h[i-1])))+((a*neta)*(h[i-1])^2)/(ret[i-1] - rt[i-1]- (nu*h[i-1]))
  }
    
  # Recursion back to time t
  FC_P <- rep(NA, Z)
  for (i in 1:Z){
    # Terminal condition for the A and B at time T
    A=0
    B=0
    steps<-round(T[i]*250,0)  #### Time to maturity expressed in terms of years in terms of days
    for (j in 1:steps){
      A= A+ r[i]*u +w*B-(1/2)*log(1-2*a*(neta^4)*B)
      B= b*B+u*nu+(neta^(-2))*( 1- sqrt((1-2*a*(neta^4)*B)*(1-2*c*B-2*u*neta))) 
    }
    
    FC_P[i]= exp(log(S[i])*u + A + B*h[Per[i]])
  }
  return(FC_P)
  
}

para_h<-c(-0.004,0.001,0,-0.005,0,-0.005,-0.001,-0.002)
M = FC_P(x=0.001, para_h=para_h,Data.ret=Data.ret, Data.contract=Data.contract)
M

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
  
  for (i in 2:Z1){
    h_star[i]=w*Pi+b*h_star[i-1]+ (c*Pi/neta)*(ret[i-1] - r[i-1] - (nu*h_star[i-1])/Pi)+(((a*neta/Pi)*(h_star[i-1])^2)/(ret[i-1] - r[i-1]- ((nu*h_star[i-1]/Pi))))
    
  }
  
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
    FC_Q[i]= exp(log(S[i])*u + A_Q + B_Q*h_star[i+1])
  }
  return(FC_Q)
}

para_h<-c(-0.001,0.1,-0.5,0.4,-0.5,-0.5,0.001)
W4<-FC_Q(x=0.08,para_h=para_h, Data.contract=Data.contract)
W4


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
para_h<-c(-0.001,0.1,-0.5,0.4,-0.5,-0.5,0.001)

start.time <- Sys.time()
Price_fft(para_h=para_h,Data.contract=Data.contract)
end.time <- Sys.time()
time.taken <- end.time - start.time






