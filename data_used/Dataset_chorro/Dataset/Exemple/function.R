rm(list=ls())
gc()
library(zoo)

###### The data set :
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
  ttm=round(x[,9]*250,0)
  
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
  index=1
  for (i in 1:n){
    strike[[index]]=dataset[[i]]$strike
    prix[[index]]=dataset[[i]]$prix
    ttm[[index]]=dataset[[i]]$ttm
    SJ[[index]]=dataset[[i]]$SJ[1]
    tsr[[index]]=dataset[[i]]$tsr[1]
    div[[index]]=dataset[[i]]$div[1]
    index=index+1
  }
  return(list(prix=prix,strike=strike,ttm=ttm,SJ=SJ,tsr=tsr,div=div))
}
data=exportation(dataset)


price_C <- data$prix     #### Prix du call: data$prix .... Call_market price
Strike_k <-data$strike   #### Prix d'exercice: data$strike
ttm<-data$ttm     #### Time to maturity: data$TM in years
S<- data$SJ       #### Prix du sous-jacent: data$SJ
r<- data$tsr      #### Taux d'interet sans risque: data$tsr
div_d<-data$div      #### dividende: data$div



### Code to transforme the base_SJ #######
###########################################

setwd("C:/Users/Leacalin/Desktop/Simulation 2015 chorro/Dataset chorro/Dataset") 

x=read.table("Base_SJ.csv",header=FALSE,sep=";")

date=x[,1]
prix_SJ=x[,2]      #### Prix du SJ:
tsr=x[,3]/100      #### Taux d'interet sans risque:
VIX_t=x[,4]        #### VIX
ret <- diff(log(prix_SJ))

length(ret)

###### Constant of the model :
r0<- 0.25        # The interet 
h1<- sd(ret)    # The first value for h 
h1_star<- sd(ret)      # The first value for h_star 
  
######  Step 1 : The volatility updating rule
###### 1- Under the historical probability:
h<-function(para_h,ret,r){
    #r          #  Taux d'interet sans risque
    # ret       #  returns, 
    h = c()     #  A vector containing h from the model,
    h[1]=h1     #  The first value for h,
    
    #para_h<-c() set up the parameters of the model para_h[1]=w, para_h[2]=b, para_h[3]=c, para_h[4]=neta, para_h[5]=nu, para_h[6]=a
    
    for (i in 2:length(ret)){
      h[i]=para_h[1]+ (para_h[2]*h[i-1])+ ((para_h[3]/para_h[4])*(ret[i-1] - r[i-1] - (para_h[5]*h[i-1])))+((para_h[6]*para_h[4])*(h[i-1])^2)/(ret[i-1] - r[i-1]- (para_h[5]*h[i-1]))
    }
    
    return(h)
  }

para_h<-c(0.1,0.2,0.3,0.4,0.5,0.7)
h(para_h,ret,tsr)

###### 2- Under the risk neutral probability:  
h_star<-function(para_h,para_h_star,ret,r){
    #r          #  Taux d'interet sans risque
    # ret       #  returns, 
                
    h_star = c()      #  A vector containing h_star from the model 
    h_star[1]=h1_star # The first value for h_star 
    
    #para_h_star<-c()  set up the parameters of the model para_h_star[1]=Pi, para_h_star[2]=neta_star
    
    
    for (i in 2:length(ret)){
      h_star[i]=para_h[1]*para_h_star[1]+ (para_h[2]*h_star[i-1])+ ((para_h[3]*para_h_star[1]/para_h[4])*(ret[i-1] - r[i-1] - ((para_h[5]/para_h_star[1])*h_star[i-1])))+((para_h[6]*para_h[4]/para_h_star[1])*(h_star[i-1])^2)/(ret[i-1] - r[i-1]- ((para_h[5]/para_h_star[1])*h_star[i-1]))
    }
    return(h_star)
  }

para_h_star<-0.9
h_star(para_h,para_h_star,ret,tsr)

######  Step 2 :The caracterisation function using the moment generating function
##########################################################
###### 1- Under the historical probability:
##########################################################
# The moment generating function  under the physical probability 


MGF_P<-function(u, para_h ,tau,ret,tsr,S,steps){
  
  ## tsr     #  Taux d'interet sans risque
  ## ret     #  returns,
  ## steps<-data$ttm   #### Transforms the time to maturity expressed in terms of years in terms of days
  ## S<- data$SJ       #### Prix du sous-jacent: data$SJ
  
  r<- tsr
  
  #  The volatility under the physical probability
  
  h <- h(para_h,ret,r)  # Obtain from updating rule
  
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
  
  MGF_P = exp(log(S[tau])*u + A[tau] + B[tau]*h[tau] )
  
  return(MGF_P)
  
}

## The Caracteristic function  under the physical probability 

FC_P<-function(u, para_h ,tau,ret,tsr,S,steps){
  # Complex number : 
  u_Complex=1i*u 
  
  FC_P= MGF_P(u, para_h ,tau,ret,tsr,S,steps)
  
  return(FC_P)
  
}

###### 2- Under the risk neutral probability:
MGF_Q<-function(u, para_h,para_h_star,tau,ret,tsr,S,steps){
  
  ## tsr     #  Taux d'interet sans risque
  ## ret     #  returns,
  ## steps<-data$ttm   #### Transforms the time to maturity expressed in terms of years in terms of days
  ## S<- data$SJ       #### Prix du sous-jacent: data$SJ
  
  r<- tsr
    
  #  The volatility under the risk neutral probability
  
  h_star<- h_star(para_h,para_h_star,ret,r) # Obtain from updating rule
  
  # Terminal condition for the A and B at time T
  A_Q_T=0
  B_Q_T=0
  A_Q=c()
  B_Q=c()
  
  #  A and B a time T-1
  
  A_Q[1]= r[steps]*u 
  B_Q[1]= u*(para_h[5]/para_h_star[1])+ ((para_h_star[2])^(-2))*( 1- sqrt(1-2*u*para_h_star[2])) 
  
  # Recursion back to time t
  
  for (i in 2:steps){
    A_Q[i]= A_Q[i-1]+ r[steps-i+1]*u + para_h[1]*para_h_star[1]*B_Q[i-1]-(1/2)*log(1-2*(para_h[6]/para_h_star[1])*para_h[4]*(para_h_star[2]^3)*B_Q[i-1])
    B_Q[i]= para_h[2]*B_Q[i-1]+u*(para_h[5]/para_h_star[1])+ ((para_h_star[2])^(-2))*( 1- sqrt((1-2*(para_h[6]/para_h_star[1])*para_h[4]*(para_h_star[2]^3)*B_Q[i-1])*(1-2*para_h[3]*para_h_star[1]*(para_h_star[2]/para_h[4])*B_Q[i-1]-2*u*para_h_star[2]))) 
  }
  
  MGF_Q = exp(log (S[tau])*u + A_Q[tau] + B_Q[tau]*h_star[tau] )
  
  return(MGF_Q)
  
}

# The Caracteristic function  under the risk neutral probability 
FC_Q<-function(u, para_h,para_h_star,tau,ret,tsr,S,steps){
  # Complex number : 
  u_Complex=1i*u  
  
  FC_Q= MGF_Q(u, para_h,para_h_star,tau,ret,tsr,S,steps)
  
  return(FC_Q)
  
}


######  Step 3 : Option pricing using FFT
###############################################################
######  The values of the call option from the model using FFT
###############################################################


Price_fft<-function(para_h,para_h_star,tau,ret,tsr,S,steps,K){
  
  r<- tsr          # Interest rate
  T= steps         # Time of maturity  transforms the time to maturity expressed in terms of years in terms of days
  Strike_k         # Strike  Prix d'exercice: data$strike
  
  N=2^10           # Number of subdivision in [0,a]
  # a=              # limite values
  alpha=2           # alpha is the parameter to make C square-integrable 
  delta= 0.25       # delta= a/N  where a is the up value of w (w in [0,a])
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
    phi=FC_Q(w_FC, para_h,para_h_star,tau,ret,tsr,S,steps)
    phi=phi*exp(-r*(T-tau))
    phi=phi/(alpha^2+alpha-w^2+1i*(2*alpha+1)*w)
    phi=phi*exp(1i*w*b)
    res=rbind(res,phi)
  }  
  
  option_prices=Re(fft(res))*exp(-alpha*(-b+(k-1)*lambda))/pi
  
  #lookup for the strikes of interest
  
  K=Strike_k 
  result=c() 
  
  for (i in 1:length(K)){
    index=which(strike<=K[i])
    index=index[length(index)]
    result=rbind(result,option_prices[index])
  }  
  
  return(result)
} 




######  Step 4 : Option pricing using FFT
###############################################################
######  The values of the call option from the model using FFT
###############################################################
## Black-Scholes Function for the marcket price 
###############################################################

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

###############################################################
## The implied volatility
###############################################################
## Function to find BS Implied Vol using Bisection Method
implied.vol <-   function(S, K, T, r, C, type){
  sig <- 0.20
  sig.up <- 1
  sig.down <- 0.001
  count <- 0
  C_market <- C
  err <- C_BS(S, K, T, r, sig, type) - C_market 
  
  ## repeat until error is sufficiently small or counter hits 1000
  while(abs(err) > 0.00001 && count<1000){
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
  if(count==1000){
    return(NA)
  }else{
    return(sig)
  }
}

implied.vol(100,110,1,.05,25,type="C")

###############################################################
## To compute vega, it's a fairly straightforward computation.
###############################################################
vega_call<-function(S, K,Tau,S,K,r,div,price_C){ 
  C<- price_C                               ## Prix du call: data$prix, Call_market price
  sig<- implied.vol(S, K, T, r, C, type)    ## Function to find BS Implied Vol using Bisection Method
  d<-div                                         ## dividende: data$div
    
  d1 <- (log(S/K) + (r-d + sig^2/2)*Tau) / (sig*sqrt(Tau))
  vega_c <-(1.0/sqrt(2*pi))*(S*exp(-r*Tau))*(exp(-((d1^2))))*sqrt(Tau) 
  return(vega_c)
}

###############################################################
# Function that returns Root Mean Squared Error
###############################################################

rmse <- function(para_h,para_h_star,ret,data)
{  

  price_C <- data$prix     #### Prix du call: data$prix .... Call_market price
  Strike_k <-data$strike   #### Prix d'exercice: data$strike
  T<-data$ttm      #### Time to maturity: data$ttm
  S<- data$SJ       #### Prix du sous-jacent: data$SJ
  r<- data$tsr      #### Taux d'interet sans risque: data$tsr
  div_d<-data$div      #### dividende: data$div
  
  
  Call_Market<- price_C                                                    ## Prix du call: data$prix, Call_market price
  Call_fft_model<- Price_fft(para_h,para_h_star,tau,ret,r,S,T,Strike_k)     ## Prix du call: from fft Call_model price
  vega_call<- vega_call(S, K,Tau,S,K,r,div)                                ## Vega of the call
  
  error<- (Call_fft_model - Call_Market)/vega_call
    rmse<-sqrt(mean(error^2))
  return(rmse)
}


#######################################################################
#NLS estimation
#######################################################################
NLS_estim=optim(para,rmse,ret=ret, data=data,method="L-BFGS-B",lower=c(-Inf,0,0,0), upper=c(Inf,Inf,1,Inf))

parametres_NLS=NLS_estim$par








  