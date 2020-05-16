rm(list=ls())
gc()
library(zoo)
setwd("C:/Users/Leacalin/Desktop/Simulation octobre chorro") 

#######################################
#Data : SP500
#######################################
################# The data ##################################
SP500 = read.table("SP500.csv",header=TRUE,sep=";")
SP500 = as.numeric(SP500[,2])

################# The Log return ##################################
ret0_SP500 = diff(log(SP500))

################# The rolling annual returns ##################################
ret2_SP500 <- rollsum(ret0_SP500,356)
n<-length(ret2_SP500)
n

################# The Histogram and the density of the rolling ##################################
hist(ret2_SP500, main="Histogram of rolling annual return of S&P 500",xlab="S&P 500 Index Returns",prob=TRUE)
plot(density(ret2_SP500),xlim=c(-5,5),main="Density of rolling annual return of S&P 500",xlab="S&P 500 Index Returns",ylab="Frequency")


#######################################
#Application sur SP500
#######################################
rend <- ret2_SP500 
plot(density(rend))
X<-rend

######  Step 1 : The volatility updating rule
###### 1- Under the historical probability:

h<-function(para_h){
  h = c()     #  A vector containing h from the model 
  h[1]=h1     # The first value for h 
  
  #para_h<-c() set up the parameters of the model para_h[1]=w, para_h[2]=b, para_h[3]=c, para_h[4]=neta, para_h[5]=nu, para_h[6]=a
    
  for (i in 2:length(X)){
    h[i]=para_h[1]+ (para_h[2]*h[i-1])+ ((para_h[3]/para_h[4])*(X[i-1] - r - (para_h[5]*h[i-1])))+((para_h[6]*para_h[4])*(h[i-1])^2)/(X[i-1] - r- (para_h[5]*h[i-1]))
  }
  
return(h)
}

para_h<-c(0.1,0.2,0.3,0.4,0.5,0.7)

h(para_h,0.25,sd(X))

###### 2- Under the risk neutral probability:

h_star<-function(para_h,para_h_star,r,hstar1){
  h_star = c() #  A vector containing h_star from the model 
  
  h_star[1]=sd(X) # The first value for h_star 
  
  #para_h_star<-c()  set up the parameters of the model para_h_star[1]=Pi, para_h_star[2]=neta_star
  
  
  for (i in 2:length(X)){
    h_star[i]=para_h[1]*para_h_star[1]+ (para_h[2]*h_star[i-1])+ ((para_h[3]*para_h_star[1]/para_h[4])*(X[i-1] - r- ((para_h[5]/para_h_star[1])*h_star[i-1])))+((para_h[6]*para_h[4]/para_h_star[1])*(h_star[i-1])^2)/(X[i-1] - r- ((para_h[5]/para_h_star[1])*h_star[i-1]))
  }
  return(h_star)
}

para_h_star<-0.9
h_star(para_h,para_h_star,0.25,sd(X))

######  Step 2 :The caracterisation function using the moment generating function
##########################################################
###### 1- Under the historical probability:
##########################################################
# The moment generating function  under the physical probability 
MGF_P<-function(u, para_h ,tau,r,h1){
  
  #  The volatility under the physical probability
  
  h <- h(para_h,r,h1)  # Obtain from updating rule
    
  # Terminal condition for the A and B at time T
  A_T=0
  B_T=0
  A=c()
  B=c()
  
  #  A and B a time T-1
  
  A[1]= r*u 
  B[1]= u*para_h[5]+ ((para_h[4])^(-2))*( 1- sqrt(1-2*u*para_h[4])) 
  
  
  # Recursion back to time t
  
  for (i in 2:length(X)){
    A[i]= A[i-1]+ r*u + para_h[1]*B[i-1]-(1/2)*log(1-2*para_h[6]*(para_h[4]^4)*B[i-1])
    B[i]= para_h[2]*B[i-1]+u*para_h[5]+ (para_h[4]^(-2))*( 1- sqrt((1-2*para_h[6]*(para_h[4]^4)*B[i-1])*(1-2*para_h[3]*B[i-1]-2*u*para_h[4]))) 
  }
  
  MGF_P = exp(log (X[tau])*u + A[tau] + B[tau]*h[tau] )
  
  return(MGF_P)
  
}

MGF_P(1,c(1,0.2,0.3,-0.5,0.5,-0.2) ,1,0.25,sd(X))


## The Caracteristic function  under the physical probability 

FC_P<-function(u, para_h,tau,r,h1){
  # Complex number : 
  u_Complex=1i*u 
  
  FC_P= MGF_P(u_Complex,para_h,tau,r,h1)
  
  return(FC_P)
  
}

FC_P(1,c(1,0.2,0.3,-0.5,0.5,-0.2) ,1,0.25,sd(X))

###### 2- Under the risk neutral probability:
MGF_Q<-function(u, para_h, para_h_star, tau, r, hstar1){
  
  #  The volatility under the risk neutral probability
  
  h_star<- h_star(para_h,para_h_star,r,hstar1)  # Obtain from updating rule
  
  # Terminal condition for the A and B at time T
  A_Q_T=0
  B_Q_T=0
  A_Q=c()
  B_Q=c()
  
  #  A and B a time T-1
  
  A_Q[1]= r*u 
  B_Q[1]= u*(para_h[5]/para_h_star[1])+ ((para_h_star[2])^(-2))*( 1- sqrt(1-2*u*para_h_star[2])) 
  
  # Recursion back to time t
  
  for (i in 2:length(X)){
    A_Q[i]= A_Q[i-1]+ r*u + para_h[1]*para_h_star[1]*B_Q[i-1]-(1/2)*log(1-2*(para_h[6]/para_h_star[1])*para_h[4]*(para_h_star[2]^3)*B_Q[i-1])
    B_Q[i]= para_h[2]*B_Q[i-1]+u*(para_h[5]/para_h_star[1])+ ((para_h_star[2])^(-2))*( 1- sqrt((1-2*(para_h[6]/para_h_star[1])*para_h[4]*(para_h_star[2]^3)*B_Q[i-1])*(1-2*para_h[3]*para_h_star[1]*(para_h_star[2]/para_h[4])*B_Q[i-1]-2*u*para_h_star[2]))) 
  }
  
  MGF_Q = exp(log (X[tau])*u + A_Q[tau] + B_Q[tau]*h_star[tau] )
  
  return(MGF_Q)
  
}

MGF_Q(1,c(1,0.2,0.3,-0.5,0.5,-0.2),c(0.1,0.2) ,1,0.25,sd(X))

# The Caracteristic function  under the risk neutral probability 
FC_Q<-function(u, para_h,para_h_star,tau, r, hstar1){
  # Complex number : 
  u_Complex=1i*u  
  
  FC_Q= MGF_Q(u_Complex,para_h,para_h_star, tau, r, hstar1)
  
  return(FC_Q)
  
}

FC_Q(1,c(1,0.2,0.3,-0.5,0.5,-0.2),c(0.1,0.2) ,1,0.25,sd(X))

######  Step 3 : Option pricing using FFT
###############################################################
######  The values of the call option from the model using FFT
###############################################################


Price_fft<-function(para_h, para_h_star,tau,r,hstar1){
  
  T= 2*250          # Time of maturity  transforms the time to maturity expressed in terms of years in terms of days
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
    phi=FC_Q(w_FC, para_h, para_h_star, tau,r,hstar1)
    phi=phi*exp(-r*T)
    phi=phi/(alpha^2+alpha-w^2+1i*(2*alpha+1)*w)
    phi=phi*exp(1i*w*b)
    res=rbind(res,phi)
  }  
  
  option_prices=Re(fft(res))*exp(-alpha*(-b+(k-1)*lambda))/pi
  
  #lookup for the strikes of interest
  
  # K
  K=c(4,6)
  result=c() 
  
  for (i in 1:length(K)){
    index=which(strike<=K[i])
    index=index[length(index)]
    result=rbind(result,option_prices[index])
  }  
  
  return(result)
} 


Price_fft(c(1,0.2,0.3,-0.5,0.5,-0.2),c(0.1,0.2) ,1,0.25,sd(X))

# To measure function execution time

system.time(Price_fft(c(1,0.2,0.3,-0.5,0.5,-0.2),c(0.1,0.2) ,1,0.25,sd(X)))   

start.time <- Sys.time()
Price_fft(c(1,0.2,0.3,-0.5,0.5,-0.2),c(0.1,0.2) ,1,0.25,sd(X))
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken



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
implied.vol <-   function(S, K, T, r, market, type){
  sig <- 0.20
  sig.up <- 1
  sig.down <- 0.001
  count <- 0
  err <- BS(S, K, T, r, sig, type) - market 
  
  ## repeat until error is sufficiently small or counter hits 1000
  while(abs(err) > 0.00001 && count<1000){
    if(err < 0){
      sig.down <- sig
      sig <- (sig.up + sig)/2
    }else{
      sig.up <- sig
      sig <- (sig.down + sig)/2
    }
    err <- BS(S, K, T, r, sig, type) - market
    count <- count + 1
  }
  
  ## return NA if counter hit 1000
  if(count==1000){
    return(NA)
  }else{
    return(sig)
  }
}

###############################################################
## To compute vega, it's a fairly straightforward computation.
###############################################################
# q dividend
vega_call<-function(S, K,Tau,sig,r,q){ 
  d1 <- (log(S/K) + (r-q + sig^2/2)*Tau) / (sig*sqrt(Tau))
  vega_c <-(1.0/sqrt(2*pi))*(S*exp(-r*Tau))*(exp(-((d1^2))))*sqrt(Tau) 
  return(vega_c)
}

vega_call(0.1, 10,5,0.1,0.2,0.3)

# Using package fOption : 
# GBSGreeks(Selection = "vega", TypeFlag = "c", S = , X = ,
#          Time = , r = , b = , sigma = )

###############################################################
# Function that returns Root Mean Squared Error
###############################################################

rmse <- function(error)
{
  error<- (Call_fft_model - Call_Market)/
  rmse<-sqrt(mean(error^2))
  return(rmse)
}


