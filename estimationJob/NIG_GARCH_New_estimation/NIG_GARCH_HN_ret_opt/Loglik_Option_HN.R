####################################################
######         The volatility updating rule       ##
####################################################
fsqrt <- function(para_h,ret,h,rt)
{
  ## set up the parameters of the model : para_h
  a0=para_h[1]; a1=para_h[2]; gama=para_h[3];  b1= para_h[4] ;  lamda0= para_h[5]   
  
  # Parameter under the physical probability
  lamda0star= -(1/2)
  gamastar= gama+lamda0+(1/2)
  
  
  h0=(a0 + a1)/(1 - b1 - a1*(gamastar)^2)    
  
  drapeau=0
  if (a0<=0){drapeau=1}
  if (a1<=0){drapeau=1}
  if (gama<=0){drapeau=1}
  if (b1<=0){drapeau=1}
  if (lamda0<=0){drapeau=1}
  
  if (is.na(h0)==TRUE){drapeau=1}else{
    if (h0<=0){drapeau=1}
    if (abs(h0)==Inf){drapeau=1}
    if (1/abs(h0)==Inf){drapeau=1}
  }
  
  if (is.na(h)==TRUE){drapeau=1}else{
    if (h<=0){drapeau=1}
    if (abs(h)==Inf){drapeau=1}
    if (1/abs(h)==Inf){drapeau=1}
  }
  
  if (drapeau==0){
    resultat= a0 +b1*h+a1*(((ret-rt-lamda0star*(h))/(sqrt(h))) - gamastar*(sqrt(h)))^2
  }else{
    resultat=NA
  }
  return(resultat)
}


h<-function(para_h,Data.ret){
  rt=Data.ret$rt/250        #### Interest rate Data : Data.BSJ$rt
  ret =Data.ret$ret         #### Returns : Data.BSJ$ret
  Z1=length(ret)
  
  # para_h<-c() set up the parameters of the model 
  a0=para_h[1]; a1=para_h[2]; gama=para_h[3];  b1= para_h[4] ;  lamda0= para_h[4] 
  
  # Parameter under the physical probability
  lamda0star= -(1/2)
  gamastar= gama+lamda0+(1/2)
  
  h_star = c()                                                        ####  A vector containing h from the model,
  h_star[1]=(a0 + a1)/(1 - b1 - a1*(gamastar)^2 )                     ####  The first value for h,
  for (i in 2:Z1){
    h_star[i]=  fsqrt(para_h,ret[i-1],h_star[i-1],rt[i-1]) ####  a0 +b1*h_star[i-1]+a1*(((ret[i-1]-rt[i-1]-lamda0star*(h_star[i-1]))/(sqrt(h_star[i-1]))) - gamastar*(sqrt(h_star[i-1])))^2
    
  }
  fsqrt(para_h,ret[i-1],h_star[i-1],rt[i-1]) ####  a0 +b1*h_star[i-1]+a1*(((ret[i-1]-rt[i-1]-lamda0star*(h_star[i-1]))/(sqrt(h_star[i-1]))) - gamastar*(sqrt(h_star[i-1])))^2
  
  drapeau=0
  
  if (a0<=0){drapeau=1}
  if (a1<=0){drapeau=1}
  if (gama<=0){drapeau=1}
  if (b1<=0){drapeau=1}
  if (lamda0<=0){drapeau=1}
  
  if (drapeau==0){
    resultat=h_star
  }else{
    resultat=rep(NA, Z1)
  }
  return(resultat)
}

####################################################
######   Computation of the Vega                  ##
####################################################
######   Black-Scholes Function for call          ##
####################################################
C_BS <-  function(S, K, T, r, sig,d, type="C"){
  d1 <- (log(S/K) + (r -d + sig^2/2)*T) / (sig*sqrt(T))
  d2 <- d1 - sig*sqrt(T)
  if(type=="C"){
    value <- S*pnorm(d1) - K*exp(-r*T)*pnorm(d2)
  }
  if(type=="P"){
    value <- K*exp(-r*T)*pnorm(-d2) - S*pnorm(-d1)
  }
  return(value)
}
####################################################
######   BS Implied Vol using Bisection Method    ##
####################################################
implied.vol <-   function(S, K, T, r, C,d, type="C"){
  sig <- 0.20
  sig.up <- 1
  sig.down <- 0.000001
  count <- 0
  C_market <- C
  err <- C_BS(S, K, T, r, sig,0 ,type="C") - C_market 
  
  ## repeat until error is sufficiently small or counter hits 1000
  while(abs(err) > 0.000001 && count<10000){
    if(err < 0){
      sig.down <- sig
      sig <- (sig.up + sig)/2
    }else{
      sig.up <- sig
      sig <- (sig.down + sig)/2
    }
    err <- C_BS(S, K, T, r, sig,0, type) - C_market
    count <- count + 1
  }
  
  ## return NA if counter hit 1000
  if(count==10000){
    return(-1)
  }else{
    return(sig)
  }
}


####################################################
######   To compute vega                          ##
####################################################
V<-function(S, K, T, r, C,d, type="C"){ 
  sig<-implied.vol(S, K, T, r, C,d, type="C")    ## Function to find BS Implied Vol using Bisection Method
  d1 <- (log(S/K) + (r-d + sig^2/2)*T) / (sig*sqrt(T))
  if(sig==-1){
    return(V=10^6)
  }else{
    return(V=(1.0/sqrt(2*pi))*(S*exp(-r*T))*(exp(-((d1^2))))*sqrt(T))
  }
}

Vega <- function(Data.N, type="C")
{  
  
  T=Data.N$T*250   ####  Time to maturity expressed in terms of years in terms of days
  S=Data.N$S       ####  Prix du sous-jacent: Data.contract$S
  K=Data.N$K       ####  Strike  Prix d'exercice: data$strike
  r=Data.N$r/250   ####  Interest rate Data.contract$r
  C=Data.N$C       ####  Call price
  d=Data.N$d*0     ####  Call dividende
  
  vega <- rep(NA, length(C))
  for (i in 1:length(C)){
    vega[i] = V(S[i], K[i], T[i], r[i], C[i], d[i], type="C")
  }
  return(vega)
}

############################################################
#### Function that returns Root Mean Squared Error        ##
############################################################
MSE <- function(para_h,Data.ret,Data.N)
{  
  C=Data.N$C       ####  Call price
  P<-Price_fft(para_h=para_h,Data.ret=Data.ret, Data.N=Data.N)
  error <- rep(NA, length(C))
  for (i in 1:length(C)){
    error[i] = (P[i]  -  C[i])^2
  }
  MSE<-sqrt(mean(error))
  return(MSE)
}

RMSE <- function(para_h,Data.ret,Data.N)
{  
  C=Data.N$C       ####  Call price
  P<-Price_fft(para_h=para_h,Data.ret=Data.ret, Data.N=Data.N)
  V<-Vega(Data.N=Data.N, type="C")
  error <- rep(NA, length(C))
  for (i in 1:length(C)){
    error[i] = ((P[i]  -  C[i])/V[i])^2
  }
  rmse<-sqrt((mean(error)))
  return(rmse)
}


############################################################
#### Function that returns Root Mean Squared Error        ##
############################################################


RMSEsim <- function(N,para_M,Data.ret, Data.N)
{  
  C=Data.N$C       ####  Call price
  
  ## set up the parameters of the model : para_M = c(para_distribution,para_h) 
  alpha=para_M[1];  beta=para_M[2];  delta=para_M[3];  mu=para_M[4]                   ## para_distribution<-c() set up the parameters of NIG
  a0=para_M[5]; a1=para_M[6]; gama=para_M[7];  b1= para_M[8] ;  lamda0= para_M[9]     ## para_h<-c() set up the parameters of the model
  
  para_h1 = c(a0,a1,gama,b1,lamda0)
  para_distribution1= c(alpha,beta,delta,mu)
  
  P<-Pricer(N,para_h1,para_distribution1,Data.N)$P
  V<-Vega(Data.N=Data.N, type="C")
  
  Norm_b= (1/sqrt((1/length(C))*sum((C)^2)))*100
  
  error <- rep(NA, length(C))
  for (i in 1:length(C)){
    error[i] = ((P[i]  -  C[i])/V[i])^2
  }
  norm_rmse<-Norm_b*sqrt((mean(error)))
  rmse<-sqrt((mean(error)))
  return(list(rmse=rmse,P=P,error=error,norm_rmse=norm_rmse)) 
}



###########################################################
#####       The Log-likeelihood over all Option        ####
###########################################################
Heston_likelihood_opti <- function(N,para_M,Data.ret, Data.N) {
  
  # alpha=para_distribution[1], beta=para_distribution[2], delta=para_distribution[3], mu=para_distribution[4]
  # a0=para_h[1]; a1=para_h[2]; gama=para_h[3];  b1= para_h[4] ;  lamda0= para_h[5] 
  
  ## set up the parameters of the model : para_M = c(para_distribution,para_h) 
  alpha=para_M[1];  beta=para_M[2];  delta=para_M[3];  mu=para_M[4]                   ## para_distribution<-c() set up the parameters of NIG
  a0=para_M[5]; a1=para_M[6]; gama=para_M[7];  b1= para_M[8] ;  lamda0= para_M[9]     ## para_h<-c() set up the parameters of the model
  
  para_h1 = c(a0,a1,gama,b1,lamda0)
  para_distribution1= c(alpha,beta,delta,mu)

  
  C=Data.N$C       ####  Call dividende
  
  print("ok_1")
  P<-Pricer(N,para_h1,para_distribution1,Data.N)$P
  V<-Vega(Data.N=Data.N, type="C")
  
  print("ok_2")
  
  error <- rep(NA, length(C))
  for (i in 1:length(C)){
    error[i]=((P[i]  -  C[i])/V[i])
  }
  sigma=mean(error^2)
  
  print("ok_3")
  log_like <- -1/2*sum(log(sigma)+((error^2)/sigma))
  return(log_like)  
  
}


