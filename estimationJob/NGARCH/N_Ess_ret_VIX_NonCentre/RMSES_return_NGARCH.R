####################################################
######         The volatility updating rule       ##
####################################################
#ht<-function(para_h,Data.ret){
#  rt=Data.ret$rt/250        #### Interest rate Data : Data.BSJ$rt
# ret =Data.ret$ret         #### Returns : Data.BSJ$ret
#  Z1=length(ret)
#  
#  # para_h<-c() set up the parameters of the model 
#  a0=para_h[1]; b1=para_h[2]; a1=para_h[3];  gama= para_h[4]; lambda= para_h[5]; a=para_h[6]; b=para_h[7] ## ; c=para_h[5]; d=para_h[6] 
#  
#  ## Mean 0 and Variance 1
#  c0=a^2 - b^2
#  c=((sqrt(a^2 - b^2))^(3/2))/a
#  d=(-b/a)*(sqrt(a^2 - b^2))^(1/2)
#  
#  
#  # Parameter under the physical probability
#  h0=a0/(1- (b1+a1*(1+gama^2)))    
#  b0=abs(b)
#  
#  
#  for (i in 2:Z1){
#    h[i]=a0 +b1*h+a1*h*((ret-rt- lambda*sqrt(h)+K_eps(sqrt(h),a,b,c,d))/(sqrt(h))-gama)^2
#  }
# #  
#   
#   drapeau=0
#   if (a0<=0){drapeau=1}
#   if (b1<=0){drapeau=1}
#   if (a1<=0){drapeau=1}
#   
#   if (b0<=0){drapeau=1}
#   if (c<=0){drapeau=1}
#   if (c0<=0){drapeau=1}
#   if (a<=0){drapeau=1}
#   
#   if (is.na(b0)==TRUE){drapeau=1}else{
#     if (b0<=0){drapeau=1}
#     if (b0>=a){drapeau=1}
#     if (b0==Inf){drapeau=1}
#     if (1/b0==Inf){drapeau=1}
#   }
#   
#   if (is.na(h0)==TRUE){drapeau=1}else{
#     if (h0<=0){drapeau=1}
#     if (abs(h0)==Inf){drapeau=1}
#     if (1/abs(h0)==Inf){drapeau=1}
#   }
#   
#   if (is.na(c)==TRUE){drapeau=1}else{
#     if (c<=0){drapeau=1}
#     if (abs(c)==Inf){drapeau=1}
#     if (1/abs(c)==Inf){drapeau=1}
#   }
#   
#   if (drapeau==0){
#     resultat=h
#   }else{
#     resultat=rep(NA, Z1)
#   }
#   return(resultat)
# }

####################################################
######  Step 4 : Compution the RMSR               ##
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


RMSEsim <- function(para_h1 ,Data.N)
{  
  C=Data.N$C       ####  Call price
  P<-Pricer(N,para_h1,Data.N)$P
  V<-Vega(Data.N=Data.N, type="C")
  error <- rep(NA, length(C))
  for (i in 1:length(C)){
    error[i] = ((P[i]  -  C[i])/V[i])^2
  }
  rmse<-sqrt((mean(error)))
  return(list(rmse=rmse,P=P)) 
}
