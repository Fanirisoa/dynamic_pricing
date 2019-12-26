####################################################
##### Real cube root of a negative number in R    ##
####################################################
cbrt <- function(x) {
  sign(x) * abs(x)^(1/3)
}
####################################################
######         The volatility updating rule       ##
####################################################
h<-function(para_h,Data.ret){
  rt=Data.ret$rt/250        #### Interest rate Data : Data.BSJ$rt
  ret =Data.ret$ret         #### Returns : Data.BSJ$ret
  Z1=length(ret)
  
  # para_h<-c() set up the parameters (physical probability) of the model 
  w=para_h[1]; b=para_h[2]; a=para_h[3];  c= para_h[4]; neta=para_h[5] ; nu=para_h[6]  ; PI=para_h[7]  
  
  
  # Variable of risk neutral
  neta0=cbrt(((PI/nu)^2)*(-1+ sqrt(1+(8*nu)/(27*PI)))) + cbrt(((PI/nu)^2)*(-1-sqrt(1+(8*nu)/(27*PI)))) 
  nu0= nu/PI
  w0=w*PI; b0=b; a0=(a*neta)/(neta0*PI);  c0=(c*neta0*PI)/neta
  
  
  
  h_star = c()                                                                ####  A vector containing h from the model,
  h_star[1]=(w0 + a0*(neta0^4))/(1 - a0*(neta0^2) - b0 - (c0*(neta0^(-2))))   ####  The first value for h,
  for (i in 2:Z1){
    h_star[i]=w0+b0*h_star[i-1]+ c0*(neta0^(-1))*(ret[i-1]-rt[i-1]-(nu0*h_star[i-1]))+((a0*neta0*(h_star[i-1])^2)/(ret[i-1]-rt[i-1]-(nu0*h_star[i-1])))
  }
  
  drapeau=0
  if (w<=0){drapeau=1}
  if (b<=0){drapeau=1}
  if (a<=0){drapeau=1}
  if (c<=0){drapeau=1}
  if (neta>=0){drapeau=1}
  if (PI<=1.1){drapeau=1}
  if (nu<=0){drapeau=1}

  
  if (drapeau==0){
    resultat=h_star
  }else{
    resultat=rep(NA, Z1)
  }
  return(resultat)
}

####################################################
######      The caracterisation function          ##
####################################################
######         Intermediate function              ##
####################################################
Fun<-function(para_h,x,r,T,h,S,A_Q ,B_Q ){
  ## set up the parameters of the model : para_h
  w=para_h[1]; b=para_h[2]; a=para_h[3];  c= para_h[4]; neta=para_h[5] ; nu=para_h[6]  ; PI=para_h[7]  
  
  drapeau=0
  if (w<=0){drapeau=1}
  if (b<=0){drapeau=1}
  if (a<=0){drapeau=1}
  if (c<=0){drapeau=1}
  if (neta>=0){drapeau=1}
  if (nu<=0){drapeau=1}
  # if (PI<=0){drapeau=1}
  if (is.na(h)==TRUE){drapeau=1}else{
    if (h<0){drapeau=1}
    if (abs(h)==Inf){drapeau=1}
    if (1/abs(h)==Inf){drapeau=1}
  }
  
  if (drapeau==0){
    resultat= (exp(log(S)*1i*x  + A_Q + B_Q*h))*exp(-r*T*250)
  }else{
    resultat=NA
  }
  return(resultat)
}

####################################################
###### The moment generating function at tau      ##
####################################################
FC_Q<-function(x,para_h,Data.ret ,Data.contract){
  T=Data.contract$T       ####  Time to maturity expressed in terms of years in terms of days
  S=Data.contract$S       ####  Prix du sous-jacent: Data.contract$S
  K=Data.contract$K       ####  Strike  Prix d'exercice: data$strike
  r=Data.contract$r/250   ####  Interest rate Data.contract$r
  Per=Data.contract$Per   ####  
  
  u=1i*x 
  Z=length(S)
  
  # para_h<-c() set up the parameters (physical probability) of the model 
  w=para_h[1]; b=para_h[2]; a=para_h[3];  c= para_h[4]; neta=para_h[5] ; nu=para_h[6]  ; PI=para_h[7]  
  
  
  # Variable of risk neutral
  neta0=cbrt(((PI/nu)^2)*(-1+ sqrt(1+(8*nu)/(27*PI)))) + cbrt(((PI/nu)^2)*(-1-sqrt(1+(8*nu)/(27*PI)))) 
  nu0= nu/PI
  w0=w*PI; b0=b; a0=(a*neta)/(neta0*PI);  c0=(c*neta0*PI)/neta
  
  h<-h(para_h,Data.ret)
  
  # Recursion back to time t
  FC_Q <- rep(NA, Z)
  for (i in 1:Z){
    # Terminal condition for the A and B at time T
    A_Q=0
    B_Q=0
    steps<-round(T[i]*250,0)  #### Time to maturity expressed in terms of years in terms of days
    for (j in 1:steps){
      A_Q= A_Q+ r[i]*u + w0*B_Q-(1/2)*log(1-2*a0*(neta0^4)*B_Q)
      B_Q= b0*B_Q+u*nu0+ (1/neta0^2)*(1-sqrt((1-2*a0*(neta0^4)*B_Q)*( 1- 2*c0*B_Q - 2*u*neta0)))
    }
    FC_Q[i]= Fun(para_h,x,r[i],T[i],h[Per[i]+1],S[i],A_Q ,B_Q )
  }
  return(FC_Q)
}


####################################################
######           Option pricing using FFT         ##
####################################################
######  The call option from the model using FFT  ##
####################################################
Price_fft<-function(para_h,Data.ret,Data.N){
  S=Data.N$S       ####  Prix du sous-jacent: Data.contract$S
  K=Data.N$K       ####  Strike  Prix d'exercice: data$strike
  Mod=Data.N$Mod   ####  Class of equivalence
  
  Data.contract=Data.N
  
  Data.contract.mod=data.frame(S=Data.contract$S,T=Data.contract$T,r=Data.contract$r,Pe=Data.contract$Pe,Per=Data.contract$Per)
  Data.class=unique(Data.contract.mod)
  
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
    phi= ((FC_Q(((delta*(i-1))-(alpha+1)*1i), para_h,Data.ret,Data.class))/(alpha^2+alpha-(delta*(i-1))^2+1i*(2*alpha+1)*(delta*(i-1))))*delta*exp(1i*(delta*(i-1))*b)
    res=rbind(res,phi)
  }  
  
  prix=c()
  for (i in 1:length(K)){
    option_prices=Re(fft(res[,Mod[i]]))*exp(-alpha*(-b+(k-1)*lambda))/pi
    prix[i]=option_prices[(log(K[i])+b)/lambda+1]
  }  
  
  return(prix)
}



####################################################
######  Step 4 : Compution the RMSR               ##r
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
  Norm_b= (1/sqrt((1/length(C))*sum((C)^2)))*100
  
  error <- rep(NA, length(C))
  for (i in 1:length(C)){
    error[i] = ((P[i]  -  C[i])/V[i])^2
  }
  norm_rmse<-Norm_b*sqrt((mean(error)))
  rmse<-sqrt((mean(error)))
  return(list(rmse=rmse,P=P,error=error,norm_rmse=norm_rmse)) 
}






