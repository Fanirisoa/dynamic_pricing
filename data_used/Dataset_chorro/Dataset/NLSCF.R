####################################################
######          The caracterisation function      ##
####################################################
######    Under the risk neutral probability:     ##
####################################################
###### The moment generating function at tau      ##
####################################################

MGF_Q<-function(u,para_h,Data.contract){
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
FC_Q<-function(u, para_h, Data.contract){
  # Complex number : 
  u_Complex=1i*u 
  
  FC_Q=  MGF_Q(u_Complex, para_h=para_h, Data.contract=Data.contract)
  
  return(FC_Q)
  
}

## W<-FC_Q(u=0.002, para_h,Data.contract)

## start.time <- Sys.time()
## FC_Q(u=0.002, para_h ,Data.ret ,Data.contract)
## end.time <- Sys.time()
## time.taken <- end.time - start.time
## time.taken

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
para_h<-c(-1,0.001,0,-0.5,0,-0.5,-0.1,-0.2)

start.time <- Sys.time()
Price_fft(para_h=para_h,Data.contract=Data.contract)
end.time <- Sys.time()
time.taken <- end.time - start.time



