rmse1 <- function(Para_h,Data.contract)
{ 
  #### Parameters of the models :
  Para_h<-c()
  #### Para_h<-c() set up the parameters of the model 
  w=para_h[1]
  b=para_h[2]
  c=para_h[3]
  neta= para_h[4]
  nu=para_h[5]
  a=para_h[6]
  
  #### Para_h_star<-c()  set up the parameters of the model 
  Pi=para_h[7]
  neta_star=para_h[8]
  
  #### The volatility under the physical probability
  h=h1
  h_star=h1
  
  #### Required inputs from the contract :  
  T=Data.contract$T       ####  Time to maturity expressed in terms of years in terms of days
  S=Data.contract$S       ####  Prix du sous-jacent: Data.contract$S
  K=Data.contract$K       ####  Strike  Prix d'exercice: data$strike
  r=Data.contract$r       ####  Interest rate Data.contract$r
  C=Data.contract$C       ####  Call price
  d=Data.contract$d       ####  Dividend 
  
  T<-round(T*250,0)
  r1<-r/250
  Z=length(S)
  
  ####  Characteristic function 
  FC_Q<-function(u){
    #### Recursion back to time t
    FC_Q <- rep(NA, Z)
    for (i in 1:Z){
    #### Terminal condition for the A and B at time T
      A_Q=0
      B_Q=0
      steps<-T[i]  #### Time to maturity expressed in terms of years in terms of days
      for (j in 1:steps){
        A_Q= A_Q+ r1[i]*u + w*Pi*B_Q-(1/2)*log(1-2*(a/Pi)*neta*(neta_star^3)*B_Q)
        B_Q= b*B_Q+u*(nu/Pi)+ ((neta_star)^(-2))*( 1- sqrt((1-2*(a/Pi)*neta*(neta_star^3)*B_Q)*(1-2*c*Pi*(neta_star/neta)*B_Q-2*u*neta_star))) 
      }
      FC_Q[i]= exp(log(S[i])*u + A_Q + B_Q*h_star )
    }
  }
   
  #### Implement the FFT   : Price call using FFT algorithm
  
  N=2^10           # Number of subdivision in [0,a]
  alpha=2          # alpha is the parameter to make C square-integrable 
  delta= 0.25      # delta= a/N  where a is the up value of w (w in [0,a])
  lambda=(2*pi)/(N*delta)
  
  j=seq(1,N,1)
  k=seq(1,N,1)
  b=(lambda*N)/2
  strike= -b+(k-1)*lambda
  strike= exp(strike)
  
  ####  Initialize the price vector;
  CallFFT=c()
  
  for (i in 1:N){
    w=delta*(i-1)      #  w= j*delta but from 1 to N so w=(i-1)*delta
    w_FC=w-(alpha+1)*1i
    phi= FC_Q(w_FC)
    phi=phi*exp(-r1*(T))
    phi=phi/(alpha^2+alpha-w^2+1i*(2*alpha+1)*w)
    phi=phi*exp(1i*w*b)
    CallFFT=rbind(CallFFT,phi)
  }  
  
  option_prices=Re(fft(CallFFT))*exp(-alpha*(-b+(k-1)*lambda))/pi
  
  #### Lookup for the strikes of interest
  Price=c() 
  
  for (i in 1:length(K)){
    index=which(strike<=K[i])
    index=index[length(index)]
    Price=rbind(Price,option_prices[index])
  }  
  
  
  ####   Black-Scholes Function for call       
  C_BS <-  function(sig){
    d1 <- (log(S/K) + (r + sig^2/2)*T) / (sig*sqrt(T))
    d2 <- d1 - sig*sqrt(T)
    C_BS <- S*pnorm(d1) - K*exp(-r*T)*pnorm(d2)
    }
  
  
   ####   BS Implied Vol using Bisection Method     
    Sigma <- rep(NA, length(C))
    for (i in 1:length(C)){
      sig <- 0.20
      sig.up <- 1
      sig.down <- 0.001
      count <- 0
      err <- c()
      err<- C_BS(sig=sig)- C
      ## repeat until error is sufficiently small or counter hits 1000
      while(abs(err[i]) > 0.00001 && count<100000){
        if(err [i]< 0){
          sig.down <- sig
          sig <- (sig.up + sig)/2
        }else{
          sig.up <- sig
          sig <- (sig.down + sig)/2
        }
        err[i]<- C_BS(sig=sig)[i]- C[i]
        count <- count + 1
      }
      
      ## return NA if counter hit 1000
      if(count==100000){
        return(NA)
      }else{
        Sigma[i]=sig
      }
    }

   
  ####  To compute vega                          
  d1=rep(NA, Z)
  for (i in 1:Z){
    d1[i]= (log(S[i]/K[i]) + (r[i]-d[i] + Sigma[i]^2/2)*T[i]) / (Sigma[i]*sqrt(T[i]))    
  }
  Vega=rep(NA, Z)
  for (i in 1:Z){
  Vega[i]=  (1.0/sqrt(2*pi))*(S[i]*exp(-r*T[i]))*(exp(-((d1[i]^2))))*sqrt(T[i]) 
  }
  
  error <- rep(NA, Z)
  for (i in 1:Z){
    error[i] = ((Price[i]  -  C[i])/Vega[i])^2
  }
  
  rmse=100*sqrt(mean(error))
  return(rmse)
  
}
para_h<-c(-1,0.001,0,-0.5,0,-0.5,-0.1,-0.2)
rmse1(Para_h,Data.contract)
  


