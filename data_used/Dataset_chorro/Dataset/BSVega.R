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
  
  T<-T/250
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


