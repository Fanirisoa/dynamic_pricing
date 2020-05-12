
############################################################################
###    Persistence, Leverage coefficient, Annualized volatility      #######
############################################################################
PLAL<-function(para_h){
  
  # para_h<-c() set up the parameters of the model 
  a0=para_h[1]; a1=para_h[2]; a2=para_h[3];  b1= para_h[4] ;  lamda0= para_h[5]  ; ro=para_h[6]
  

  P=  b1 +a1+a2/2
  Pstar= b1+ (a1+a2*(pnorm(lamda0)))*(1+lamda0^2)+a2*lamda0*dnorm(lamda0) 
  

  return(list(P,Pstar)) 
}


PLAL(para_h1)
