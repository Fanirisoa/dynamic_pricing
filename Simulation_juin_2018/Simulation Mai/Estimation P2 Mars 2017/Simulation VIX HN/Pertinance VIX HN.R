
############################################################################
###    Persistence, Leverage coefficient, Annualized volatility      #######
############################################################################
PLAL<-function(para_h){
  
  # para_h<-c() set up the parameters of the model 
  a0=para_h[1]; a1=para_h[2]; gama=para_h[3];  b1= para_h[4] ; lamda0= para_h[5] ; ro=para_h[6]
  
  # Parameter under the physical probability
  lamda0star= -(1/2)
  gamastar= gama+lamda0+(1/2)
  

  
  P= b1 + a1*(gama)^2
  Pstar= b1 + a1*(gamastar)^2
  
  L=gama
  
  return(list(P,Pstar,L)) 
}


1568961 * 5
