
############################################################################
###    Persistence, Leverage coefficient, Annualized volatility      #######
############################################################################
PLAL<-function(para_h){
  
  # para_h<-c() set up the parameters of the model 
  w=para_h[1]; b=para_h[2]; a=para_h[3];  c= para_h[4]; neta=para_h[5] ; nu=para_h[6]
 
  # Variable of risk neutral
  theta0= (1/2)*((neta)^(-1) - (1/((nu^2)*(neta^3)))*(1 + ((nu^2)*(neta^3))/2)^2)
  neta0=neta/(1 - 2*neta*theta0)
  PI=(neta0/neta)^(3/2)
  w0=w*(neta0/neta)^(3/2); b0=b; a0=a*(neta0/neta)^(-5/2);  c0=c*(neta0/neta)^(5/2)
  
  # The volatility under the physical probability
  nu0=(1/(neta0^2))*(((1-2*neta0)^(1/2))-1)
  
  PI=(neta0/neta)^(3/2) 
  P=(a0*((neta0)^2))+b0+(c0/((neta0)^2))
  L=(c0/neta0)- ((neta0)^3)*a0
  
  return(list(PI,P,L)) 
}


PLAL(para_h1)

