
############################################################################
###    Persistence, Leverage coefficient, Annualized volatility      #######
############################################################################
PLAL<-function(para_h){
  
  # para_h<-c() set up the parameters (physical probability) of the model 
  w=para_h[1]; b=para_h[2]; a=para_h[3];  c= para_h[4]; neta=para_h[5] ; nu=para_h[6]  ; PI=para_h[7] ; ro=para_h[8]
  
  
  # Variable of risk neutral
  neta0=cbrt(((PI/nu)^2)*(-1+ sqrt(1+(8*nu)/(27*PI)))) + cbrt(((PI/nu)^2)*(-1-sqrt(1+(8*nu)/(27*PI)))) 
  nu0= nu/PI
  w0=w*PI; b0=b; a0=(a*neta)/(neta0*PI);  c0=(c*neta0*PI)/neta
  
  
  PI=para_h[7] 
  P=(a0*((neta0)^2))+b0+(c0/((neta0)^2))
  L=(c0/neta0)- ((neta0)^3)*a0
  
  return(list(PI,P,L)) 
}


PLAL(para_h1)
