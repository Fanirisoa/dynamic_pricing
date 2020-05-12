
############################################################################
###    Persistence, Leverage coefficient, Annualized volatility      #######
############################################################################
PLAL<-function(para_h){
  
  a0=para_h[1]; a1=para_h[2]; gama=para_h[3];  b1= para_h[4] ;  lamda0= para_h[5]   
  
  # Parameter under the physical probability
  lamdastar0= -(1/2)
  
  gamastar=gama+lamda0 +0.5
  
  
  Pstar= b1 + a1*(gamastar)^2
  
  L=gamastar
  
  return(list(Pstar,L)) 
}


PLAL(para_h1)



# ############################################################################
# ###    Persistence, Leverage coefficient, Annualized volatility      #######
# ############################################################################
# PLAL<-function(para_h){
#   
#   # para_h<-c() set up the parameters of the model 
#   a0=para_h[1]; a1=para_h[2]; gamastar=para_h[3];  b1= para_h[4]
#   
#   
#   Pstar= b1 + a1*(gamastar)^2
#   
#   L=gamastar
#   
#   return(list(Pstar,L)) 
# }
# 
# 
# PLAL(para_h1)
