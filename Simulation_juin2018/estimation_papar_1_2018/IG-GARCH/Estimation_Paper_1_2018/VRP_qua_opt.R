 #####################################################
###         Parameters of the model           #######
#####################################################
#w=para_h[1]; b=para_h[2]; a=para_h[3];  c= para_h[4]; neta=para_h[5] ; nu=para_h[6] ; PI=para_h[7]  


para_h_qua_opt<-c( 6.4945e-06 , 5.4045e-01  ,3.6359e+02,  1.1031e-05, -5.0328e-03 , 1.9460e+02  ,1.2453e+00 )




###########################################################
#####   Conditional variance with risk netral Proba    ####
###########################################################

h_p_qua_opt <- function(para_h_qua_opt, Data.returns) {
  ret=Data.returns$ret   
  # rt=Data.returns$rt/250     
  rt=(0.05+ Data.returns$rt - Data.returns$rt)    
  Z1=length(rt)
  
  para_h=para_h_qua_opt
  ## set up the parameters of the model : para_h
  w=para_h[1]; b=para_h[2]; a=para_h[3];  c= para_h[4]; neta=para_h[5] ; nu=para_h[6] ; PI=para_h[7] 
  
  h = c()                                                        ####  A vector containing h from the model,
  h[1]=(w + a*(neta^4))/(1 - a*(neta^2) - b - (c*(neta^(-2))))   ####  The first value for h, Unconditional Variance
  
  for (i in 2:Z1){
    h[i]=w+b*h[i-1]+ c*(neta^(-1))*(ret[i-1]-rt[i-1]-(nu*h[i-1]))+((a*neta*(h[i-1])^2)/(ret[i-1]-rt[i-1]-(nu*h[i-1])))
  }
  
     
  drapeau=0
  
  if (w<=0){drapeau=1}
  if (b<=0){drapeau=1}
  if (a<=0){drapeau=1}
  if (c<=0){drapeau=1}
  if (nu<=0){drapeau=1}
  if (neta>=0){drapeau=1}

  
  if (drapeau==0){
    resultat=h*10
  }else{
    resultat=rep(NA, Z1)
  }
  
  return(resultat)  
}

##############################################################
######     Conditional variance with risk netral Proba      ##
##############################################################

####################################################
##### Real cube root of a negative number in R    ##
####################################################
cbrt <- function(x) {
  sign(x) * abs(x)^(1/3)
}

####################################################
######         The volatility updating rule       ##
####################################################
h_q_qua_opt<-function(para_h_qua_opt,Data.ret){
  # rt=Data.returns$rt/250     
  rt=(0.05+ Data.returns$rt - Data.returns$rt)     
  ret =Data.ret$ret         #### Returns : Data.BSJ$ret
  Z1=length(ret)
  
  para_h=para_h_qua_opt
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
    resultat=h_star*10
  }else{
    resultat=rep(NA, Z1)
  }
  return(resultat)
}


##############################################################
######     Conditional variance with risk netral Proba      ##
##############################################################
VRP_qua_opt<-function(para_h_qua_opt,Data.returns){
  # rt=Data.returns$rt/250     
  rt=(0.05+ Data.returns$rt - Data.returns$rt)     
  ret=Data.returns$ret         #### Returns : Data.BSJ$ret
  Z1=length(ret)
  
  
  para_h=para_h_qua_opt
  # para_h<-c() set up the parameters of the model 
  w=para_h[1]; b=para_h[2]; a=para_h[3];  c= para_h[4]; neta=para_h[5] ; nu=para_h[6]  ; PI=para_h[7] 
  
  
  # Variable of risk neutral
  neta0=cbrt(((PI/nu)^2)*(-1+ sqrt(1+(8*nu)/(27*PI)))) + cbrt(((PI/nu)^2)*(-1-sqrt(1+(8*nu)/(27*PI)))) 
  nu0= nu/PI
  w0=w*PI; b0=b; a0=(a*neta)/(neta0*PI);  c0=(c*neta0*PI)/neta
  
    
  h_1=h_p_qua_opt(para_h_qua_opt,Data.returns)
  h_2=h_q_qua_opt(para_h_qua_opt,Data.returns)
  
  
  
  VRP = c()                                                                ####  A vector containing h from the model,
  VRP[1]=  sqrt(h_1[1])-sqrt(h_2[1])                ####  The first value for VRP,
  for (i in 2:Z1){
    VRP[i]=  sqrt(h_1[i])-sqrt(h_2[i]) 
  }
  
  drapeau=0
  
  if (w<=0){drapeau=1}
  if (b<=0){drapeau=1}
  if (a<=0){drapeau=1}
  if (c<=0){drapeau=1}
  if (nu<=0){drapeau=1}
  if (neta>=0){drapeau=1}

  
  if (drapeau==0){
    resultat=(-VRP)
  }else{
    resultat=rep(NA, Z1)
  }
  return(resultat)
}
