####################################################
######         The volatility updating rule       ##
####################################################
shape_h_P<-function(para_h,Data.ret){
  rt=Data.ret$rt/250        #### Interest rate Data : Data.BSJ$rt
  ret =Data.ret$ret         #### Returns : Data.BSJ$ret
  Z1=length(ret)
  
  # With the parameter a: para_h<-c() set up the parameters of the model 
  # w=para_h[1]; b=para_h[2];  a=para_h[3];  c= para_h[4]; neta=para_h[5]; nu=para_h[6] 
 
  # Without the parameter a: para_h<-c() set up the parameters of the model 
  w=para_h[1]; b=para_h[2];  c= para_h[3]; neta=para_h[4]; nu=para_h[5] 

  # With the parameter a: 
  # h = c()                                                      ####  A vector containing h from the model,
  # [1]=(w + a*(neta^4))/(1 - a*(neta^2) - b - (c*(neta^(-2))))  ####  The first value for h,
  
  # Without the parameter a: 
  h = c()                                                        ####  A vector containing h from the model,
  h[1]=(w)/(1- b - (c*(neta^(-2))))                              ####  The first value for h,
  
  for (i in 2:Z1){
    
    # With the parameter a: 
    # h[i]=w+b*h[i-1]+ c*(neta^(-1))*(ret[i-1]-rt[i-1]-(nu*h[i-1]))+((a*neta*(h[i-1])^2)/(ret[i-1]-rt[i-1]-(nu*h[i-1])))
    
    # Without the parameter a: 
    h[i]=w+b*h[i-1]+ c*(neta^(-1))*(ret[i-1]-rt[i-1]-(nu*h[i-1])) 
    
  }
  
  # With the parameter a: 
  # g0=(a*(neta^2) +b +(c*(neta^(-2))))                            ####  to have h[1] > 0    
  
  # Without the parameter a: 
  g0=(b +(c*(neta^(-2))))                                        ####  to have h[1] > 0      
  
  drapeau=0
  if (w<=0){drapeau=1}
  if (b<=0){drapeau=1}
  if (c<=0){drapeau=1}
  
  # With the parameter a: 
  # if (a<0){drapeau=1}
  
  
  if (neta>=0){drapeau=1}
  if (drapeau==0){
    resultat=h 
  }else{
    resultat=rep(NA, Z1)
  }
  return(resultat)
}



###########################################################
#####  The conditional density of the daily return     ####
###########################################################

Retdensity <- function(para_h,Ret,h,r)
{
  # With the parameter a: para_h<-c() set up the parameters of the model 
  # w=para_h[1]; b=para_h[2];  a=para_h[3];  c= para_h[4]; neta=para_h[5]; nu=para_h[6] 
  
  # Without the parameter a: para_h<-c() set up the parameters of the model 
  w=para_h[1]; b=para_h[2];  c= para_h[3]; neta=para_h[4]; nu=para_h[5] 
  
  
  z1= (Ret-r-nu*h)/neta                         ####  to have h > 0
  z2=2*pi*((Ret-r-nu*h)^3)*neta^(-3)            ####  to have positive value inside sqrt()
 
  # With the parameter a: 
  # g0=(a*(neta^2) +b +(c*(neta^(-2))))         ####  to have h[1] > 0    
  
  # Without the parameter a: 
  g0=(b +(c*(neta^(-2))))                       ####  to have h[1] > 0      
  
  
  drapeau=0
  if (w<=0){drapeau=1}
  if (b<=0){drapeau=1}
  if (c<=0){drapeau=1}

  # With the parameter a: 
  # if (a<0){drapeau=1}
  

  if (neta>=0){drapeau=1}
  if (nu<=0){drapeau=1}

  if (is.na(h)==TRUE){drapeau=1}else{
    if (h<=0){drapeau=1}
    if (abs(h)==Inf){drapeau=1}
    if (1/abs(h)==Inf){drapeau=1}
  }
  if (is.na(z1)==TRUE){drapeau=1}else{
    if (z1<0){drapeau=1}
    if (abs(z1)==Inf){drapeau=1}
    if (1/abs(z1)==Inf){drapeau=1}
  }
  if (is.na(z2)==TRUE){drapeau=1}else{
    if (z2<0){drapeau=1}
    if (abs(z2)==Inf){drapeau=1}
    if (1/abs(z2)==Inf){drapeau=1}
  }

  if (drapeau==0){
    resultat= ((h*abs(neta^(-3)))/(sqrt(2*pi*((Ret-r-nu*h)^3)*neta^(-3))))*exp((-1/2)*(sqrt((Ret-r-nu*h)/neta)- (h/(neta^2))*sqrt(neta/(Ret-r-nu*h)))^2)
  }else{
    resultat=NA
  }
  return(resultat)
}



###########################################################
#####  The Log-likeelihood over all the returns dates  ####
###########################################################

IGGARCH_likelihood_ret <- function(para_h, Data.returns) {
  ret=Data.returns$ret   
  rt=Data.returns$rt/250        
  Z1=length(rt)
  
  # With the parameter a: para_h<-c() set up the parameters of the model 
  # w=para_h[1]; b=para_h[2];  a=para_h[3];  c= para_h[4]; neta=para_h[5]; nu=para_h[6] 
  
  # Without the parameter a: para_h<-c() set up the parameters of the model 
  w=para_h[1]; b=para_h[2];  c= para_h[3]; neta=para_h[4]; nu=para_h[5]   
  
  # Without the parameter a: 
  h = c()                                                          ####  A vector containing h from the model,
  h[1]=(w)/(1- b - (c*(neta^(-2))))                                ####  The first value for h,

  # With the parameter a:   
  # h = c()                                                        ####  A vector containing h from the model,
  # h[1]=(w + a*(neta^4))/(1 - a*(neta^2) - b - (c*(neta^(-2))))   ####  The first value for h, Unconditional Variance
  
  
  dens = Retdensity(para_h,ret[1],h[1],rt[1])
  for (i in 2:Z1){
    h[i]=w+b*h[i-1]+ c*(neta^(-1))*(ret[i-1]-rt[i-1]-(nu*h[i-1]))
    temp=Retdensity(para_h,ret[i],h[i],rt[i])
    dens<-dens+log(temp)
  }
  
  return(dens)  
}
