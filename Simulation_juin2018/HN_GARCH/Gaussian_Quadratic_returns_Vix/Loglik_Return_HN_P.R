####################################################
######         The volatility updating rule       ##
####################################################
gsqrt <- function(para_h,ret,h,rt)
{
  ## set up the parameters of the model : para_h
  a0=para_h[1]; a1=para_h[2]; gama=para_h[3];  b1= para_h[4] ;  lamda0= para_h[5]   ; Pi=para_h[6] ; ro=para_h[7]
  
  # Parameter under the physical probability
  h0=(a0 + a1)/(1 - b1 - a1*(gama)^2)    
  g0=b1 + a1*(gama)^2  

  drapeau=0
  if (a0<=0){drapeau=1}
  if (a1<=0){drapeau=1}
  if (gama<=0){drapeau=1}
  if (b1<=0){drapeau=1}
  if (lamda0<=0){drapeau=1}
  if (Pi>=1.75){drapeau=1}
  if (Pi<=1.14545){drapeau=1}
  if (g0>=0.9985256){drapeau=1}
  if (g0<=0.8015256){drapeau=1}
  
   if (is.na(g0)==TRUE){drapeau=1}else{
   if (abs(g0)==Inf){drapeau=1}
   if (1/abs(g0)==Inf){drapeau=1}
  }
  # if (is.na(g1)==TRUE){drapeau=1}else{
  #   if (g1>=1){drapeau=1}
  #   if (abs(g1)==Inf){drapeau=1}
  #   if (1/abs(g1)==Inf){drapeau=1}
  # }
  if (is.na(h0)==TRUE){drapeau=1}else{
  if (h0<=0){drapeau=1}
  if (abs(h0)==Inf){drapeau=1}
  if (1/abs(h0)==Inf){drapeau=1}
  }
  # if (is.na(h1)==TRUE){drapeau=1}else{
  #   if (h1<=0){drapeau=1}
  #   if (abs(h1)==Inf){drapeau=1}
  #   if (1/abs(h1)==Inf){drapeau=1}
  # }
  # 
  if (is.na(h)==TRUE){drapeau=1}else{
    if (h<=0){drapeau=1}
    if (abs(h)==Inf){drapeau=1}
    if (1/abs(h)==Inf){drapeau=1}
  }
  
  if (drapeau==0){
    resultat= a0 +b1*h+a1*(((ret-rt-lamda0*(h))/(sqrt(h))) - gama*(sqrt(h)))^2
  }else{
    resultat=NA
  }
  return(resultat)
}


###########################################################
#####  The conditional density of the daily return     ####
###########################################################
Retdensity <- function(para_h,Ret,h,r)
{
  ## set up the parameters of the model : para_h
  a0=para_h[1]; a1=para_h[2]; gama=para_h[3];  b1= para_h[4] ;  lamda0= para_h[5]   ; Pi=para_h[6] ; ro=para_h[7]

  
  h0=(a0 + a1)/(1 - b1 - a1*(gama)^2) 
  g1=b1 + a1*(gama)^2  
  
  # Parameter under the physical probability
  lamda0star= -(1/2)
  gamastar= (gama/Pi)+(lamda0/Pi)+(1/2)
  a0star=Pi*a0
  a1star=Pi*Pi*a1
  
  
  h0=(a0 + a1)/(1 - b1 - a1*(gamastar)^2)    
  g0= b1 + a1star*(gamastar^2)  
  
  
  
  drapeau=0
  if (a0<=0){drapeau=1}
  if (a1<=0){drapeau=1}
  if (gama<=0){drapeau=1}
  if (b1<=0){drapeau=1}
  if (lamda0<=0){drapeau=1}
  if (Pi>=1.75){drapeau=1}
  if (Pi<=1.14545){drapeau=1}
  if (g0>=0.9985256){drapeau=1}
  if (g0<=0.805256){drapeau=1}
  if (g1>=0.9985256){drapeau=1}
  if (g1<=0.8015256){drapeau=1}
  
  if (is.na(g1)==TRUE){drapeau=1}else{
    if (g1>=1){drapeau=1}
    if (abs(g1)==Inf){drapeau=1}
    if (1/abs(g1)==Inf){drapeau=1}
  }
  if (is.na(h)==TRUE){drapeau=1}else{
   if (h<=0){drapeau=1}
   if (abs(h)==Inf){drapeau=1}
   if (1/abs(h)==Inf){drapeau=1}
  }
  if (is.na(h0)==TRUE){drapeau=1}else{
    if (h0<=0){drapeau=1}
    if (abs(h0)==Inf){drapeau=1}
    if (1/abs(h0)==Inf){drapeau=1}
  }
 
  if (drapeau==0){
    resultat= (1/(sqrt(2*pi*h)))*exp((-1/2)*(((Ret-r-lamda0*h)^2)/h)) 
  }else{
    resultat=NA
  }
  return(resultat)
}

###########################################################
#####  The Log-likeelihood over all the returns dates  ####
###########################################################
Heston_likelihood_ret <- function(para_h, Data.returns) {
  ret=Data.returns$ret   
  rt=Data.returns$rt/250        
  Z1=length(rt)
    
  # para_h<-c() set up the parameters of the model 
  a0=para_h[1]; a1=para_h[2]; gama=para_h[3];  b1= para_h[4] ;  lamda0= para_h[5]  ; Pi=para_h[6] ; ro=para_h[7]
  
  h = c()                                                          ####  A vector containing h from the model,
  h[1]=(a0 + a1)/(1 - b1 - a1*(gama)^2)                        ####  The first value for h, Unconditional Variance
  dens = Retdensity(para_h,ret[1],h[1],rt[1])
    
  for (i in 2:Z1){
    h[i]= gsqrt(para_h,ret[i-1],h[i-1],rt[i-1]) ### a0 +b1*h[i-1]+a1*(((ret[i-1]-rt[i-1]-lamda0*(h[i-1]))/(sqrt(h[i-1]))) - gama*(sqrt(h[i-1])))^2
    temp=Retdensity(para_h,ret[i],h[i],rt[i])
    dens<-dens+log(temp)
  }
  
  return(dens)  
}



####################################################
######   The volatility updating rule under Q     ##
####################################################
gsqrt_Q <- function(para_h,ret,h,rt)
{
  ## set up the parameters of the model : para_h
  a0=para_h[1]; a1=para_h[2]; gama=para_h[3];  b1= para_h[4] ;  lamda0= para_h[5]   ; Pi=para_h[6] ; ro=para_h[7]
  



  # Parameter under the physical probability
  lamda0star= -(1/2)
  gamastar= (gama/Pi)+(lamda0/Pi)+(1/2)
  a0star=Pi*a0
  a1star=Pi*Pi*a1
  
  
  h0=(a0 + a1)/(1 - b1 - a1*(gamastar)^2)    
  g0= b1 + a1star*(gamastar^2)  
  
  
  
  drapeau=0
  if (a0<=0){drapeau=1}
  if (a1<=0){drapeau=1}
  if (gama<=0){drapeau=1}
  if (b1<=0){drapeau=1}
  if (lamda0<=0){drapeau=1}
  
  if (is.na(g0)==TRUE){drapeau=1}else{
    if (g0>=1){drapeau=1}
    if (abs(g0)==Inf){drapeau=1}
    if (1/abs(g0)==Inf){drapeau=1}
  }
  # if (is.na(g1)==TRUE){drapeau=1}else{
  #   if (g1>=1){drapeau=1}
  #   if (abs(g1)==Inf){drapeau=1}
  #   if (1/abs(g1)==Inf){drapeau=1}
  # }
  if (is.na(h0)==TRUE){drapeau=1}else{
    if (h0<=0){drapeau=1}
    if (abs(h0)==Inf){drapeau=1}
    if (1/abs(h0)==Inf){drapeau=1}
  }
  # if (is.na(h1)==TRUE){drapeau=1}else{
  #   if (h1<=0){drapeau=1}
  #   if (abs(h1)==Inf){drapeau=1}
  #   if (1/abs(h1)==Inf){drapeau=1}
  # }
  # 
  if (is.na(h)==TRUE){drapeau=1}else{
    if (h<=0){drapeau=1}
    if (abs(h)==Inf){drapeau=1}
    if (1/abs(h)==Inf){drapeau=1}
  }
  
  if (drapeau==0){
    resultat= a0 +b1*h+a1*(((ret-rt-lamda0star*(h))/(sqrt(h))) - gamastar*(sqrt(h)))^2
  }else{
    resultat=NA
  }
  return(resultat)
}
####################################################
######         The volatility shape under Q       ##
####################################################
shape_vol_P <- function(para_h, Data.returns) {
  ret=Data.returns$ret   
  rt=Data.returns$rt/250        
  Z1=length(rt)
  
  # para_h<-c() set up the parameters of the model 
  a0=para_h[1]; a1=para_h[2]; gama=para_h[3];  b1= para_h[4] ;  lamda0= para_h[5] ; Pi=para_h[6] ; ro=para_h[7]
  
  
  h = c()                                                        ####  A vector containing h from the model,
  h[1]=(a0 + a1)/(1 - b1 - a1*(gama)^2)                         ####  The first value for h, Unconditional Variance
  
  for (i in 2:Z1){
    h[i]= gsqrt(para_h,ret[i-1],h[i-1],rt[i-1]) ### a0 +b1*h[i-1]+a1*(((ret[i-1]-rt[i-1]-lamda0*(h[i-1]))/(sqrt(h[i-1]))) - gama*(sqrt(h[i-1])))^2
  }
  
  return(h)  
}

####################################################
######         The volatility shape under Q       ##
####################################################
shape_vol_Q <- function(para_h, Data.returns) {
  ret=Data.returns$ret   
  rt=Data.returns$rt/250        
  Z1=length(rt)
  
  # para_h<-c() set up the parameters of the model 
  a0=para_h[1]; a1=para_h[2]; gama=para_h[3];  b1= para_h[4] ;  lamda0= para_h[5] ; Pi=para_h[6] ; ro=para_h[7]
  
  # Parameter under the physical probability
  lamda0star= -(1/2)
  gamastar= gama+lamda0+(1/2)
  
  h = c()                                                        ####  A vector containing h from the model,
  h[1]=(a0 + a1)/(1 - b1 - a1*(gamastar)^2)                         ####  The first value for h, Unconditional Variance
  
  for (i in 2:Z1){
    h[i]= gsqrt_Q (para_h,ret[i-1],h[i-1],rt[i-1]) ### a0 +b1*h[i-1]+a1*(((ret[i-1]-rt[i-1]-lamda0*(h[i-1]))/(sqrt(h[i-1]))) - gama*(sqrt(h[i-1])))^2
  }
  
  return(h)  
}