####################################################
######         Cumulant  generating function      ##
####################################################
K_eps<-function(z,a,b,c,d){
  
  ## Mean 0 and Variance 1
  c0=a^2 - b^2
  c2=a^2 - (b+z*c)^2
  
  
  drapeau=0
  if (is.na(c0)==TRUE){drapeau=1}else{
    if (c0<=0){drapeau=1}
    if (c0==Inf){drapeau=1}
    if (1/c0==Inf){drapeau=1}
  }
  
  if (is.na(c2)==TRUE){drapeau=1}else{
    if (c2<=0){drapeau=1}
    if (abs(c2)==Inf){drapeau=1}
    if (1/abs(c2)==Inf){drapeau=1}
  }

  if (drapeau==0){
    resultat=  z*d+sqrt(a^2 - b^2)-sqrt(a^2 - (b+z*c)^2)
  }else{
    resultat=NA
  }
  return(resultat)
}
  

####################################################
######         The volatility updating rule       ##
####################################################
gsqrt <- function(para_h,ret,h,rt)
{
  # para_h<-c() set up the parameters of the model 
  a0=para_h[1]; b1=para_h[2]; a1=para_h[3];  gama= para_h[4]; lambda= para_h[5]; a=para_h[6]; b=para_h[7] ## ; c=para_h[5]; d=para_h[6] 
  
  ## Mean 0 and Variance 1
  c0=a^2 - b^2
  c=((sqrt(a^2 - b^2))^(3/2))/a
  d=(-b/a)*(sqrt(a^2 - b^2))^(1/2)
  g1=(b1+a1*(1+gama^2))
  gamastar =  gama+lambda
  g0=b1 + a1*(gamastar)^2  
  
  # Parameter under the physical probability
  h0=a0/(1- (b1+a1*(1+gama^2)))    
  b0=abs(b)
  
  
  drapeau=0
  if (a0<=0){drapeau=1}
  if (b1<=0){drapeau=1}
  if (a1<=0){drapeau=1}
  if (g0<=0.7){drapeau=1}
  if (g1<=0.7){drapeau=1}
  if (g0>=0.996132){drapeau=1}
  if (g1>=0.996132){drapeau=1}
  if (gama<=0){drapeau=1}
  if (b0<=0){drapeau=1}
  if (c<=0){drapeau=1}
  if (c0<=0){drapeau=1}
  if (a<=0){drapeau=1}
  
  if (is.na(b0)==TRUE){drapeau=1}else{
    if (b0<=0){drapeau=1}
    if (b0>=a){drapeau=1}
    if (b0==Inf){drapeau=1}
    if (1/b0==Inf){drapeau=1}
  }
 
  if (is.na(h0)==TRUE){drapeau=1}else{
    if (h0<=0){drapeau=1}
    if (abs(h0)==Inf){drapeau=1}
    if (1/abs(h0)==Inf){drapeau=1}
  }
 
  if (is.na(c)==TRUE){drapeau=1}else{
    if (c<=0){drapeau=1}
    if (abs(c)==Inf){drapeau=1}
    if (1/abs(c)==Inf){drapeau=1}
  }
  

  if (drapeau==0){
    resultat= a0 +b1*h+a1*h*((ret-rt- lambda*sqrt(h)+K_eps(sqrt(h),a,b,c,d))/(sqrt(h))-gama)^2
  }else{
    resultat=NA
  }
  return(resultat)
}

###########################################################
#####  The conditional density of the daily return     ####
###########################################################
Retdensity <- function(para_h,ret,h,rt)
{
  # para_h<-c() set up the parameters of the model 
  a0=para_h[1]; b1=para_h[2]; a1=para_h[3];  gama= para_h[4]; lambda= para_h[5]; a=para_h[6]; b=para_h[7] ## ; c=para_h[5]; d=para_h[6] 
  
  ## Mean 0 and Variance 1
  c0=a^2 - b^2
  c=((sqrt(a^2 - b^2))^(3/2))/a
  d=(-b/a)*(sqrt(a^2 - b^2))^(1/2)
  g1=(b1+a1*(1+gama^2))
  gamastar =  gama+lambda
  g0=b1 + a1*(gamastar)^2  
  
  # Parameter under the physical probability
  h0=a0/(1- (b1+a1*(1+gama^2)))    
  b0=abs(b)
  
  
  drapeau=0
  if (a0<=0){drapeau=1}
  if (b1<=0){drapeau=1}
  if (g0<=0.7){drapeau=1}
  if (g1<=0.7){drapeau=1}
  if (g0>=0.996132){drapeau=1}
  if (g1>=0.996132){drapeau=1}
  if (a1<=0){drapeau=1}

  if (gama<=0){drapeau=1}
  if (b0<=0){drapeau=1}
  if (c<=0){drapeau=1}
  if (c0<=0){drapeau=1}
  if (a<=0){drapeau=1}
  
  if (is.na(b0)==TRUE){drapeau=1}else{
    if (b0<=0){drapeau=1}
    if (b0>=a){drapeau=1}
    if (b0==Inf){drapeau=1}
    if (1/b0==Inf){drapeau=1}
  }
  
  if (is.na(h0)==TRUE){drapeau=1}else{
    if (h0<=0){drapeau=1}
    if (abs(h0)==Inf){drapeau=1}
    if (1/abs(h0)==Inf){drapeau=1}
  }
  
  if (is.na(c)==TRUE){drapeau=1}else{
    if (c<=0){drapeau=1}
    if (abs(c)==Inf){drapeau=1}
    if (1/abs(c)==Inf){drapeau=1}
  }
  
  if (drapeau==0){
    resultat= dnig((ret-rt- lambda*sqrt(h)+K_eps(sqrt(h),a,b,c,d))/(sqrt(h)), alpha = a, beta = b, delta = c, mu = d, log = FALSE)

  }else{
    resultat=NA
  }
  return(resultat)
}


###########################################################
#####  The Log-likeelihood over all the returns dates  ####
###########################################################
NGARCH_likelihood_ret <- function(para_h, Data.returns) {
  ret=Data.returns$ret   
  rt=Data.returns$rt/250        
  Z1=length(rt)
  # para_h<-c() set up the parameters of the model 
  a0=para_h[1]; b1=para_h[2]; a1=para_h[3];  gama= para_h[4]; lambda= para_h[5]; a=para_h[6]; b=para_h[7] ## ; c=para_h[5]; d=para_h[6] 
  
  ## Mean 0 and Variance 1
  c0=a^2 - b^2
  c=((sqrt(a^2 - b^2))^(3/2))/a
  d=(-b/a)*(sqrt(a^2 - b^2))^(1/2)
  
  
  h = c()                                                          ####  A vector containing h from the model,
  h[1]=a0/(1- (b1+a1*(1+gama^2)))                                  ####  The first value for h, Unconditional Variance
  dens = Retdensity(para_h,ret[1],h[1],rt[1])
  
  for (i in 2:Z1){
    h[i]= gsqrt(para_h,ret[i-1],h[i-1],rt[i-1]) ### a0 +b1*h[i-1]+a1*(((ret[i-1]-rt[i-1]-lamda0*(h[i-1]))/(sqrt(h[i-1]))) - gama*(sqrt(h[i-1])))^2
    temp=Retdensity(para_h,ret[i],h[i],rt[i])
    dens<-dens+log(temp)
  }
  
  return(-dens)  
}



####################################################
######   The volatility updating rule under Q     ##
####################################################
gsqrt_Q <- function(para_h,ret,h,rt)
{
  # para_h<-c() set up the parameters of the model 
  a0=para_h[1]; b1=para_h[2]; a1=para_h[3];  gama= para_h[4]; lambda= para_h[5]; a=para_h[6]; b=para_h[7] ## ; c=para_h[5]; d=para_h[6] 
  
  ## Mean 0 and Variance 1
  c0=a^2 - b^2
  c=((sqrt(a^2 - b^2))^(3/2))/a
  d=(-b/a)*(sqrt(a^2 - b^2))^(1/2)
  
  
  # Parameter under the physical probability
  h0=a0/(1- (b1+a1*(1+gama^2)))    
  b0=abs(b)
  
  
  drapeau=0
  if (a0<=0){drapeau=1}
  if (b1<=0){drapeau=1}
  if (a1<=0){drapeau=1}
  if (gama<=0){drapeau=1}
  if (b0<=0){drapeau=1}
  if (c<=0){drapeau=1}
  if (c0<=0){drapeau=1}
  if (a<=0){drapeau=1}
  
  if (is.na(b0)==TRUE){drapeau=1}else{
    if (b0<=0){drapeau=1}
    if (b0>=a){drapeau=1}
    if (b0==Inf){drapeau=1}
    if (1/b0==Inf){drapeau=1}
  }
  
  if (is.na(h0)==TRUE){drapeau=1}else{
    if (h0<=0){drapeau=1}
    if (abs(h0)==Inf){drapeau=1}
    if (1/abs(h0)==Inf){drapeau=1}
  }
  
  if (is.na(c)==TRUE){drapeau=1}else{
    if (c<=0){drapeau=1}
    if (abs(c)==Inf){drapeau=1}
    if (1/abs(c)==Inf){drapeau=1}
  }
  
  
  if (drapeau==0){
    resultat= a0 +b1*h+a1*h*((ret-rt- lambda*sqrt(h))/(sqrt(h))-gama)^2
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
  a0=para_h[1]; b1=para_h[2]; a1=para_h[3];  gama= para_h[4]; lambda= para_h[5]; a=para_h[6]; b=para_h[7] ## ; c=para_h[5]; d=para_h[6] 
  
  
  ## Mean 0 and Variance 1
  c0=a^2 - b^2
  c=((sqrt(a^2 - b^2))^(3/2))/a
  d=(-b/a)*(sqrt(a^2 - b^2))^(1/2)
  
  
  # Parameter under the physical probability
  h0=a0/(1- (b1+a1*(1+gama^2)))    
  b0=abs(b)
  
  h = c()                                                        ####  A vector containing h from the model,
  h[1]=h0                                                        ####  The first value for h, Unconditional Variance
  
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
  a0=para_h[1]; b1=para_h[2]; a1=para_h[3];  gama= para_h[4]; lambda= para_h[5]; a=para_h[6]; b=para_h[7] ## ; c=para_h[5]; d=para_h[6] 
  
  
  ## Mean 0 and Variance 1
  c0=a^2 - b^2
  c=((sqrt(a^2 - b^2))^(3/2))/a
  d=(-b/a)*(sqrt(a^2 - b^2))^(1/2)
  
  
  # Parameter under the physical probability
  h0=a0/(1- (b1+a1*(1+gama^2)))    
  b0=abs(b)
  
  h = c()                                                        ####  A vector containing h from the model,
  h[1]=h0                                                        ####  The first value for h, Unconditional Variance
  
  for (i in 2:Z1){
    h[i]= gsqrt_Q (para_h,ret[i-1],h[i-1],rt[i-1]) ### a0 +b1*h[i-1]+a1*(((ret[i-1]-rt[i-1]-lamda0*(h[i-1]))/(sqrt(h[i-1]))) - gama*(sqrt(h[i-1])))^2
  }
  
  return(h)  
}