###########################################################
#####  The conditional density of the daily return     ####
###########################################################
Retdensity <- function(para_h,Ret,h,r)
{
  ## set up the parameters of the model : para_h
  a0=para_h[1]; a1=para_h[2]; a2=para_h[3];  b1= para_h[4] ;  lamda1= para_h[5]   
  
  # Change from mt = lamda0*((ht)^(1/2))- (ht)/2   to   mt = lamda1*ht
  #   lamda0*((ht)^(1/2))- (ht)/2 = lamda1*ht
  #   lamda0*((ht)^(1/2)) = lamda1*ht + (ht)/2
  #   lamda0*((ht)^(1/2)) = (3/2)*lamda1*ht
  #   lamda0 =   (3/2)*lamda1*ht*(ht^(-1/2))
  #   lamda0 =   (3/2)*lamda1*(ht^(1/2)) 
  lamda0 <-  (3/2)*lamda1*(h^(1/2)) 
  
  
  Z0=b1 + a1 + a2/2
  
  drapeau=0
  if (a0<=0){drapeau=1}
  if (a1<=0){drapeau=1}
  if (a2<=0){drapeau=1}
  if (b1<=0){drapeau=1}

  
  # print("Step 1:") 
  # print(drapeau) 
  # print(a0) 
  # print(a1) 
  # print(a2)
  # print(b1) 
  # print(lamda1) 
  # 
  # if (drapeau == 1) {
  #   break
  # }
  # 
  if (is.na(h)==TRUE){drapeau=1}else{
    if (h<=0){drapeau=1}
    if (abs(h)==Inf){drapeau=1}
    if (1/abs(h)==Inf){drapeau=1}
  }
  
  # print("Step 2:") 
  # print(drapeau) 
  # if (drapeau== 1) {
  #   break
  # }
  # 
  if (is.na(Z0)==TRUE){drapeau=1}else{
    if (Z0>=1){drapeau=1}
    if (Z0<=0){drapeau=1}
    if (abs(Z0)==Inf){drapeau=1}
    if (1/abs(Z0)==Inf){drapeau=1}
  }

  # print("Step 3:") 
  # print(drapeau) 
  # if (drapeau == 1) {
  #   break
  # }
  
  if (drapeau==0){
    resultat= (1/(sqrt(2*pi*h)))*exp((-1/(2*h))*(((Ret-r-lamda0*((h)^(1/2))+(h)/2)^2)))
  }else{
    resultat=NA
  }
  return(resultat)
}


###########################################################
#####  The Log-likeelihood over all the returns dates  ####
###########################################################
GJR_likelihood_ret_mt <- function(para_h, Data.returns) {
  ret=Data.returns$ret   
  rt=Data.returns$rt/250        
  Z1=length(rt)
  
  ## set up the parameters of the model : para_h
  a0=para_h[1]; a1=para_h[2]; a2=para_h[3];  b1= para_h[4] ;  lamda1= para_h[5]   
  
  # Change from mt = lamda0*((ht)^(1/2))- (ht)/2   to   mt = lamda1*ht
  #   lamda0*((ht)^(1/2))- (ht)/2 = lamda1*ht
  #   lamda0*((ht)^(1/2)) = lamda1*ht + (ht)/2
  #   lamda0*((ht)^(1/2)) = (3/2)*lamda1*ht
  #   lamda0 =   (3/2)*lamda1*ht*(ht^(-1/2))
  #   lamda0 =   (3/2)*lamda1*(ht^(1/2)) 

  
  h = c()                                                        ####  A vector containing h from the model,
  h[1]=(a0 )/(1 - b1 - a1- a2/2)                                 ####  The first value for h, Unconditional Variance
  dens = Retdensity(para_h,ret[1],h[1],rt[1])
  
 lamda0 = c() 
 lamda0[1] =   (3/2)*lamda1*(h[1]^(1/2)) 
  
  mt = c()                                                       ####  the predictible excess of return process mt,
  mt[1]=lamda0[1]*((h[1])^(1/2))- (h[1])/2
  
  for (i in 2:Z1){
    h[i]=a0 +b1*h[i-1]+(a1*(ret[i-1]-rt[i-1]-mt[i-1])^2)+ (a2*max(0,-(ret[i-1]-rt[i-1]-mt[i-1])^2))
    lamda0[i] =   (3/2)*lamda1*(h[i]^(1/2)) 
    mt[i]=(lamda0[i])*((h[i])^(1/2))- (h[i])/2
    temp=Retdensity(para_h,ret[i],h[i],rt[i])
    dens<-dens+log(temp)
  }
  
  return(-dens)  
}



####################################################
######         The volatility updating rule       ##
####################################################
gsqrt <- function(para_h,ret,h,rt)
{
  ## set up the parameters of the model : para_h
  a0=para_h[1]; a1=para_h[2]; a2=para_h[3];  b1= para_h[4] ;  lamda1= para_h[5]   
  
  # Change from mt = lamda0*((ht)^(1/2))- (ht)/2   to   mt = lamda1*ht
  #   lamda0*((ht)^(1/2))- (ht)/2 = lamda1*ht
  #   lamda0*((ht)^(1/2)) = lamda1*ht + (ht)/2
  #   lamda0*((ht)^(1/2)) = (3/2)*lamda1*ht
  #   lamda0 =   (3/2)*lamda1*ht*(ht^(-1/2))
  #   lamda0 =   (3/2)*lamda1*(ht^(1/2)) 
  lamda0 =   (3/2)*lamda1*(h^(1/2)) 
  
  
  # Parameter under the physical probability
  h0=(a0 )/(1 - b1 - a1- a2/2)   
  g0=b1+ a1+ a2/2

  mt=lamda0*((h)^(1/2))- (h)/2

  drapeau=0
  if (a0<=0){drapeau=1}
  if (a1<=0){drapeau=1}
  if (a2<=0){drapeau=1}
  if (b1<=0){drapeau=1}

   if (is.na(g0)==TRUE){drapeau=1}else{
   if (g0>=1){drapeau=1}
   if (g0<=0.70){drapeau=1}
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
    resultat= a0 +b1*h +(a1*(ret-rt-mt)^2)+ (a2*max(0,-(ret-rt-mt)^2))
  }else{
    resultat=NA
  }
  return(resultat)
}




####################################################
######   The volatility updating rule under Q     ##
####################################################
gsqrt_Q <- function(para_h,ret,h,rt)
{
  ## set up the parameters of the model : para_h
  a0=para_h[1]; a1=para_h[2]; a2=para_h[3];  b1= para_h[4] ;  lamda1= para_h[5]   
  
  # Change from mt = lamda0*((ht)^(1/2))- (ht)/2   to   mt = lamda1*ht
  #   lamda0*((ht)^(1/2))- (ht)/2 = lamda1*ht
  #   lamda0*((ht)^(1/2)) = lamda1*ht + (ht)/2
  #   lamda0*((ht)^(1/2)) = (3/2)*lamda1*ht
  #   lamda0 =   (3/2)*lamda1*ht*(ht^(-1/2))
  #   lamda0 =   (3/2)*lamda1*(ht^(1/2)) 
  lamda0 =   (3/2)*lamda1*(h^(1/2)) 
  
  
  # Parameter under the physical probability
  h0=(a0 )/(1 - b1 - a1- a2/2)   
  g0=b1+ a1+ a2/2
  
  mt= - (h)/2
  
  drapeau=0
  if (a0<=0){drapeau=1}
  if (a1<=0){drapeau=1}
  if (a2<=0){drapeau=1}
  if (b1<=0){drapeau=1}

  if (is.na(g0)==TRUE){drapeau=1}else{
    if (g0>=0.9975146){drapeau=1}
    if (g0<=0.50){drapeau=1}
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
    resultat= a0 +b1*h + h*a1*(((ret-rt-mt)/(h^(-1/2))) - (lamda1+(1/2))*(h^(-1/2)))^2
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
  a0=para_h[1]; a1=para_h[2]; a2=para_h[3];  b1= para_h[4] ;  lamda1= para_h[5]
  
  
  h = c()                                                        ####  A vector containing h from the model,
  h[1]=(a0 )/(1 - b1 - a1- a2/2)                        ####  The first value for h, Unconditional Variance
  
  for (i in 2:Z1){
    h[i]= gsqrt(para_h,ret[i-1],h[i-1],rt[i-1]) ### a0 +b1*h[i-1]+a1*(((ret[i-1]-rt[i-1]-lamda0*(h[i-1]))/(sqrt(h[i-1]))) - a2*(sqrt(h[i-1])))^2
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
  a0=para_h[1]; a1=para_h[2]; a2=para_h[3];  b1= para_h[4] ;  lamda1= para_h[5]
  
 
  h = c()                                                        ####  A vector containing h from the model,
  h[1]=(a0 )/(1 - b1 - a1- a2/2)                                 ####  The first value for h, Unconditional Variance
  
  for (i in 2:Z1){
    h[i]= gsqrt_Q (para_h,ret[i-1],h[i-1],rt[i-1]) ### a0 +b1*h[i-1]+a1*(((ret[i-1]-rt[i-1]-lamda0*(h[i-1]))/(sqrt(h[i-1]))) - a2*(sqrt(h[i-1])))^2
  }
  
  return(h)  
}