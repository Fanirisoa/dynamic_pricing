###########################################################
#####  The conditional density of the daily return     ####
###########################################################
Retdensity <- function(para_h,Ret,h,r)
{
  # para_h<-c() set up the parameters of the model 
  a0=para_h[1]; a1=para_h[2]; a2=para_h[3];  b1= para_h[4] ;  lamda0= para_h[5]  
  
  
  Z0=b1 + a1 + a2/2
  
  drapeau=0
  if (a0<=0){drapeau=1}
  if (a1<=0){drapeau=1}
  if (a2<=0){drapeau=1}
  if (b1<=0){drapeau=1}
  if (lamda0<=0){drapeau=1}
  
  if (is.na(h)==TRUE){drapeau=1}else{
    if (h<=0){drapeau=1}
    if (abs(h)==Inf){drapeau=1}
    if (1/abs(h)==Inf){drapeau=1}
  }
  if (is.na(Z0)==TRUE){drapeau=1}else{
    if (Z0>=1){drapeau=1}
    if (Z0<=0){drapeau=1}
    if (abs(Z0)==Inf){drapeau=1}
    if (1/abs(Z0)==Inf){drapeau=1}
  }
  
  
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
Heston_likelihood_ret <- function(para_h, Data.returns) {
  ret=Data.returns$ret   
  rt=Data.returns$rt/250        
  Z1=length(rt)
  
  # para_h<-c() set up the parameters of the model 
  a0=para_h[1]; a1=para_h[2]; a2=para_h[3];  b1= para_h[4] ;  lamda0= para_h[5]  
  
  h = c()                                                        ####  A vector containing h from the model,
  h[1]=(a0 )/(1 - b1 - a1- a2/2)                                 ####  The first value for h, Unconditional Variance
  dens = Retdensity(para_h,ret[1],h[1],rt[1])
  
  mt = c()                                                       ####  the predictible excess of return process mt,
  mt[1]=lamda0*((h[1])^(1/2))- (h[1])/2
  
  for (i in 2:Z1){
    h[i]=h[i]=a0 +b1*h[i-1]+(a1*(ret[i-1]-rt[i-1]-mt[i-1])^2)+ (a2*max(0,-(ret[i-1]-rt[i-1]-mt[i-1])^2))
    mt[i]=lamda0*((h[i])^(1/2))- (h[i])/2
    temp=Retdensity(para_h,ret[i],h[i],rt[i])
    dens<-dens+log(temp)
  }
  
  return(dens)  
}


#############################################################
#####  tHE LOGLIKELIHOODE of both the mixe return-option ####
#############################################################
Heston_likelihood_GJRNIG <- function(para_h,Data.returns) {
  
  log_like= Heston_likelihood_ret(para_h, Data.returns)
  
  return(-log_like)  
}





