library(MASS) 
library(fitdistrplus)
########################################################################
#                     Extraction of the inovation HN                   # 
########################################################################

E_errorHN <- function(para_h,Data.returns) {
  ret=Data.returns$ret   
  rt=Data.returns$rt/250        
  Z1=length(rt)
  

  # para_h<-c() set up the parameters of the model 
  a0=para_h[1]; a1=para_h[2]; gama=para_h[3];  b1= para_h[4] ;  lamda0= para_h[5]
  
  h = c()                                                        ####  A vector containing h from the model,
  h[1]=(a0 + a1)/(1 - b1 - a1*(gama)^2 )                         ####  The first value for h, Unconditional Variance
  z = c()                                                        ####  A vector containing z from the model,  innovation
  z[1]=ret[1]-rt[1]-lamda0*(h[1])
  
  for (i in 2:Z1){
    h[i]=a0 +b1*h[i-1]+a1*(((ret[i-1]-rt[i-1]-lamda0*(h[i-1]))/(sqrt(h[i-1]))) - gama*(sqrt(h[i-1])))^2
    z[i]=ret[i]-rt[i]-lamda0*(h[i])
  }
  
  return(z)
}


########################################################################
#                     Extraction of the inovation GJR                   # 
########################################################################
E_errorGJR <- function(para_h,Data.returns) {
  ret=Data.returns$ret   
  rt=Data.returns$rt/250        
  Z1=length(rt)
  
  # para_h<-c() set up the parameters of the model 
  a0=para_h[1]; a1=para_h[2]; a2=para_h[3];  b1= para_h[4] ;  lamda0= para_h[5]  ; ro=para_h[6]
  
  h = c()                                                        ####  A vector containing h from the model,
  h[1]=(a0 )/(1 - b1 - a1- a2/2)                                 ####  The first value for h, Unconditional Variance
  
  mt = c()                                                       ####  the predictible excess of return process mt,
  mt[1]=lamda0*((h[1])^(1/2))- (h[1])/2
  
  z = c()                                                        ####  A vector containing z from the model,  innovation
  z[1]=ret[1]-rt[1] -mt[1]
  
  for (i in 2:Z1){
    h[i]=a0 +b1*h[i-1]+(a1*(ret[i-1]-rt[i-1]-mt[i-1])^2)+ (a2*max(0,-(ret[i-1]-rt[i-1]-mt[i-1])^2))
    mt[i]=lamda0*((h[i])^(1/2))- (h[i])/2
    z[i]=ret[i]-rt[i]-lamda0*((h[i])^(1/2)) + (h[i])/2
  }
  
  return(z)
}
########################################################################
#   densite fonction loi NIG distribution based on GH distribution     # 
########################################################################

ds <- function(x,a, b, d,  mu){
  SPret=Data.returns$ret
  n<-length(SPret)
  
  ## set up the parameters of the distribution : para_h  
  ## a=para_distribution[1];   b=para_distribution[2];  d=para_distribution[3];  mu=para_distribution[4];
  
  
  drapeau=0
  if (abs(a)<abs(b)){drapeau=1}
  if (a<=0){drapeau=1}
  if (a==Inf){drapeau=1}
  if (d<=0){drapeau=1}
  
  if (drapeau==0){
    resultat=dgh(x,a,b,d,mu,-1/2)
  }else{
    resultat=NA
  }
  return(resultat)
}

########################################################################
#      CDF fonction loi NIG distribution based on GH distribution      # 
########################################################################

ps <- function(q,a, b, d,  mu){
  SPret=Data.returns$ret
  n<-length(SPret)
  
  ## set up the parameters of the distribution : para_h  
  ## a=para_distribution[1];   b=para_distribution[2];  d=para_distribution[3];  mu=para_distribution[4];
  
  
  drapeau=0
  if (abs(a)<abs(b)){drapeau=1}
  if (a<=0){drapeau=1}
  if (a==Inf){drapeau=1}
  if (d<=0){drapeau=1}
  
  if (drapeau==0){
    resultat=pgh(q,a,b,d,mu,-1/2)
  }else{
    resultat=NA
  }
  return(resultat)
}


########################################################################
#   quantile fonction loi NIG distribution based on GH distribution    # 
########################################################################
qs <- function(p,a, b, d,  mu){
  SPret=Data.returns$ret
  n<-length(SPret)
  
  ## set up the parameters of the distribution : para_h  
  ## a=para_distribution[1];   b=para_distribution[2];  d=para_distribution[3];  mu=para_distribution[4];
  
  
  drapeau=0
  if (abs(a)<abs(b)){drapeau=1}
  if (a<=0){drapeau=1}
  if (a==Inf){drapeau=1}
  if (d<=0){drapeau=1}
  
  if (drapeau==0){
    resultat=qgh(p,a,b,d,mu,-1/2)
  }else{
    resultat=NA
  }
  return(resultat)
}

