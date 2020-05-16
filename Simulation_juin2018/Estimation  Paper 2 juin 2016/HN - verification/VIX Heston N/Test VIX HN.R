library(MASS) 
library(fitdistrplus)
########################################################################
#                     Estimation NIG distribution                      # 
########################################################################

E_error <- function(para_distribution,para_h,Data.returns) {
  ret=Data.returns$ret   
  rt=Data.returns$rt/250        
  Z1=length(rt)
  
  # para_distribution<-c() set up the parameters of NIG
  alpha=para_distribution[1];  beta=para_distribution[2];  delta=para_distribution[3];  mu=para_distribution[4];
  
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


#########################################
# Two-sample kolmogorov-Smirnov (K-S)   # 
#########################################


library(distr)

ks_test<- function(para_distribution,X){
  n<-length(X)
  
  # para_distribution<-c() set up the parameters of the NIG distribution under P
  alpha=para_distribution1[1];  beta=para_distribution1[2];  delta=para_distribution1[3];  mu=para_distribution1[4]
  
  A <- AbscontDistribution(d = function(x, log = FALSE){
    d <-  dgh(x, alpha,beta,delta,mu,-1/2) 
    ## unstandardized!!
    if(log) d <- log(d)
    return(d)}, 
    withStand =T)
  
  Q = r(A)(n)
  resultat=ks.test(X, p(A), alternative="two.sided")
  
  return(resultat)
}


#########################################
#             AIC and BIC               # 
#########################################

Info_criteria<- function(para_distribution,X){
  n<-length(Data.returns$ret)
  L= abs(X$value)
  K<-length(para_distribution)
  
  aic <- 2*L  - 2*K
  bic <- 2*L  - K*log(n)
  
  
  return(list(log_like=L,A=aic ,B=bic))
}









