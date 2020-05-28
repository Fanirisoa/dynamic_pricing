
########################################################################
#      Fonction densite chaque loi conditionelle pour les returns      # 
########################################################################
densite <- function(para_M,l){
  # para_M = c(para_distribution,para_h) 
  # alpha=para_distribution[1], beta=para_distribution[2], delta=para_distribution[3], mu=para_distribution[4]
  # a0=para_h[1]; a1=para_h[2]; a2=para_h[3];  b1= para_h[4] ;  lamda0= para_h[5] 
  
  ## set up the parameters of the model : para_M = c(para_distribution,para_h) 
  alpha = para_M[1];  beta = para_M[2];  delta = para_M[3];  mu = para_M[4]
  a0 = para_M[5]; a1 = para_M[6]; a2 = para_M[7];  b1 = para_M[8] ;  lamda1 = para_M[9] 
  
  
  drapeau=0
  if (abs(alpha)<abs(beta)){drapeau=1}
  if (alpha<=0){drapeau=1}
  if (alpha==Inf){drapeau=1}
  if (delta<=0){drapeau=1}
  
  if (drapeau==0){
    resultat=dgh(l,alpha,beta,delta,mu,-1/2)
  }else{
    resultat=NA
  }
  return(resultat)
}


##########################################################
#                likelihood function                     # 
##########################################################  
GJR_NIG_likelihood_mt <- function(para_M,Data.returns) {
  ret=Data.returns$ret   
  rt=Data.returns$rt/250        
  Z1=length(rt)
  
  # alpha=para_distribution[1], beta=para_distribution[2], delta=para_distribution[3], mu=para_distribution[4]
  # a0=para_h[1]; a1=para_h[2]; gama=para_h[3];  b1= para_h[4] ;  lamda0= para_h[5] 
  
  ## set up the parameters of the model : para_M = c(para_distribution,para_h) 
  alpha=para_M[1];  beta=para_M[2];  delta=para_M[3];  mu=para_M[4]                    ## para_distribution<-c() set up the parameters of NIG
  a0=para_M[5]; a1=para_M[6]; a2=para_M[7];  b1= para_M[8] ;  lamda1= para_M[9]        ## para_h<-c() set up the parameters of the model
  
  
  # Change from mt = lamda0*((ht)^(1/2))- (ht)/2   to   mt = lamda1*ht
  #   lamda0*((ht)^(1/2))- (ht)/2 = lamda1*ht
  #   lamda0*((ht)^(1/2)) = lamda1*ht + (ht)/2
  #   lamda0*((ht)^(1/2)) = (3/2)*lamda1*ht
  #   lamda0 =   (3/2)*lamda1*ht*(ht^(-1/2))
  #   lamda0 =   (3/2)*lamda1*(ht^(1/2)) 

  

  gamma_0 = sqrt((alpha^2) -(beta^2))
  
  ## Normalisation :
  sigma_z = (delta*(alpha^2))/((gamma_0)^3)
  
  mu_z= mu - ((delta*beta)/(gamma_0 ))
  
  ## Parametrization : 
  
  alpha_1 = alpha*(sigma_z)
  beta_1 =beta*(sigma_z)
  delta_1 =delta/(sigma_z)
  mu_1 =(1/(sigma_z))*(mu-mu_z)


  h = c()                                                        ####  A vector containing h from the model,
  h[1]=(a0 )/(1 - b1 - a1- a2/2)                                 ####  The first value for h, Unconditional Variance
  

  lamda0 = c() 
  lamda0[1] =   (3/2)*lamda1*(h[1]^(1/2)) 
  
  mt = c()                                                       ####  the predictible excess of return process mt,
  mt[1]=lamda0*((h[1])^(1/2))- (h[1])/2
  
  z = c()                                                        ####  A vector containing z from the model,  innovation
  z[1]=ret[1]-rt[1] -mt[1]
  
  dens <- densite(para_M,z[1])
  
  
  for (i in 2:Z1){
    h[i]=a0 +b1*h[i-1]+(a1*(ret[i-1]-rt[i-1]-mt[i-1])^2)+ (a2*max(0,-(ret[i-1]-rt[i-1]-mt[i-1])^2))
    lamda0[i] =   (3/2)*lamda1*(h[i]^(1/2)) 
    mt[i]=lamda0[i]*((h[i])^(1/2))- (h[i])/2
    z[i]=ret[i]-rt[i]-lamda0[i]*((h[i])^(1/2)) + (h[i])/2
    temp=densite(para_M,z[i])
    dens<-dens+log(temp)
  }

  return(-dens/length(dens))
}


##################################
######   plot The volatility    ##
##################################
####################################################
######         The volatility updating rule       ##
####################################################
gsqrt <- function(para_h,ret,h,rt)
{
  ## set up the parameters of the model : para_h
  a0 = para_h[1]; a1 = para_h[2]; a2 = para_h[3];  b1 = para_h[4] ;  lamda1 = para_h[5]   
  
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
######         The volatility shape under P       ##
####################################################
shape_vol_P <- function(para_M, Data.returns) {
  ret=Data.returns$ret   
  rt=Data.returns$rt/250        
  Z1=length(rt)
  
  # para_M = c(para_distribution,para_h) 
  # alpha=para_distribution[1], beta=para_distribution[2], delta=para_distribution[3], mu=para_distribution[4]
  # a0=para_h[1]; a1=para_h[2]; a2=para_h[3];  b1= para_h[4] ;  lamda0= para_h[5] 
  
  ## set up the parameters of the model : para_M = c(para_distribution,para_h) 
  alpha = para_M[1];  beta = para_M[2];  delta = para_M[3];  mu = para_M[4]
  a0 = para_M[5]; a1 = para_M[6]; a2 = para_M[7];  b1 = para_M[8] ;  lamda1 = para_M[9] 
  
  para_h <- c(a0,a1,a2,b1,lamda1)
  h = c()                                                        ####  A vector containing h from the model,
  h[1]=(a0 )/(1 - b1 - a1- a2/2)                        ####  The first value for h, Unconditional Variance
  
  for (i in 2:Z1){
    h[i]= gsqrt(para_h,ret[i-1],h[i-1],rt[i-1])    ### a0 +b1*h[i-1]+a1*(((ret[i-1]-rt[i-1]-lamda0*(h[i-1]))/(sqrt(h[i-1]))) - a2*(sqrt(h[i-1])))^2
  }
  
  return(h)  
}
