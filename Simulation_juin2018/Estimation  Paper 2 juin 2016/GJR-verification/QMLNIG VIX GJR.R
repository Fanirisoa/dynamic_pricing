########################################################################
#      Fonction densite chaque loi conditionelle pour les returns      # 
########################################################################

densite <- function(para_distribution,para_h,l){
  alpha=para_distribution[1]
  beta=para_distribution[2]
  delta=para_distribution[3]
  mu=para_distribution[4]
  
  ## set up the parameters of the model : para_h
  a0=para_h[1]; a1=para_h[2]; a2=para_h[3];  b1= para_h[4] ;  lamda0= para_h[5]  
  
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
NIG_likelihood_dens_QML <- function(para_distribution,para_h,Data.returns) {
  ret=Data.returns$ret   
  rt=Data.returns$rt/250        
  Z1=length(rt)
  
  # para_distribution<-c() set up the parameters of NIG
  alpha=para_distribution[1];  beta=para_distribution[2];  delta=para_distribution[3];  mu=para_distribution[4];
  
  # para_h<-c() set up the parameters of the model 
  a0=para_h[1]; a1=para_h[2]; a2=para_h[3];  b1= para_h[4] ;  lamda0= para_h[5] 
  
  h = c()                                                        ####  A vector containing h from the model,
  h[1]=(a0 )/(1 - b1 - a1- a2/2)                                 ####  The first value for h, Unconditional Variance
  
  mt = c()                                                       ####  the predictible excess of return process mt,
  mt[1]=lamda0*((h[1])^(1/2))- (h[1])/2
  
  z = c()                                                        ####  A vector containing z from the model,  innovation
  z[1]=ret[1]-rt[1] -mt[1]
  
  dens = densite(para_distribution,para_h,z[1])
  
  for (i in 2:Z1){
    h[i]=a0 +b1*h[i-1]+(a1*(ret[i-1]-rt[i-1]-mt[i-1])^2)+ (a2*max(0,-(ret[i-1]-rt[i-1]-mt[i-1])^2))
    mt[i]=lamda0*((h[i])^(1/2))- (h[i])/2
    z[i]=ret[i]-rt[i]-lamda0*((h[i])^(1/2)) + (h[i])/2
    temp=densite(para_distribution,para_h,z[i])
    dens<-dens+log(temp)
  }
  
  return(-dens/length(dens))
}

  
 
