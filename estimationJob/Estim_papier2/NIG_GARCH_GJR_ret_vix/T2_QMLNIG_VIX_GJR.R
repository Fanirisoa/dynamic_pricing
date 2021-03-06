########################################################################
#      Fonction densite chaque loi conditionelle pour les returns      # 
########################################################################

densite <- function(para_distribution,para_h,l){
  alpha=para_distribution[1]
  beta=para_distribution[2]
  delta=para_distribution[3]
  mu=para_distribution[4]
  
  ## set up the parameters of the model : para_h
  a0=para_h[1]; a1=para_h[2]; a2=para_h[3];  b1= para_h[4] ;  lamda0= para_h[5]  ; ro=para_h[6]
  
  
  gamma_0 = sqrt((alpha^2) -(beta^2))
  
  ## Normalisation :
  sigma_z = (delta*(alpha^2))/((gamma_0)^3)
  
  mu_z= mu - ((delta*beta)/(gamma_0 ))
  
  ## Parametrization : 
  
  alpha_1 = alpha*(sigma_z)
  beta_1 =beta*(sigma_z)
  delta_1 =delta/(sigma_z)
  mu_1 =(1/(sigma_z))*(mu-mu_z)
  
  
  drapeau=0
  if (abs(alpha)<abs(beta)){drapeau=1}
  if (alpha<=0){drapeau=1}
  if (alpha==Inf){drapeau=1}
  if (delta<=0){drapeau=1}
  
  
  if (abs(alpha_1)<abs(beta_1)){drapeau=1}
  if (alpha_1<=0){drapeau=1}
  if (alpha_1==Inf){drapeau=1}
  if (delta_1<=0){drapeau=1}
  

    if (drapeau==0){
      resultat=dgh(l,alpha_1,beta_1,delta_1,mu_1,-1/2)
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
  a0=para_h[1]; a1=para_h[2]; a2=para_h[3];  b1= para_h[4] ;  lamda0= para_h[5]  ; ro=para_h[6]
  
  
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
  
  mt = c()                                                       ####  the predictible excess of return process mt,
  mt[1]=lamda0*((h[1])^(1/2))- (h[1])/2
  
  z = c()                                                        ####  A vector containing z from the model,  innovation
  z[1]=(ret[1]-rt[1] -mt[1])/ (sqrt(h[1]))  
  
  dens = densite(para_distribution,para_h,z[1])
  
  for (i in 2:Z1){
    h[i]=a0 +b1*h[i-1]+(a1*(ret[i-1]-rt[i-1]-mt[i-1])^2)+ (a2*max(0,-(ret[i-1]-rt[i-1]-mt[i-1])^2))
    mt[i]=lamda0*((h[i])^(1/2))- (h[i])/2
    z[i]=(ret[i]-rt[i] -mt[i])/ (sqrt(h[i])) 
    temp=densite(para_distribution,para_h,z[i])
    dens<-dens+log(temp)
  }
  
  return(-dens/length(dens))
}

##########################################################
#                likelihood function                     # 
##########################################################  
NIG_likelihood_dens_QML_sim <- function(para_distribution,para_h,Ret_sim) {
  rt=0.0001197619
  Z1=length(Ret_sim)
  
  # para_distribution<-c() set up the parameters of NIG
  alpha=para_distribution[1];  beta=para_distribution[2];  delta=para_distribution[3];  mu=para_distribution[4];
  
  # para_h<-c() set up the parameters of the model 
  a0=para_h[1]; a1=para_h[2]; a2=para_h[3];  b1= para_h[4] ;  lamda0= para_h[5]  ; ro=para_h[6]
  
  
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
  
  mt = c()                                                       ####  the predictible excess of return process mt,
  mt[1]=lamda0*((h[1])^(1/2))- (h[1])/2
  
  z = c()                                                        ####  A vector containing z from the model,  innovation
  z[1]=(Ret_sim[1]-rt -mt[1])/ (sqrt(h[1]))  
  
  dens = densite(para_distribution,para_h,z[1])
  
  for (i in 2:Z1){
    h[i]=a0 +b1*h[i-1]+(a1*(Ret_sim[i-1]-rt-mt[i-1])^2)+ (a2*max(0,-(Ret_sim[i-1]-rt-mt[i-1])^2))
    mt[i]=lamda0*((h[i])^(1/2))- (h[i])/2
    z[i]=(Ret_sim[i]-rt -mt[i])/ (sqrt(h[i])) 
    temp=densite(para_distribution,para_h,z[i])
    dens<-dens+log(temp)
  }
  

  return(-dens/length(dens))
}


 
