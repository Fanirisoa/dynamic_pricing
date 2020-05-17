##############################################################
######     Conditional variance with risk netral Proba      ##
##############################################################
h_vol<-function(para_h,h,ret,rt){
  
  # para_h<-c() set up the parameters of the model 
  a0=para_h[1]; b1=para_h[2]; a1=para_h[3];  gama= para_h[4]; lambda= para_h[5] ; ro=para_h[6]
  
  # Parameter under the physical probability
  h0=a0/(1- (b1+a1*(1+gama^2)))    
  
  
  g1=(b1+a1*(1+gama^2))
  gamastar =  gama+lambda
  g0=b1 + a1*(gamastar)^2  
  
  # Parameter under the physical probability
  h0=a0/(1- (b1+a1*(1+gama^2)))    
  
  
  drapeau=0
  if (a0<=0){drapeau=1}
  if (b1<=0){drapeau=1}
  if (a1<=0){drapeau=1}
  if (g0<=0.7){drapeau=1}
  if (g1<=0.7){drapeau=1}
  if (g0>=0.996132){drapeau=1}
  if (g1>=0.996132){drapeau=1}
  if (gama<=0){drapeau=1}
  if (ro<=0.7){drapeau=1}
  if (ro>=0.998765){drapeau=1}
  
  if (is.na(h0)==TRUE){drapeau=1}else{
    if (h0<=0){drapeau=1}
    if (abs(h0)==Inf){drapeau=1}
    if (1/abs(h0)==Inf){drapeau=1}
  }
  
  if (drapeau==0){
    resultat= a0 +b1*h+a1*h*(((ret-rt- lambda*sqrt(h)+(1/2)*h)/(sqrt(h)))-lambda-gama)^2
  }else{
    resultat=NA
  }
  return(resultat)
}



##############################################################
######     Conditional variance with risk netral Proba      ##
##############################################################
h_star<-function(para_h,Data.returns){
  rt=Data.returns$rt/250        #### Interest rate Data : Data.BSJ$rt
  ret=Data.returns$ret         #### Returns : Data.BSJ$ret
  Z1=length(ret)
  
  # para_h<-c() set up the parameters of the model 
  a0=para_h[1]; b1=para_h[2]; a1=para_h[3];  gama= para_h[4]; lambda= para_h[5]; ro=para_h[6]  
  
  
  # Parameter under the physical probability
  h0=a0/(1- (b1+a1*(1+gama^2)))    
  g1=(b1+a1*(1+gama^2))
  gamastar =  gama+lambda
  g0=b1 + a1*(gamastar)^2  
  
  h_star_val = c()                                                                ####  A vector containing h from the model,
  h_star_val[1]=a0/(1- (b1+a1*(1+gama^2)))                                        ####  The first value for h,
  for (i in 2:Z1){
    h_star_val[i]=h_vol(para_h,h_star_val[i-1],ret[i-1],rt[i-1])
    # a0 +b1*h_star[i-1]+a1*h_star[i-1]*(((ret[i-1]-rt[i-1]- lambda*sqrt(h_star[i-1]))/(sqrt(h_star[i-1])))-lambda-gama)^2
  }
  
  drapeau=0
  if (a0<=0){drapeau=1}
  if (b1<=0){drapeau=1}
  if (a1<=0){drapeau=1}
  if (ro<=0.7){drapeau=1}
  if (ro>=0.998765){drapeau=1}
  if (gama<=0){drapeau=1}
  if (g0<=0.7){drapeau=1}
  if (g1<=0.7){drapeau=1}
  if (g0>=0.996132){drapeau=1}
  if (g1>=0.996132){drapeau=1}
  
  if (is.na(h0)==TRUE){drapeau=1}else{
    if (h0<=0){drapeau=1}
    if (abs(h0)==Inf){drapeau=1}
    if (1/abs(h0)==Inf){drapeau=1}
  }
  
  if (drapeau==0){
    resultat=h_star_val
  }else{
    resultat=rep(NA, Z1)
  }
  return(resultat)
}

######################
######     VIX      ##
######################
VIX_Q<-function(para_h,h){
  tau = 250
  T_0=22
  
  # para_h<-c() set up the parameters of the model 
  a0=para_h[1]; b1=para_h[2]; a1=para_h[3];  gama= para_h[4]; lambda= para_h[5]; ro=para_h[6]
  
  # Parameter under the physical probability
  h0=a0/(1- (b1+a1*(1+gama^2)))    
  
  drapeau=0
  if (a0<=0){drapeau=1}
  if (b1<=0){drapeau=1}
  if (a1<=0){drapeau=1}
  if (ro<=0){drapeau=1}
  if (ro>=1){drapeau=1}
  if (gama<=0){drapeau=1}
  
  if (is.na(h0)==TRUE){drapeau=1}else{
    if (h0<=0){drapeau=1}
    if (abs(h0)==Inf){drapeau=1}
    if (1/abs(h0)==Inf){drapeau=1}
  }
  
  Psy = b1+a1*(1+(lambda+gama)^2)
  #  VIX 
  
  if (drapeau==0){
    resultat= 100*sqrt(tau/T_0)*sqrt(a0*((22/(1-Psy))- ((1-(Psy^T_0))/(1-Psy)^2))+ h*((1-(Psy^T_0))/(1-Psy)))
  }else{
    resultat=NA
  }
  return(resultat)
  
}


###########################################################
#####       The Log-likeelihood over all Option        ####
###########################################################
NGARCH_likelihood_vix <- function(para_M, Data.returns,Data.ret){
  Vix=Data.ret$VIX      ####  Call dividende
  
  # para_M = c(para_distribution,para_h) 
  # alpha=para_distribution[1], beta=para_distribution[2], delta=para_distribution[3], mu=para_distribution[4]
  # a0=para_h[1]; a1=para_h[2]; a2=para_h[3];  b1= para_h[4] ;  lamda0= para_h[5]  ; ro=para_h[6]
  
  ## set up the parameters of the model : para_M = c(para_distribution,para_h) 
  alpha=para_M[1];  beta=para_M[2];  delta=para_M[3];  mu=para_M[4]
  a0=para_M[5]; b1=para_M[6]; a1=para_M[7];  gama= para_M[8] ;  lambda= para_M[9]  ; ro=para_M[10]
  
  # para_h<-c() set up the parameters of the model 
  para_h = c()
  para_h[1]= a0; para_h[2]=b1; para_h[3]=a1; para_h[4]=gama;  para_h[5]=lambda ; para_h[6]=ro

  
  VIX_Market<-Vix

  Nvix=length(Vix)
  
  h = h_star(para_h,Data.returns)
  
  VIX_Model <- rep(NA, Nvix)
  for (i in 1:Nvix){
    VIX_Model[i]= VIX_Q(para_h,h[i+1])
  }
  
  error <- rep(NA, Nvix)
  error[Nvix]=0
  for (i in 1:Nvix-1){
    error[i]= VIX_Market[i] - VIX_Model[i]
  }

  error_2 <- rep(NA, Nvix)
  error_2[1]=0
  for (i in 2:Nvix){
    error_2[i]= ((error[i]-ro*error[i-1])^2)/(1-ro^2)
  }
    
  sigma=mean(error^2)
  log_like=-1/2*sum(log(sigma)+((error^2)/sigma))  
  -(Nvix/2)*(log(2*pi)+log(sigma*(1-(ro^2))))+ (1/2)*(log(sigma*(1-(ro^2)))-log(sigma))-(1/(2*sigma))*(error[i]^2+sum(error_2))
  
  return(log_like)  
  
}

