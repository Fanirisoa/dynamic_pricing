####################################################
######         Cumulant  generating function      ##
####################################################
h_vol<-function(para_h,h,ret,rt){
  
  # para_h<-c() set up the parameters of the model 
  a0=para_h[1]; b1=para_h[2]; a1=para_h[3];  gama= para_h[4]; lambda= para_h[5];  ro=para_h[6]
  

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
  
  if (is.na(h)==TRUE){drapeau=1}else{
    if (h<=0){drapeau=1}
    if (abs(h)==Inf){drapeau=1}
    if (1/abs(h)==Inf){drapeau=1}
  }
             
  if (drapeau==0){
    resultat= a0 +b1*h+a1*h*((ret-rt- lambda*sqrt(h)+(1/2)*h)/(sqrt(h))-gama)^2
  }else{
    resultat=NA
  }
  return(resultat)
}




##############################################################
######     Conditional variance with risk netral Proba      ##
##############################################################
hstar<-function(para_h,Data.returns){
  rt=Data.returns$rt/250        #### Interest rate Data : Data.BSJ$rt
  ret=Data.returns$ret         #### Returns : Data.BSJ$ret
  Z1=length(ret)
  
  # para_h<-c() set up the parameters of the model 
  a0=para_h[1]; b1=para_h[2]; a1=para_h[3];  gama= para_h[4]; lambda= para_h[5];  ro=para_h[6]

  # Parameter under the physical probability
  h0=a0/(1- (b1+a1*(1+gama^2)))    
  
  h_star = c()                                                                ####  A vector containing h from the model,
  h_star[1]=a0/(1- (b1+a1*(1+gama^2)))                                        ####  The first value for h,
  for (i in 2:Z1){
    h_star[i]=h_vol(para_h,h_star[i-1],ret[i-1],rt[i-1])
      # a0 +b1*h_star[i-1]+a1*h_star[i-1]*(((ret[i-1]-rt[i-1]- lambda*sqrt(h_star[i-1]))/(sqrt(h_star[i-1])))-lambda-gama)^2
  }
  
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
  

  
  if (drapeau==0){
    resultat=h_star
  }else{
    resultat=rep(NA, Z1)
  }
  
  return(resultat)
}



######################
######     VIX      ##
######################
VIX_Q<-function(para_h,h,Ret,r){
  tau = 250
  T_0=22
  
  # para_h<-c() set up the parameters of the model 
  a0=para_h[1]; b1=para_h[2]; a1=para_h[3];  gama= para_h[4]; lambda= para_h[5];  ro=para_h[6]
  
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
NGARCH_likelihood_vix <- function(para_h,Data.returns,Data.ret) {
  
  Data.ret.reduiced <- Data.ret[index_vix:length(Data.ret$VIX),]
  row.names(Data.ret.reduiced) <- NULL
  Vix=Data.ret.reduiced$VIX     ####  Call dividende
  
  
  # para_h<-c() set up the parameters of the model 
  a0=para_h[1]; b1=para_h[2]; a1=para_h[3];  gama= para_h[4]; lambda= para_h[5];  ro=para_h[6]
  
  # Parameter under the physical probability
  h0=a0/(1- (b1+a1*(1+gama^2)))    
  
  VIX_Market<-Vix
  Nvix=length(Vix)
  
  h_all= hstar(para_h,Data.returns)
  h = h_all[index_ht:length(h_all)] 
  
  VIX_Model <- rep(NA, Nvix)
  for (i in 1:Nvix){
    VIX_Model[i]= VIX_Q(para_h,h[i+1],ret[i+1],rt[i+1])
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
  log_like=-1/2*sum(log(sigma)+((error^2)/sigma))-(Nvix/2)*(log(2*pi)+log(sigma*(1-(ro^2))))+ (1/2)*(log(sigma*(1-(ro^2)))-log(sigma))-(1/(2*sigma))*(error[i]^2+sum(error_2))
  
  return(log_like) 

 
}
