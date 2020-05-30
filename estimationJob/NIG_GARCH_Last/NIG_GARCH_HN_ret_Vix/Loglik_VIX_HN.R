####################################################
######         The volatility updating rule       ##
####################################################
gsqrt <- function(para_h,ret,h,rt)
{
  # para_h<-c() set up the parameters of the model 
  a0=para_h[1]; a1=para_h[2]; gama=para_h[3];  b1= para_h[4] ;  lamda0= para_h[5] ; ro=para_h[6]
  
  # Parameter under the physical probability
  lamda0star= -(1/2)
  gamastar= gama+lamda0+(1/2)
  
  g1= b1 + a1*(gamastar^2)           ####  The percistence
  h0=(a0 + a1)/(1 - g1)              ####  The first value for h, Unconditional Variance
  z2=1 - b1 - a1*(gamastar)^2 
  
  drapeau=0
  if (a0<=0){drapeau=1}
  if (a1<=0){drapeau=1}
  if (gama<=0){drapeau=1}
  if (b1<=0){drapeau=1}
  if (lamda0<=-1/2){drapeau=1}
  
  if (is.na(g1)==TRUE){drapeau=1}else{
    if (g1>=1){drapeau=1}
    if (g1<=0){drapeau=1}
    if (abs(g1)==Inf){drapeau=1}
    if (1/abs(g1)==Inf){drapeau=1}
  }
  if (is.na(z2)==TRUE){drapeau=1}else{
    if (z2<0){drapeau=1}
    if (abs(z2)==Inf){drapeau=1}
    if (1/abs(z2)==Inf){drapeau=1}
  }
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
    resultat= a0 +b1*h+a1*(((ret-rt-lamda0star*(h))/((h)^(1/2))) - gamastar*((h)^(1/2)))^2
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
  ret=Data.returns$ret          #### Returns : Data.BSJ$ret
  Z1=length(ret)
  
  # para_h<-c() set up the parameters of the model 
  a0=para_h[1]; a1=para_h[2]; gama=para_h[3];  b1= para_h[4] ;  lamda0= para_h[5] ; ro=para_h[6]
  
  # Parameter under the physical probability
  lamda0star= -(1/2)
  gamastar= gama+lamda0+(1/2)
  
  g1= b1 + a1*(gamastar^2)           ####  The percistence
  h0=(a0 + a1)/(1 - g1)              ####  The first value for h, Unconditional Variance
  
  
  h_star = c()                       ####  A vector containing h from the model,
  h_star[1]=h0                       ####  The first value for h,
  for (i in 2:Z1){ 
    h_star[i]=gsqrt(para_h,ret[i-1],h_star[i-1],rt[i-1])
  }
  
  
  z2=min(h_star)
  z3=1 - b1 - a1*(gamastar)^2 
  
  drapeau=0
  
  if (a0<=0){drapeau=1}
  if (a1<=0){drapeau=1}
  if (gama<=0){drapeau=1}
  if (b1<=0){drapeau=1}
  if (lamda0<=-1/2){drapeau=1}
  if (ro<=0){drapeau=1}
  if (ro>=1){drapeau=1}
  
  if (is.na(g1)==TRUE){drapeau=1}else{
    if (g1>=1){drapeau=1}
    if (g1<=0){drapeau=1}
    if (abs(g1)==Inf){drapeau=1}
    if (1/abs(g1)==Inf){drapeau=1}
  }
  if (is.na(z2)==TRUE){drapeau=1}else{
    if (z2<0){drapeau=1}
    if (abs(z2)==Inf){drapeau=1}
    if (1/abs(z2)==Inf){drapeau=1}
  }
  if (is.na(z3)==TRUE){drapeau=1}else{
    if (z3<=0){drapeau=1}
    if (abs(z3)==Inf){drapeau=1}
    if (1/abs(z3)==Inf){drapeau=1}
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
VIX_Q<-function(para_h,h){
  tau = 250
  T_0=22
  
  # para_h<-c() set up the parameters of the model 
  a0=para_h[1]; a1=para_h[2]; gama=para_h[3];  b1= para_h[4] ; lamda0= para_h[5] ; ro=para_h[6]
  
  # Parameter under the physical probability
  lamda0star= -(1/2)
  gamastar= gama+lamda0+(1/2)
  
  g1= b1 + a1*(gamastar^2)           ####  The percistence
  h0=(a0 + a1)/(1 - g1)              ####  The first value for h, Unconditional Variance
  
  Psy = g1
  h_0 = h0
  
  drapeau=0
  if (a0<=0){drapeau=1}
  if (a1<=0){drapeau=1}
  if (gama<=0){drapeau=1}
  if (b1<=0){drapeau=1}
  if (lamda0<=-1/2){drapeau=1}
  if (ro<=0){drapeau=1}
  if (ro>=1){drapeau=1}
  
  if (is.na(h)==TRUE){drapeau=1}else{
    if (h<=0){drapeau=1}
    if (abs(h)==Inf){drapeau=1}
    if (1/abs(h)==Inf){drapeau=1}
  }
  if (is.na(g1)==TRUE){drapeau=1}else{
    if (g1>=1){drapeau=1}
    if (g1<=0){drapeau=1}
    if (abs(g1)==Inf){drapeau=1}
    if (1/abs(g1)==Inf){drapeau=1}
  }
  
  #  VIX 
  
  if (drapeau==0){
    resultat= 100*sqrt(tau*((h*((1-Psy^T_0)/((1-Psy)*T_0))) + h_0*(1-((1-Psy^T_0)/((1-Psy)*T_0)))))
  }else{
    resultat=NA
  }
  return(resultat)
  
}

###########################################################
#####       The Log-likeelihood over all Option        ####
###########################################################
Heston_likelihood_vix <- function(para_M, Data.returns,Data.ret){
  
  Data.ret.reduiced <- Data.ret[index_vix:length(Data.ret$VIX),].reset_index()
  row.names(Data.ret.reduiced) <- NULL
  Vix=Data.ret$VIX     ####  Call dividende
  

  ## set up the parameters of the model : para_M = c(para_distribution,para_h) 
  alpha=para_M[1];  beta=para_M[2];  delta=para_M[3];  mu=para_M[4]
  a0=para_M[5]; a1=para_M[6]; gama=para_M[7];  b1= para_M[8] ;  lamda0= para_M[9]  ; ro=para_M[10]
  
  para_h <- c(a0,a1,gama,b1,lamda0,ro)
  
  VIX_Market<-Vix
  
  Nvix=length(Vix)
  
  h = hstar(para_h,Data.returns)
  
  VIX_Model <- rep(NA, Nvix)
  for (i in 1:Nvix){
    VIX_Model[i]= VIX_Q(para_h,h[i+1])
  }
  
  # para_h<-c() set up the parameters of the model 
  a0=para_h[1]; a1=para_h[2]; gama=para_h[3];  b1= para_h[4] ; lamda0= para_h[5] ; ro=para_h[6]
  
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

