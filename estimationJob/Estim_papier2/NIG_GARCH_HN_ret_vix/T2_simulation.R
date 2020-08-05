##############################
#####  Simulation de H_t  ####
##############################
####################################################
######    The volatility updating ruleunder P     ##
####################################################
vol_sim_h <- function(para_h, para_distribution,h)
{
  ## set up the parameters of the model : para_h
  a0=para_h[1]; a1=para_h[2]; gama=para_h[3];  b1= para_h[4] ;  lamda0= para_h[5]   
  
  # para_distribution<-c() set up the parameters of NIG
  alpha=para_distribution[1];  beta=para_distribution[2];  delta=para_distribution[3];  mu=para_distribution[4];
  
  ## Normalisation :
  gamma_0 = sqrt((alpha^2) -(beta^2))
  sigma_z = (delta*(alpha^2))/((gamma_0)^3)
  mu_z= mu - ((delta*beta)/(gamma_0 ))
  
  ## Parametrization : 
  alpha_1 = alpha*(sigma_z)
  beta_1 =beta*(sigma_z)
  delta_1 =delta/(sigma_z)
  mu_1 =(1/(sigma_z))*(mu-mu_z)
  
  
  
  # Parameter under the physical probability
  h0=(a0 + a1)/(1 - b1 - a1*(gama)^2)    
  g0=b1 + a1*(gama)^2  
  
  drapeau=0

  if (is.na(h)==TRUE){drapeau=1}else{
    if (h<=0){drapeau=1}
    if (abs(h)==Inf){drapeau=1}
    if (1/abs(h)==Inf){drapeau=1}
  }
  
  if (drapeau==0){
    resultat= a0 +b1*h+a1*(rgh(1,alpha_1,beta_1,delta_1,mu_1,-1/2) - gama*(sqrt(h)))^2
  }else{
    resultat=NA
  }
  
  return(resultat)
}


####################################################
######         The volatility shape under P       ##
####################################################
shape_vol_sim <- function(para_h, para_distribution, N_t) {
  ## set up the parameters of the model : para_h
  a0=para_h[1]; a1=para_h[2]; gama=para_h[3];  b1= para_h[4] ;  lamda0= para_h[5]   
  
  # para_distribution<-c() set up the parameters of NIG
  alpha=para_distribution[1];  beta=para_distribution[2];  delta=para_distribution[3];  mu=para_distribution[4];
  
  N = N_t
  h = c()                                                        ####  A vector containing h from the model,
  h[1]=(a0 + a1)/(1 - b1 - a1*(gama)^2)                          ####  The first value for h, Unconditional Variance

  for (i in 2:N){
    h[i]= vol_sim_h(para_h, para_distribution,h[i-1])
    }
  
  return(h)  
}




###########################################################
#####  modified  Log-likeelihood    returns            ####
###########################################################
modified_Heston_likelihood_ret <- function(para_h, Data.returns, h) {
  ret=Data.returns$ret   
  rt=Data.returns$rt/250        
  Z1=length(rt)
  
  # para_h<-c() set up the parameters of the model 
  a0=para_h[1]; a1=para_h[2]; gama=para_h[3];  b1= para_h[4] ;  lamda0= para_h[5]  ; ro=para_h[6]

  dens = log(modified_Retdensity(para_h,ret[1],h[1],rt[1]))
  for (i in 2:Z1){
    temp=modified_Retdensity(para_h,ret[i],h[i],rt[i])
    dens<-dens+log(temp)
  }
  
  return(dens)  
}


###########################################################
#####  modified conditional density of the returns     ####
###########################################################
modified_Retdensity <- function(para_h,Ret,h,r)
{
  ## set up the parameters of the model : para_h
  a0=para_h[1]; a1=para_h[2]; gama=para_h[3];  b1= para_h[4] ;  lamda0= para_h[5] ; ro=para_h[6]
  
  Z0=b1 + a1*(gama^2)
  
  drapeau=0
  if (a0<=0){drapeau=1}
  if (a1<=0){drapeau=1}
  if (gama<=0){drapeau=1}
  if (b1<=0){drapeau=1}
  if (lamda0<=-1/2){drapeau=1}
  
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
    resultat= (1/(sqrt(2*pi*h)))*exp((-1/(2*h))*(((Ret-r-lamda0*h)^2)))
  }else{
    resultat=NA
  }
  return(resultat)
}






####################################################
######    The volatility updating ruleunder Q     ##
####################################################
vol_sim_h_Q <- function(para_h, para_distribution,h)
{
  ## set up the parameters of the model : para_h
  a0=para_h[1]; a1=para_h[2]; gama=para_h[3];  b1= para_h[4] ;  lamda0= para_h[5]   
  
  # para_distribution<-c() set up the parameters of NIG
  alpha=para_distribution[1];  beta=para_distribution[2];  delta=para_distribution[3];  mu=para_distribution[4];
  
  ## Normalisation :
  gamma_0 = sqrt((alpha^2) -(beta^2))
  sigma_z = (delta*(alpha^2))/((gamma_0)^3)
  mu_z= mu - ((delta*beta)/(gamma_0 ))
  
  ## Parametrization : 
  alpha_1 = alpha*(sigma_z)
  beta_1 =beta*(sigma_z)
  delta_1 =delta/(sigma_z)
  mu_1 =(1/(sigma_z))*(mu-mu_z)
  
  # Parameter under the physical probability
  lamda0star= -(1/2)
  gamastar= gama+lamda0+(1/2)

  # Parameter under the physical probability  
  g1= b1 + a1*(gamastar^2)           ####  The percistence
  h0=(a0 + a1)/(1 - g1)              ####  The first value for h, Unconditional Variance
  
  drapeau=0
  
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
  
  if (drapeau==0){
    resultat= a0 +b1*h+a1*(rgh(1,alpha_1,beta_1,delta_1,mu_1,-1/2) - gamastar*((h)^(1/2)))^2
  }else{
    resultat=NA
  }
  
  return(resultat)
}



####################################################
######         The volatility shape under Q       ##
####################################################
shape_vol_sim_Q <- function(para_h, para_distribution, N_t) {
  ## set up the parameters of the model : para_h
  a0=para_h[1]; a1=para_h[2]; gama=para_h[3];  b1= para_h[4] ;  lamda0= para_h[5]   
  
  # para_distribution<-c() set up the parameters of NIG
  alpha=para_distribution[1];  beta=para_distribution[2];  delta=para_distribution[3];  mu=para_distribution[4];
  
  # Parameter under the physical probability
  lamda0star= -(1/2)
  gamastar= gama+lamda0+(1/2)
  
  g1= b1 + a1*(gamastar^2)           ####  The percistence
  h0=(a0 + a1)/(1 - g1)              ####  The first value for h, Unconditional Variance
  
  
  N = N_t
  h = c()                            ####  A vector containing h from the model,
  h[1]=h0                            ####  The first value for h, Unconditional Variance
  
  for (i in 2:N){
    h[i]= vol_sim_h_Q(para_h, para_distribution,h[i-1])
  }
  return(h)  
}



####################################################
######         Dimulation VIX ubder Q             ##
####################################################
shape_VIX_sim <- function(para_h, para_distribution, N_t) {
  ## set up the parameters of the model : para_h
  a0=para_h[1]; a1=para_h[2]; gama=para_h[3];  b1= para_h[4] ;  lamda0= para_h[5]   

  Nvix= N_t
  h = shape_vol_sim_Q(para_h, para_distribution, N_t)
  
  VIX_Model <- rep(NA, Nvix)
  for (i in 1:Nvix){
    VIX_Model[i]= VIX_Q(para_h,h[i+1])
  }
  
  return(h)  
}


###########################################################
#####      modified Log-likeelihood over all VIX       ####
###########################################################
Heston_likelihood_vix <- function(para_h, Data.returns,Data.ret, h){
  
  ## set up the parameters of the model : para_h
  a0=para_h[1]; a1=para_h[2]; gama=para_h[3];  b1= para_h[4] ;  lamda0= para_h[5] ; ro=para_h[6]
  
  
  Data.ret.reduiced <- Data.ret[index_vix:length(Data.ret$VIX),]
  row.names(Data.ret.reduiced) <- NULL
  Vix=Data.ret.reduiced$VIX     ####  Call dividende
  
  
  VIX_Market<-Vix
  
  Nvix=length(Vix)
  
  
  h_all= hstar(para_h,Data.returns)
  h = h_all[index_ht:length(h_all)] 
  
  
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








