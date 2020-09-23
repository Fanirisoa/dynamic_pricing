##############################
#####  Simulation de H_t  ####
##############################
####################################################
######    The volatility updating ruleunder P     ##
####################################################
z_sim <- function(para_h, para_distribution,N_t)
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
  
  z = c()     
  for (i in 1:N_t){
    z[i]= rgh(1,alpha_1,beta_1,delta_1,mu_1,-1/2)
  }
  
  return(z)
}

####################################################
######    The volatility updating ruleunder P     ##
####################################################
vol_sim_h <- function(para_h, para_distribution,h,z)
{
  ## set up the parameters of the model : para_h
  a0=para_h[1]; a1=para_h[2]; gama=para_h[3];  b1= para_h[4] ;  lamda0= para_h[5]   
  

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
    resultat= a0 +b1*h+a1*(z - gama*(sqrt(h)))^2
  }else{
    resultat=NA
  }
  
  return(resultat)
}


####################################################
######         The volatility shape under P       ##
####################################################
shape_vol_sim <- function(para_h, para_distribution,z_sim, N_t) {
  ## set up the parameters of the model : para_h
  a0=para_h[1]; a1=para_h[2]; gama=para_h[3];  b1= para_h[4] ;  lamda0= para_h[5]   
  
  # para_distribution<-c() set up the parameters of NIG
  alpha=para_distribution[1];  beta=para_distribution[2];  delta=para_distribution[3];  mu=para_distribution[4];
  
  N = N_t
  h = c()                                                        ####  A vector containing h from the model,
  h[1]=(a0 + a1)/(1 - b1 - a1*(gama)^2)                          ####  The first value for h, Unconditional Variance

  for (i in 2:N){
    h[i]= vol_sim_h(para_h, para_distribution,h[i-1],z_sim[i-1])
    }
  
  return(h)  
}

####################################
#####  Simulation of  returns   ####
####################################
ret_simulation <- function(para_h, para_distribution,z_sim, h) {
  n=length(h)
  # para_h<-c() set up the parameters of the model 
  a0=para_h[1]; a1=para_h[2]; gama=para_h[3];  b1= para_h[4] ;  lamda0= para_h[5]  ; ro=para_h[6]
  
  rt=0.0001197619
  ret  = c()                            ####  A vector containing h from the model,
  for (i in 1:n){
    ret[i]= rt  +lamda0*(h[i]) + ((h[i])^(1/2))*z_sim[i]
  }
  
  return(ret)  
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
  
  return(VIX_Model)  
}


##################################################################
#####       Comparing predictibility of time series VIX       ####
##################################################################
Compa_vix <- function(VIX_Model,VIX_Market) {
  VIX_Market<-Vix
  Nvix=length(Vix)

  ## Error terms :
  
  error <- rep(NA, Nvix)
  error[Nvix]=0
  for (i in 1:Nvix-1){
    error[i]= (VIX_Model[i]/ VIX_Market[i]) - 1
  }
  ## MPE :
  
  MPE_Vix <- rep(NA, Nvix)
  MPE_Vix[Nvix]=0
  for (i in 1:Nvix-1){
    MPE_Vix[i]= (VIX_Model[i]*(1/VIX_Market[i]))-1
  }
  
  MPE<- (mean(MPE_Vix))
  
  ## MAE :
  
  MAE_Vix <- rep(NA, Nvix)
  MAE_Vix[Nvix]=0
  for (i in 1:Nvix-1 ){
    MAE_Vix[i]= abs((VIX_Market[i]*(1/VIX_Model[i]))-1)
  }
  
  MAE<- (mean(MAE_Vix))
  
  ## RMSE_Vix :
  RMSE_Vix <- rep(NA, Nvix)
  RMSE_Vix[Nvix]=0
  for (i in 1:Nvix-1){
    RMSE_Vix[i]= (error[i])^2
  }
  
  Vrmse<-sqrt((mean(RMSE_Vix)))
  
  ## MAE :
  MAE2<- (mean(abs(error)))
  
  mse<-mean(RMSE_Vix)
  
  return(list(MPE=MPE, MAE=MAE ,MAE2=MAE2 ,Vrmse=Vrmse, MSE_VIX=mse))   
}

