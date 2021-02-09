##############################
#####  Simulation de H_t  ####
##############################
########################################
######   Generate the residual z_t    ##
########################################
z_sim <- function(para_h, para_distribution,N_t)
{
  ## N_t is the length of the simulation
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
######    The volatility updating rule under P    ##
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


##############################
#####     Compute S_T     ####
##############################

val_S_T<-function(Y,S_0){  
  # Y y_t return values at time t
  a = sum(Y)
  b = exp(a)
  return(b*S_0) 
}


#################################################
####   Generate   dataset of option Data.N   ####
#################################################

vec_S_T<-function(St,l){  
  # St is the value of S_T
  # this function return list of constant values : St of length : l
  return(rep(St, l)) 
}


########################################################
###      Generate  values of the strike : K      #######
########################################################

vec_K<-function(SK,St){  
  # SK  vector of values of the moneyness  S/K  
  # this function return list of constant values : St of length : l
  K=c()    
  for(i in 1:length(SK)) {
    K[i] = St/SK[i]
  }
  return(K) 
}


ger_Data.N<-function(l,r, S_T,list.SK,list.T){  
  # SK  vector of values of the moneyness  S/K  
  # this function return list of constant values : St of length : l
  list.S <- vec_S_T(S_T,l)
  list.K <- vec_K(list.SK,S_T)
  list.ttm <- list.T/250
  dataset_option <-  data.frame(expand.grid(K = list.K, T = list.ttm, S = S_T, r = r))
  return(dataset_option) 
}

# 
# ######################################################################
# ######         Compute option prices using MC simulation            ##
# ######################################################################
# Pricer<-function(N,para_h1,para_distribution1,Data.N){
#   ## This function compute option prices by MC, using the function "h" and "sim"
#   ## para_h is a vector containing the volatility's parameters
#   ## para_distribution is a vector containing the distribution's parameters
#   # para_h<-c() set up the parameters of the model 
#   
#   a0=para_h1[1]; a1=para_h1[2]; gama=para_h1[3];  b1= para_h1[4] ;  lamda0= para_h1[4] 
#   alpha=para_distribution1[1] ; beta=para_distribution1[2] ; delta=para_distribution1[3] ; mu=para_distribution1[4]
#   
#   # Parameter under the physical probability
#   lamda0star= -(1/2)
#   gamastar= gama+lamda0+(1/2)
#   
#   T=Data.N$T       ####  Time to maturity expressed in terms of years 
#   S=Data.N$S       ####  Prix du sous-jacent: Data.contract$S
#   K=Data.N$K       ####  Strike  Prix d'exercice: data$strike
#   r=Data.N$r/250   ####  Interest rate Data.contract$r
#   Z1=length(r)
#   
#   
#   T=Data.N$T       
#   T=T*250
#   T=round(T,0)     ####  Time to maturity expressed in terms of days
#   
#   ######################################     
#   ##   Step 1 : Sampling the returns  ## 
#   ######################################  
#   
#   ##Y_t= lapply(1:Z1, function(x) Matrice_ret(x,para_h1,para_distribution1))
#   
#   #####################################################     
#   ## Step 2 : turning returns into Monte-Carlos Prices#
#   ##################################################### 
#   
#   St= MC_Sim_St(Y_t)
#   
#   ##############################################     
#   ## Step 3 : Martingalisation of the sample  ##
#   ##############################################  
#   
#   St_Mar=Mar_St(St)
#   
#   ################################################     
#   ## Step 4 : Computation of the option prices  ##
#   ################################################    
#   
#   P=P_T(St_Mar)
#   
#   return(list(P=P,Yt=Y_t,St=St, St_Mar=St_Mar))
# } 
# 
# 
# 
# 
# 
# 
