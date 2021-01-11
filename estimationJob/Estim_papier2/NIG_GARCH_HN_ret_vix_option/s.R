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

/Users/leafanirisoa/Documents/GitHub/dynamic_pricing/estimationJob/Estim_papier2/NIG_GARCH_HN_ret_vix/T2_Met_VIX_HN.R