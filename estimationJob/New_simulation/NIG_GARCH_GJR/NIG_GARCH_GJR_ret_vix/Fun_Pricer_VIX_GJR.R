######################################################################
######         Compute option prices using MC simulation            ##
######################################################################
Pricer<-function(N,para_h1,para_distribution1,Data.N){
  ## This function compute option prices by MC, using the function "h" and "sim"
  ## para_h is a vector containing the volatility's parameters
  ## para_distribution is a vector containing the distribution's parameters
  # para_h<-c() set up the parameters of the model 
 
  a0=para_h1[1]; a1=para_h1[2]; a2=para_h1[3];  b1= para_h1[4] ;  lamda0= para_h1[5]  ; ro=para_h1[6]
  
  T=Data.N$T       ####  Time to maturity expressed in terms of years 
  S=Data.N$S       ####  Prix du sous-jacent: Data.contract$S
  K=Data.N$K       ####  Strike  Prix d'exercice: data$strike
  r=Data.N$r/250   ####  Interest rate Data.contract$r
  Z1=length(r)
  
  
  T=Data.N$T       
  T=T*250
  T=round(T,0)     ####  Time to maturity expressed in terms of days
  
  ######################################     
  ##   Step 1 : Sampling the returns  ## 
  ######################################  
  
  Y_t= lapply(1:Z1, function(x) Matrice_ret(x))
  
  
  #####################################################     
  ## Step 2 : turning returns into Monte-Carlos Prices#
  ##################################################### 
  
  St= MC_Sim_St(Y_t)
  
  ##############################################     
  ## Step 3 : Martingalisation of the sample  ##
  ##############################################  
   
  St_Mar=Mar_St(St)
  
  ################################################     
  ## Step 4 : Computation of the option prices  ##
  ################################################    
  
  P=P_T(St_Mar)
  
  return(list(P=P,Yt=Y_t,St=St, St_Mar=St_Mar))
} 

