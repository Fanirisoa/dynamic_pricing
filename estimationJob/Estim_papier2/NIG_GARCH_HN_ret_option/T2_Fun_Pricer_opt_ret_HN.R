######################################################################
######         Compute option prices using MC simulation            ##
######################################################################
Pricer_P<-function(N,para_h1,para_distribution1,Data.N){
  ## This function compute option prices by MC, using the function "h" and "sim"
  ## para_h is a vector containing the volatility's parameters
  ## para_distribution is a vector containing the distribution's parameters
  # para_h<-c() set up the parameters of the model 
  
  a0=para_h1[1]; a1=para_h1[2]; gama=para_h1[3];  b1= para_h1[4] ;  lamda0= para_h1[4] 
  alpha=para_distribution1[1] ; beta=para_distribution1[2] ; delta=para_distribution1[3] ; mu=para_distribution1[4]
  
  # Parameter under the physical probability
  lamda0star= -(1/2)
  gamastar= gama+lamda0+(1/2)
  
  T=Data.N$T       ####  Time to maturity expressed in terms of years 
  S=Data.N$S       ####  Prix du sous-jacent: Data.contract$S
  K=Data.N$K       ####  Strike  Prix d'exercice: data$strike
  r=Data.N$r/250   ####  Interest rate Data.contract$r
  Z1=length(r)
  
  print(paste0("value of N : ", 15000))

  
  T=Data.N$T       
  T=T*250
  T=round(T,0)     ####  Time to maturity expressed in terms of days
  
  ######################################     
  ##   Step 1 : Sampling the returns  ## 
  ######################################  
  
  t1_start.time <- Sys.time()
  Y_t= lapply(1:Z1, function(x) Matrice_ret(x,para_h1,para_distribution1))
  t1_end.time <- Sys.time()
  t1.taken <- t1_end.time -   t1_start.time
  
  print(paste0("The value of T1 for Step 1 : Sampling the returns  : ", t1.taken))
  #####################################################     
  ## Step 2 : turning returns into Monte-Carlos Prices#
  ##################################################### 
  

  
  t2_start.time <- Sys.time()
  St= MC_Sim_St(Y_t)
  t2_end.time <- Sys.time()
  t2.taken <- t2_end.time -   t2_start.time
  
  print(paste0("The value of T2 for Step 2 : turning returns into Monte-Carlos Prices  : ", t2.taken))
  
  ##############################################     
  ## Step 3 : Martingalisation of the sample  ##
  ##############################################  
  
  t3_start.time <- Sys.time()
  St_Mar=Mar_St(St)
  t3_end.time <- Sys.time()
  t3.taken <- t3_end.time -   t3_start.time
  
  print(paste0("The value of T3 for Step 3 : Martingalisation of the sample  : ", t3.taken))
  

  
  ################################################     
  ## Step 4 : Computation of the option prices  ##
  ################################################    
  t4_start.time <- Sys.time()
  P=P_T(St_Mar)
  t4_end.time <- Sys.time()
  t4.taken <- t4_end.time -   t4_start.time
  
  print(paste0("The value of T3 for Step 4 : Computation of the option prices : ", t4.taken))
  

  
  return(list(P=P,Yt=Y_t,St=St, St_Mar=St_Mar))
} 

