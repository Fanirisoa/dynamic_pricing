######################################################################
######         Compute option prices using MC simulation            ##
######################################################################
Pricer<-function(N,para_h1,Data.N){
  ## This function compute option prices by MC, using the function "h" and "sim"
  ## para_h is a vector containing the volatility's parameters
  ## para_distribution is a vector containing the distribution's parameters
  # para_h<-c() set up the parameters of the model 
  
  # para_h<-c() set up the parameters of the model 
  a0=para_h[1]; b1=para_h[2]; a1=para_h[3];  gama= para_h[4]; lambda= para_h[5]; a=para_h[6]; b=para_h[7] ## ; c=para_h[5]; d=para_h[6] 
  
  ## Mean 0 and Variance 1
  c0=a^2 - b^2
  c=((sqrt(a^2 - b^2))^(3/2))/a
  d=(-b/a)*(sqrt(a^2 - b^2))^(1/2)
  
  
  # Parameter under the physical probability
  h0=a0/(1- (b1+a1*(1+gama^2)))    
  b0=abs(b)
  
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
  
  Y_t= lapply(1:Z1, function(x) Matrice_ret(x,para_h1))
  
  
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

