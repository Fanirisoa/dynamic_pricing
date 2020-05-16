#####################################################
###         Parameters of the model           #######
#####################################################
### a0=para_h[1]; a1=para_h[2]; gama=para_h[3];  b1= para_h[4] ;  lamda0= para_h[5]  
para_h_NIG = para_h1


para_distribution_NIG = para_distribution1

###########################################################
###  Conditional variance without risk netral Proba    ####
###########################################################

h_p_vix_NIG  <- function(para_h_NIG , Data.returns)  
{
  ret=Data.returns$ret   
  #  rt=Data.returns$rt/250     
  rt=(0.05+ Data.returns$rt - Data.returns$rt)
  Z1=length(rt)
  
  para_h=para_h_NIG 
  
  ## set up the parameters of the model : para_h
  a0=para_h[1]; a1=para_h[2]; gama=para_h[3];  b1= para_h[4] ;  lamda0= para_h[5]   
  
  # Parameter under the physical probability
  h0=(a0 + a1)/(1 - b1 - a1*(gama)^2)    
  g0=b1 + a1*(gama)^2  
  
  
  h = c()                                  ####  A vector containing h from the model,
  h[1]=(a0 + a1)/(1 - b1 - a1*(gama)^2)    ####  The first value for h, Unconditional Variance
  
  
  
  for (i in 2:Z1){
    h[i]= a0 +b1*h[i-1]+a1*(((ret[i-1]-rt[i-1]-lamda0*(h[i-1]))/((h[i-1])^(1/2))) - gama*((h[i-1])^(1/2)))^2
  }
  
  drapeau=0
  if (a0<=0){drapeau=1}
  if (a1<=0){drapeau=1}
  if (gama<=0){drapeau=1}
  if (b1<=0){drapeau=1}
  if (lamda0<=0){drapeau=1}
  
  if (is.na(g0)==TRUE){drapeau=1}else{
    if (g0>=1){drapeau=1}
    if (abs(g0)==Inf){drapeau=1}
    if (1/abs(g0)==Inf){drapeau=1}
  }

  if (is.na(h0)==TRUE){drapeau=1}else{
    if (h0<=0){drapeau=1}
    if (abs(h0)==Inf){drapeau=1}
    if (1/abs(h0)==Inf){drapeau=1}
  }

  if (drapeau==0){
    resultat=abs(h)
  }else{
    resultat=rep(NA, Z1)
  }
  
  return(resultat)
}



##############################################################
######     Conditional variance with risk netral Proba      ##
##############################################################
##############################################################
######     Conditional variance with risk netral Proba      ##
##############################################################

h_q_vix_NIG<-function(para_h_NIG,Data.returns){
  # rt=Data.returns$rt/250     
  rt=(0.05+ Data.returns$rt - Data.returns$rt)
  ret=Data.returns$ret         #### Returns : Data.BSJ$ret
  Z1=length(ret)
  
  
  para_h=para_h_NIG
  para_distribution=para_distribution_NIG
  
  
  ## set up the parameters of the model : para_h
  a0=para_h[1]; a1=para_h[2]; gama=para_h[3];  b1= para_h[4] ;  lamda0= para_h[5]   
  
  # para_distribution<-c() set up the parameters of the NIG distribution under P
  alpha=para_distribution[1];  beta=para_distribution[2];  delta=para_distribution[3];  mu=para_distribution[4]
  
  # gama Constrainte GH distribution
  gama_d=(alpha^2-beta^2)^(1/2)
  
  gamma_0 = sqrt((alpha^2) -(beta^2))
  
  ## Normalisation :
  sigma_z = (delta*(alpha^2))/((gamma_0)^3)
  
  mu_z= mu - ((delta*beta)/(gamma_0 ))
  
  ## Parametrization : 
  

  
  
  h_1=h_p_vix_NIG(para_h_NIG,Data.returns)
  
  
  h= c()
  mt= c()  
  A= c()   
  B= c()                                        
  theta= c()  
  h_star= c()
  
  
  lambda=-0.5
  
  h_1=h_p_vix_NIG(para_h_NIG,Data.returns)
  
  
  h= c()
  mt= c()  
  A= c()   
  B= c()                                        
  theta= c()  
  gama_N= c()  
  
  arg= c()   
  
  K_1= c()                                        
  K_2= c()  
  K_3= c()
  C_1= c()                                        
  C_2= c()  
  C_3= c()
  h_star= c()
  
  
  for (i in 1:Z1){
    i=1
    
    h[i]= h_1[i]
    mt[i]= lamda0*((h[i])^(1/2))- (h[i])/2
    A[i]= ((alpha*mt[i]+ sqrt(delta*h[i])* beta* gama_d)^2)/(h[i]*delta*(gama_d^(3)))
    B[i] = ((4*(alpha^4)*(delta^2))/(h[i]*delta*(gama_d^(3)) + (alpha*mt[i]+sqrt(delta*h[i])* beta*gama_d)^2))-1
    theta[i]=  -1/2 - ((alpha* beta*sqrt(delta))/(sqrt(h[i])*(gama_d^(3/2)))) -(1/2)*((A[i]*B[i])^(1/2))
    gama_N[i]=sqrt(alpha^2 -( beta+(sqrt(h[i])*theta[i]))^2)
    
    arg[i]=delta*gama_N[i]
    
    K_1[i]=besselK(arg[i], lambda +1, expon.scaled = TRUE)
    K_2[i]=besselK(arg[i], lambda, expon.scaled = TRUE)
    K_3[i]=besselK(arg[i], lambda+2, expon.scaled = TRUE)
    
    C_1[i]=(delta*K_1[i])/(gama_N[i]*K_2[i])
    C_2[i]=(K_3[i])/(K_2[i])
    C_3[i]=((K_1[i])^2)/((K_2[i])^2)
    
    h_star[i]= h[i]*(C_1[i]+ (((delta^2)*(beta+sqrt(h[i])*theta[i])^2)/(gama_N[i]^2))*(C_2[i] - C_3[i]))
    
  }
  
  
  drapeau=0
  if (a0<=0){drapeau=1}
  if (a1<=0){drapeau=1}
  if (gama<=0){drapeau=1}
  if (b1<=0){drapeau=1}
  
  if (drapeau==0){
    resultat=abs(h_star)+0.0032
  }else{
    resultat=rep(NA, Z1)
  }
  
  return(resultat)
}
  
  
  
  


##############################################################
######     Conditional variance with risk netral Proba      ##
##############################################################
VRP_vix_NIG<-function(para_h_vix_NIG,Data.returns){
  #  rt=Data.returns$rt/250     
  rt=(0.05+ Data.returns$rt - Data.returns$rt)
  ret=Data.returns$ret         #### Returns : Data.BSJ$ret
  Z1=length(ret)
  
  
  para_h=para_h_NIG
  
  ## set up the parameters of the model : para_h
  a0=para_h[1]; a1=para_h[2]; gama=para_h[3];  b1= para_h[4] ;  lamda0= para_h[5]   
  

  h_1=h_p_vix_NIG(para_h_NIG,Data.returns)
  h_2=h_q_vix_NIG(para_h_NIG,Data.returns)
  
  
  
  VRP = c()                                                                ####  A vector containing h from the model,
  VRP[1]=  sqrt(h_1[1])-sqrt(h_2[1])                ####  The first value for VRP,
  for (i in 2:Z1){
    VRP[i]=  sqrt(h_1[i])-sqrt(h_2[i]) 
  }
  
  drapeau=0
  

  if (a0<=0){drapeau=1}
  if (a1<=0){drapeau=1}
  if (gama<=0){drapeau=1}
  if (b1<=0){drapeau=1}
  if (lamda0<=0){drapeau=1}

  if (drapeau==0){
    resultat=-VRP
  }else{
    resultat=rep(NA, Z1)
  }
  return(resultat)
}
  
  
  
  


