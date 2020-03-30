#####################################################
###         Parameters of the model           #######
#####################################################
### a0=para_h[1]; a1=para_h[2]; gama=para_h[3];  b1= para_h[4] ;  lamda0= para_h[5]  

para_h_NIG <-c(1.271996e-12, 1.522922e-06, 4.645827e+02 ,6.620047e-01,6.399961e-01, 9.646983e-01)


para_distribution_NIG <-c(48.3192334409, -5.3400989131,  0.0091622345,  0.0007211133)

###########################################################
###  Conditional variance without risk netral Proba    ####
###########################################################

h_p_vix_NIG  <- function(para_h_NIG , Data.returns)  
{
  ret=Data.returns$ret   
  rt=Data.returns$rt/250  
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

h_q_vix_NIG<-function(para_h_NIG,Data.returns){
  rt=Data.returns$rt/250  
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
  
  
  
  h_1=h_p_vix_NIG(para_h_NIG,Data.returns)
  

  h= c()
  mt= c()  
  A= c()   
  B= c()                                        
  theta= c()  
  h_star= c()

  
  for (i in 1:Z1){
    h[i]= h_1[i]
    mt[i]= -(h[i]/2)
    A[i]= ((alpha*mt[i]+ sqrt(delta*h[i])* beta* gama_d)^2)/(h[i]*delta*(gama_d^(3)))
    B[i] = ((4*(alpha^4)*(delta^2))/(h[i]*delta*(gama_d^(3)) + (alpha*mt[i]+sqrt(delta*h[i])* beta*gama_d)^2))-1
    theta[i]=  -1/2 - ((alpha* beta*sqrt(delta))/(sqrt(h[i])*(gama_d^(3/2)))) -(1/2)*((A[i]*B[i])^(1/2))
    h_star[i]= (delta*sqrt(h[i]))*((alpha/sqrt(h[i]))^2)/((((alpha/sqrt(h[i]))^2)-(((beta/sqrt(h[i]))+theta[i])^2))^3)
  }
      
  drapeau=0
  if (a0<=0){drapeau=1}
  if (a1<=0){drapeau=1}
  if (gama<=0){drapeau=1}
  if (b1<=0){drapeau=1}
  if (lamda0<=0){drapeau=1}
  
  if (drapeau==0){
    resultat=abs(h_star)
  }else{
    resultat=rep(NA, Z1)
  }
  
  return(resultat)
}
  
  
  
  


##############################################################
######     Conditional variance with risk netral Proba      ##
##############################################################
VRP_vix_NIG<-function(para_h_vix_NIG,Data.returns){
  rt=Data.returns$rt/250  
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
  
  
  
  


