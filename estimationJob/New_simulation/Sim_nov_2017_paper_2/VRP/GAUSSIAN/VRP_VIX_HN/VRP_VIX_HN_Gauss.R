#####################################################
###         Parameters of the model           #######
#####################################################
### a0=para_h[1]; a1=para_h[2]; gama=para_h[3];  b1= para_h[4] ;  lamda0= para_h[5]  

###para_h_Gauss<-c(1.874e-04,3.346e-04, 1.142e-00,1.125e-03,2.248e+02) ## RMSE2$rmse :   ## RMSE3$rmse :  



###para_h_Gauss<-c(1.271996e-12, 1.522922e-06, 4.645827e+02 ,6.620047e-01,6.399961e-01, 9.646983e-01)



###para_h_Gauss<-c( 5.881028e-07,  1.506407e-06,  4.550518e+02,  6.500114e-01,  8.596182e+00)


### para_h_Gauss<-c( 3.285e-05,  3.602e-04,  9.253e+00,  2.510e-01,  1.353e-06)

###########################################################
###  Conditional variance without risk netral Proba    ####
###########################################################

h_p_vix_gauss  <- function(para_h_Gauss, Data.returns)  
{
  ret=Data.returns$ret   
  rt=Data.returns$rt/250  
  Z1=length(rt)
  
  para_h=para_h_Gauss
  
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

h_q_vix_gauss<-function(para_h_Gauss,Data.returns){
  rt=Data.returns$rt/250  
  ret=Data.returns$ret         #### Returns : Data.BSJ$ret
  Z1=length(ret)
  
  
  para_h=para_h_Gauss
  
  ## set up the parameters of the model : para_h
  a0=para_h[1]; a1=para_h[2]; gama=para_h[3];  b1= para_h[4] ;  lamda0= para_h[5]   
  
  # Parameter under the physical probability
  lamda0star= -(1/2)
  gamastar= gama+lamda0+(1/2)
  
  
  h0=(a0 + a1)/(1 - b1 - a1*(gamastar)^2)    
  g0=b1 + a1*(gamastar)^2  
  
  
  h = c()                                       ####  A vector containing h from the model,
  h[1]=(a0 + a1)/(1 - b1 - a1*(gamastar)^2)     ####  The first value for h, Unconditional Variance
 
  for (i in 2:Z1){
    h[i]= a0 +b1*h[i-1]+a1*(((ret[i-1]-rt[i-1]-lamda0star*(h[i-1]))/(sqrt(h[i-1]))) - gamastar*(sqrt(h[i-1])))^2
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
VRP_vix_gauss<-function(para_h_vix_gauss,Data.returns){
  rt=Data.returns$rt/250  
  ret=Data.returns$ret         #### Returns : Data.BSJ$ret
  Z1=length(ret)
  
  
  para_h=para_h_Gauss
  
  ## set up the parameters of the model : para_h
  a0=para_h[1]; a1=para_h[2]; gama=para_h[3];  b1= para_h[4] ;  lamda0= para_h[5]   
  
  # Parameter under the risk neutral probability
  lamda0star= -(1/2)
  gamastar= gama+lamda0+(1/2)
  

  
  h_1=h_p_vix_gauss(para_h_Gauss,Data.returns)
  h_2=h_q_vix_gauss(para_h_Gauss,Data.returns)
  
  
  
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
  
  
  
  


