##############################################################
######     Conditional variance with risk netral Proba      ##
##############################################################
h_vol<-function(para_h,h,ret,rt){
  
  # para_h<-c() set up the parameters of the model 
  a0=para_h[1]; b1=para_h[2]; a1=para_h[3];  gama= para_h[4]; lambda= para_h[5] 
  
  # Parameter under the physical probability
  h0=a0/(1- (b1+a1*(1+gama^2)))    

  
  
  a0=para_h[1]; b1=para_h[2]; a1=para_h[3];  gama= para_h[4]; lambda= para_h[5]
  
  g1=(b1+a1*(1+gama^2))
  gamastar =  gama+lambda
  g0=b1 + a1*(gamastar)^2  
  
  # Parameter under the physical probability
  h0=a0/(1- (b1+a1*(1+gama^2)))    
  
  
  drapeau=0
  if (a0<=0){drapeau=1}
  if (b1<=0){drapeau=1}
  if (a1<=0){drapeau=1}
  if (g0<=0.7){drapeau=1}
  if (g1<=0.7){drapeau=1}
  if (g0>=0.996132){drapeau=1}
  if (g1>=0.996132){drapeau=1}
  if (gama<=0){drapeau=1}
  
  if (is.na(h0)==TRUE){drapeau=1}else{
    if (h0<=0){drapeau=1}
    if (abs(h0)==Inf){drapeau=1}
    if (1/abs(h0)==Inf){drapeau=1}
  }
  
  if (drapeau==0){
    resultat= a0 +b1*h+a1*h*(((ret-rt- lambda*sqrt(h)+(1/2)*h)/(sqrt(h)))-lambda-gama)^2
  }else{
    resultat=NA
  }
  return(resultat)
}

####################################################
######         The volatility updating rule       ##
####################################################
h_P<-function(para_h,Data.ret){
  rt=Data.ret$rt/250        #### Interest rate Data : Data.BSJ$rt
  ret =Data.ret$ret         #### Returns : Data.BSJ$ret
  Z1=length(ret)
  
  
  
  # para_h<-c() set up the parameters of the model 
  a0=para_h[1]; b1=para_h[2]; a1=para_h[3];  gama= para_h[4]; lambda= para_h[5]
  # Parameter under the physical probability
  h0=a0/(1- (b1+a1*(1+gama^2)))    

  
  h = c()                                                        ####  A vector containing h from the model,
  h[1]= h0                                                       ####  The first value for h,
  for (i in 2:Z1){
    h[i]=h_vol(para_h,h[i-1],ret[i-1],rt[i-1])
  }
  
  #  
  drapeau=0
  if (a0<=0){drapeau=1}
  if (b1<=0){drapeau=1}
  if (a1<=0){drapeau=1}
  if (is.na(h0)==TRUE){drapeau=1}else{
    if (h0<=0){drapeau=1}
    if (abs(h0)==Inf){drapeau=1}
    if (1/abs(h0)==Inf){drapeau=1}
  }
  
  if (drapeau==0){
    resultat=h 
  }else{
    resultat=rep(NA, Z1)
  }
  return(resultat)
}

######################
######     VIX      ##
######################
VIX_Q<-function(para_h,h,Ret,r){
  tau = 250
  T_0=22
  
  # para_h<-c() set up the parameters of the model 
  a0=para_h[1]; b1=para_h[2]; a1=para_h[3];  gama= para_h[4]; lambda= para_h[5]
  
  # Parameter under the physical probability
  h0=a0/(1- (b1+a1*(1+gama^2)))    
  g1=(b1+a1*(1+gama^2))
  gamastar =  gama+lambda
  g0=b1 + a1*(gamastar)^2  
  
    drapeau=0
  if (a0<=0){drapeau=1}
  if (b1<=0){drapeau=1}
  if (a1<=0){drapeau=1}
  if (g0<=0.7){drapeau=1}
  if (g1<=0.7){drapeau=1}
  if (g0>=0.996132){drapeau=1}
  if (g1>=0.996132){drapeau=1}
  if (gama<=0){drapeau=1}
  
  if (is.na(h0)==TRUE){drapeau=1}else{
    if (h0<=0){drapeau=1}
    if (abs(h0)==Inf){drapeau=1}
    if (1/abs(h0)==Inf){drapeau=1}
  }
  
  Psy = b1+a1*(1+(lambda+gama)^2)
  #  VIX 
  
  if (drapeau==0){
    resultat= 100*sqrt(tau/T_0)*sqrt(a0*((22/(1-Psy))- ((1-(Psy^T_0))/(1-Psy)^2))+ h*((1-(Psy^T_0))/(1-Psy)))
  }else{
    resultat=NA
  }
  return(resultat)
  
}

##################################################################
#####       Comparing predictibility of time series VIX       ####
##################################################################
Compa_vix <- function(para_h,Data.returns) {
  Vix=Data.returns$VIX      ####  Call dividende
  ret =Data.returns$ret     #### Returns : Data.BSJ$ret
  rt=Data.returns$rt/250  
  
  # para_h<-c() set up the parameters of the model 
  a0=para_h[1]; b1=para_h[2]; a1=para_h[3];  gama= para_h[4]; lambda= para_h[5]
  
  
  # Parameter under the physical probability
  h0=a0/(1- (b1+a1*(1+gama^2)))    

  VIX_Market<-Vix
  
  
  Nvix=length(Vix)
  
  h = h_P(para_h,Data.returns)
  
  
  VIX_Model <- rep(0, Nvix)
  for (i in 1:Nvix){
    VIX_Model[i]= VIX_Q(para_h,h[i+1])
  }  
  
  
  VIX_Model=VIX_Model[2241: 2717]
  VIX_Market=VIX_Market[2241: 2717]
  
  N_VIX_2012= length(VIX_Model)
  
  #a= (1/sqrt((1/N_VIX_2012)*sum((VIX_Market)^2)))*100
  Norm_b= (1/sqrt((1/N_VIX_2012)*sum((VIX_Market)^2)))*100
  Norm_a= 100/mean(VIX_Market)
  ## Error terms :
  
  error <- rep(0,N_VIX_2012)
  #error[Nvix]=0
  for (i in 1:N_VIX_2012-1){
    error[i]= (VIX_Model[i]/ VIX_Market[i]) - 1
  }
  ## MPE :
  
  MPE_Vix <- rep(NA, N_VIX_2012)
  MPE_Vix[N_VIX_2012]=0
  for (i in 1:N_VIX_2012-1){
    MPE_Vix[i]= (VIX_Model[i]*(1/VIX_Market[i]))-1
  }
  
  MPE<-  Norm_a*(mean(MPE_Vix))
  
  ## MAE :
  
  MAE_Vix <- rep(NA, N_VIX_2012)
  MAE_Vix[N_VIX_2012]=0
  for (i in 1:N_VIX_2012-1 ){
    MAE_Vix[i]= abs((VIX_Model[i]*(1/VIX_Market[i]))-1)
  }
  
  MAE<-  Norm_a*(mean(MAE_Vix))
  
  ## RMSE_Vix :
  RMSE_Vix <- rep(NA, N_VIX_2012)
  RMSE_Vix[N_VIX_2012]=0
  for (i in 1:N_VIX_2012-1){
    RMSE_Vix[i]= (VIX_Market[i]-VIX_Model[i])^2
  }
  
  Vrmse<- Norm_b*sqrt((mean(RMSE_Vix)))
  
  ## MAE :
  MAE2<-  Norm_a*(mean(abs(error)))
  
  mse<- Norm_b*mean(RMSE_Vix)
  
  return(list(MPE=MPE, MAE=MAE ,MAE2=MAE2 ,Vrmse=Vrmse, MSE_VIX=mse,VIX_Market=VIX_Market,VIX_Model=VIX_Model))   
}
