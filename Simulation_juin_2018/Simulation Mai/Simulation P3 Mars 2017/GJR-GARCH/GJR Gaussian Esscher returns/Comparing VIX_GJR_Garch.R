####################################################
######         The volatility updating rule       ##
####################################################
h_P<-function(para_h,Data.ret){
    ret=Data.returns$ret   
    rt=Data.returns$rt/250        
    Z1=length(rt)
    
    # para_h<-c() set up the parameters of the model 
    a0=para_h[1]; a1=para_h[2]; a2=para_h[3];  b1= para_h[4] ;  lamda0= para_h[5]
    
    
    h = c()                                                        ####  A vector containing h from the model,
    h[1]=(a0 )/(1 - b1 - a1- a2/2)                        ####  The first value for h, Unconditional Variance
    
    for (i in 2:Z1){
      h[i]= gsqrt(para_h,ret[i-1],h[i-1],rt[i-1]) ### a0 +b1*h[i-1]+a1*(((ret[i-1]-rt[i-1]-lamda0*(h[i-1]))/(sqrt(h[i-1]))) - a2*(sqrt(h[i-1])))^2
    }
    
    return(h)  
  }
  

######################
######     VIX      ##
######################
VIX_Q<-function(para_h,h){
  tau = 250
  T_0=22
  
  # para_h<-c() set up the parameters of the model 
  a0=para_h[1]; a1=para_h[2]; a2=para_h[3];  b1= para_h[4] ;  lamda0= para_h[5]  
  
  g1= b1+ (a1+a2*(pnorm(lamda0)))*(1+lamda0^2)+a2*lamda0*dnorm(lamda0)            ####  The percistence
  h0=(a0 )/(1 - g1)                                                               ####  The first value for h, Unconditional Variance
  
  Psy = g1
  h_0 = h0
  
  drapeau=0
  if (a0<=0){drapeau=1}
  if (a1<=0){drapeau=1}
  if (a2<=0){drapeau=1}
  if (b1<=0){drapeau=1}

  
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
  
  #  VIX 
  
  if (drapeau==0){
    resultat= 100*sqrt(tau*((h*((1-Psy^T_0)/((1-Psy)*T_0))) + h_0*(1-((1-Psy^T_0)/((1-Psy)*T_0)))))
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
  a0=para_h[1]; a1=para_h[2]; a2=para_h[3];  b1= para_h[4] ;  lamda0= para_h[5]  ; ro=para_h[6]
  
  VIX_Market<-Vix
  
  Nvix=length(Vix)
  
  h = h_P(para_h,Data.returns)
  
  VIX_Model <- rep(NA, Nvix)
  for (i in 1:Nvix){
    VIX_Model[i]= VIX_Q(para_h,h[i+1])
  }  
  
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
