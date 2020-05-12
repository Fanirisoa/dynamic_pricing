####################################################
##### Real cube root of a negative number in R    ##
####################################################
cbrt <- function(x) {
  sign(x) * abs(x)^(1/3)
}
##############################################################
######     Conditional variance with risk netral Proba      ##
##############################################################
h<-function(para_h,Data.returns){
  rt=Data.returns$rt/250        #### Interest rate Data : Data.BSJ$rt
  ret =Data.returns$ret         #### Returns : Data.BSJ$ret
  Z1=length(ret)
  
  # para_h<-c() set up the parameters (physical probability) of the model 
  w=para_h[1]; b=para_h[2]; a=para_h[3];  c= para_h[4]; neta=para_h[5] ; nu=para_h[6]  ; PI=para_h[7] ; ro=para_h[8]
  
  
  # Variable of risk neutral
  neta0=cbrt(((PI/nu)^2)*(-1+ sqrt(1+(8*nu)/(27*PI)))) + cbrt(((PI/nu)^2)*(-1-sqrt(1+(8*nu)/(27*PI)))) 
  nu0= nu/PI
  w0=w*PI; b0=b; a0=(a*neta)/(neta0*PI);  c0=(c*neta0*PI)/neta
  
  
  
  h_star = c()                                                                ####  A vector containing h from the model,
  h_star[1]=(w0 + a0*(neta0^4))/(1 - a0*(neta0^2) - b0 - (c0*(neta0^(-2))))   ####  The first value for h,
  for (i in 2:Z1){
    h_star[i]=w0+b0*h_star[i-1]+ c0*(neta0^(-1))*(ret[i-1]-rt[i-1]-(nu0*h_star[i-1]))+((a0*neta0*(h_star[i-1])^2)/(ret[i-1]-rt[i-1]-(nu0*h_star[i-1])))
  }
  
  drapeau=0
  if (w<=0){drapeau=1}
  if (b<=0){drapeau=1}
  if (a<=0){drapeau=1}
  if (c<=0){drapeau=1}
  if (neta>=0){drapeau=1}
  if (PI<=1.0259e+00){drapeau=1}
  if (nu<=0){drapeau=1}
  if (ro<=0){drapeau=1}
  if (ro>=1){drapeau=1}
  
  if (drapeau==0){
    resultat=h_star
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
  # para_h<-c() set up the parameters (physical probability) of the model 
  w=para_h[1]; b=para_h[2]; a=para_h[3];  c= para_h[4]; neta=para_h[5] ; nu=para_h[6] ; PI=para_h[7] ; ro=para_h[8]
  
  
  # Variable of risk neutral
  neta0=cbrt(((PI/nu)^2)*(-1+ sqrt(1+(8*nu)/(27*PI)))) + cbrt(((PI/nu)^2)*(-1-sqrt(1+(8*nu)/(27*PI)))) 
  nu0= nu/PI
  w0=w*PI; b0=b; a0=(a*neta)/(neta0*PI);  c0=(c*neta0*PI)/neta
  
  # z1= (Ret-r-nu*h)/neta
  # z2=2*pi*((Ret-r-nu*h)^3)*neta^(-3)
  
  drapeau=0
  if (w<=0){drapeau=1}
  if (b<=0){drapeau=1}
  if (a<=0){drapeau=1}
  if (c<=0){drapeau=1}
  if (neta>=0){drapeau=1}
  if (PI<=1.0259e+00){drapeau=1}
  if (nu<=0){drapeau=1}
  if (ro<=0){drapeau=1}
  if (ro>=1){drapeau=1}
  
  if (is.na(h)==TRUE){drapeau=1}else{
    if (h<0){drapeau=1}
    if (abs(h)==Inf){drapeau=1}
    if (1/abs(h)==Inf){drapeau=1}
  }
  
  # if (is.na(z1)==TRUE){drapeau=1}else{
  #   if (z1<0){drapeau=1}
  #   if (abs(z1)==Inf){drapeau=1}
  #   if (1/abs(z1)==Inf){drapeau=1}
  # }
  # 
  # 
  # if (is.na(z2)==TRUE){drapeau=1}else{
  #   if (z2<0){drapeau=1}
  #   if (abs(z2)==Inf){drapeau=1}
  #   if (1/abs(z2)==Inf){drapeau=1}
  # }  
  # Variable of risk neutral
  # PI=((-nu)*(neta0^2))/(1- (sqrt(1-2*neta0)))
  # nu0= nu/PI
  # w0=w*PI; b0=b; a0=(a*neta)/(neta0*PI);  c0=(c*neta0*PI)/neta
  
  
  Psy = b0+ (c0/(neta0^2))+a0*(neta0^2)
  h_0=(w0 + a0*(neta0^4))/(1 - a0*(neta0^2) - b0 - (c0*(neta0^(-2))))
  
  #  VIX 
  
  if (drapeau==0){
    resultat= 100*sqrt(tau*((h*((1-Psy^T_0)/((1-Psy)*T_0))) + h_0*(1-((1-Psy^T_0)/((1-Psy)*T_0))))/PI)
  }else{
    resultat=NA
  }
  return(resultat)
  
}

###########################################################
#####       The Log-likeelihood over all Option        ####
###########################################################
IGGARCH_likelihood_vix <- function(para_h,Data.returns) {
  Vix=Data.returns$VIX      ####  Call dividende
  ret =Data.returns$ret     #### Returns : Data.BSJ$ret
  rt=Data.returns$rt/250  
  
  # para_h<-c() set up the parameters (physical probability) of the model 
  w=para_h[1]; b=para_h[2]; a=para_h[3];  c= para_h[4]; neta=para_h[5] ; nu=para_h[6] ; PI=para_h[7] ; ro=para_h[8]
  
  VIX_Market<-Vix
  
  Nvix=length(Vix)
  
  h = h(para_h,Data.returns)
  
  VIX_Model <- rep(NA, Nvix)
  for (i in 1:Nvix){
    VIX_Model[i]= VIX_Q(para_h,h[i+1],ret[i+1],rt[i+1])
  }  
  
  ## Error terms :
  
  error <- rep(NA, Nvix)
  error[Nvix]=0
  for (i in 1:Nvix-1){
    error[i]= VIX_Market[i] - VIX_Model[i]
  }
  
  error_2 <- rep(NA, Nvix)
  error_2[1]=0
  for (i in 2:Nvix){
    error_2[i]= ((error[i]-ro*error[i-1])^2)/(1-ro^2)
  }
  
  sigma=mean(error^2)
  log_like=-1/2*sum(log(sigma)+((error^2)/sigma))-(Nvix/2)*(log(2*pi)+log(sigma*(1-(ro^2))))+ (1/2)*(log(sigma*(1-(ro^2)))-log(sigma))-(1/(2*sigma))*(error[i]^2+sum(error_2))
  
  return(log_like)  
  
}

