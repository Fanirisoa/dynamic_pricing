####################################################
######         The volatility updating rule       ##
####################################################
shape_h_P<-function(para_h,Data.ret){
  rt=Data.ret$rt/250        #### Interest rate Data : Data.BSJ$rt
  ret =Data.ret$ret         #### Returns : Data.BSJ$ret
  Z1=length(ret)
  
  # para_h<-c() set up the parameters of the model 
  w=para_h[1]; b=para_h[2]; a=para_h[3];  c= para_h[4]; neta=para_h[5]; nu=para_h[6] 
  
  
  
  h = c()                                                        ####  A vector containing h from the model,
  h[1]=(w + a*(neta^4))/(1 - a*(neta^2) - b - (c*(neta^(-2))))   ####  The first value for h,
  for (i in 2:Z1){
    h[i]=w+b*h[i-1]+ c*(neta^(-1))*(ret[i-1]-rt[i-1]-(nu*h[i-1]))+((a*neta*(h[i-1])^2)/(ret[i-1]-rt[i-1]-(nu*h[i-1])))
  }
  
  g0=(a*(neta^2) +b +(c*(neta^(-2))))
  
  drapeau=0
  if (w<=0){drapeau=1}
  if (b<=0){drapeau=1}
  if (a<=0){drapeau=1}
  if (c<=0){drapeau=1}
  if (a<=0){drapeau=1}
  if (g0<=0.8555){drapeau=1}
  if (g0>=0.9686){drapeau=1}
  # if (nu<=0){drapeau=1}
  if (drapeau==0){
    resultat=h 
  }else{
    resultat=rep(NA, Z1)
  }
  return(resultat)
}



###########################################################
#####  The conditional density of the daily return     ####
###########################################################

Retdensity <- function(para_h,Ret,h,r)
{
  ## set up the parameters of the model : para_h
  w=para_h[1]; b=para_h[2]; a=para_h[3];  c= para_h[4]; neta=para_h[5] ; nu=para_h[6] 
  
  z1= (Ret-r-nu*h)/neta
  z2=2*pi*((Ret-r-nu*h)^3)*neta^(-3)  
  
  theta0= (1/2)*((neta)^(-1) - (1/((nu^2)*(neta^3)))*(1 + ((nu^2)*(neta^3))/2)^2)
  neta0=neta/(1 - 2*neta*theta0)
  PI=(neta0/neta)^(3/2)
  w0=w*(neta0/neta)^(3/2); b0=b; a0=a*(neta0/neta)^(-5/2);  c0=c*(neta0/neta)^(5/2)
  
  # The volatility under the physical probability
  nu0=(1/(neta0^2))*(((1-2*neta0)^(1/2))-1)
  g0=  ( a0*(neta0^2) + b0+ (c0*(neta0^(-2))))   
  g1=(a*(neta^2) +b +(c*(neta^(-2))))
  

  drapeau=0
  if (w<=0){drapeau=1}
  if (b<=0){drapeau=1}
  if (a<=0){drapeau=1}
  if (c<=0){drapeau=1}
  if (neta>=0){drapeau=1}
  if (nu<=0){drapeau=1}
  if (g1<=0.8555){drapeau=1}
  if (g1>=0.9686){drapeau=1}
  if (g0<=0.9786){drapeau=1}
  if (g0>=0.9996){drapeau=1}
  if (g1>=g0){drapeau=1}
  if (is.na(h)==TRUE){drapeau=1}else{
    if (h<=0){drapeau=1}
    if (abs(h)==Inf){drapeau=1}
    if (1/abs(h)==Inf){drapeau=1}
  }
  if (is.na(z1)==TRUE){drapeau=1}else{
    if (z1<0){drapeau=1}
    if (abs(z1)==Inf){drapeau=1}
    if (1/abs(z1)==Inf){drapeau=1}
  }
  if (is.na(z2)==TRUE){drapeau=1}else{
    if (z2<0){drapeau=1}
    if (abs(z2)==Inf){drapeau=1}
    if (1/abs(z2)==Inf){drapeau=1}
  }
  
  if (drapeau==0){
    resultat= ((h*abs(neta^(-3)))/(sqrt(2*pi*((Ret-r-nu*h)^3)*neta^(-3))))*exp((-1/2)*(sqrt((Ret-r-nu*h)/neta)- (h/(neta^2))*sqrt(neta/(Ret-r-nu*h)))^2)
  }else{
    resultat=NA
  }
  return(resultat)
}



###########################################################
#####  The Log-likeelihood over all the returns dates  ####
###########################################################

IGGARCH_likelihood_ret <- function(para_h, Data.returns) {
  ret=Data.returns$ret   
  rt=Data.returns$rt/250        
  Z1=length(rt)
  
  ## set up the parameters of the model : para_h
  w=para_h[1]; b=para_h[2]; a=para_h[3];  c= para_h[4]; neta=para_h[5] ; nu=para_h[6]  
  
  h = c()                                                        ####  A vector containing h from the model,
  h[1]=(w + a*(neta^4))/(1 - a*(neta^2) - b - (c*(neta^(-2))))   ####  The first value for h, Unconditional Variance
  dens = Retdensity(para_h,ret[1],h[1],rt[1])
  
  for (i in 2:Z1){
    h[i]=w+b*h[i-1]+ c*(neta^(-1))*(ret[i-1]-rt[i-1]-(nu*h[i-1]))+((a*neta*(h[i-1])^2)/(ret[i-1]-rt[i-1]-(nu*h[i-1])))
    temp=Retdensity(para_h,ret[i],h[i],rt[i])
    dens<-dens+log(temp)
  }
  
  return(dens)  
}
