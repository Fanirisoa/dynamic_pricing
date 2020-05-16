####################################################
######         The volatility updating rule       ##
####################################################
ht<-function(para_h,Data.ret){
  rt=Data.ret$rt/250        #### Interest rate Data : Data.BSJ$rt
  ret =Data.ret$ret         #### Returns : Data.BSJ$ret
  Z1=length(ret)
  
  # para_h<-c() set up the parameters (physical probability) of the model 
  w=para_h[1]; b=para_h[2]; a=para_h[3];  c= para_h[4]; neta=para_h[5] ; nu=para_h[6]  ; PI=para_h[7] ; ro=para_h[8]
  
  
  h = c()                                                        ####  A vector containing h from the model,
  h[1]=(w + a*(neta^4))/(1 - a*(neta^2) - b - (c*(neta^(-2))))   ####  The first value for h, Unconditional Variance
  
  for (i in 2:Z1){
    h[i]=w+b*h[i-1]+ c*(neta^(-1))*(ret[i-1]-rt[i-1]-(nu*h[i-1]))+((a*neta*(h[i-1])^2)/(ret[i-1]-rt[i-1]-(nu*h[i-1])))
  }
  
  drapeau=0
  if (w<=0){drapeau=1}
  if (b<=0){drapeau=1}
  if (a<=0){drapeau=1}
  if (c<=0){drapeau=1}
  
  if (drapeau==0){
    resultat=h
  }else{
    resultat=rep(NA, Z1)
  }
  return(resultat)
}

###########################################################
####        Compute the Annulized volatility           ####
###########################################################
Data.ret.2009=Data.ret[247:494,]

Vol2009=ht(para_h,Data.ret.2009)
anVol=(248*(Vol2009))^(1/2)
ts.plot(anVol, col = "steelblue", main = "IG Garch Model",xlab="2009",ylab="Volatility")
mean(anVol)

Vol2009cond=h(para_h,Data.ret.2009)
anVolcond=(248*(Vol2009cond))^(1/2)
ts.plot(anVolcond, col = "steelblue", main = "IG Garch Model",xlab="2009",ylab="Volatility")
mean(anVolcond)


