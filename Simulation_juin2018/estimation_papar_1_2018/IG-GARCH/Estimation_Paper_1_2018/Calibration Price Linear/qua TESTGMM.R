
###################################################################
###       Test the moment condition for the returns         #######
###################################################################
fht<-function(para_h,Data.ret){
  ret =Data.ret$ret   
  rt=Data.ret$rt/250        
  Z1=length(rt)
  
  ## set up the parameters of the model : para_h
  w=para_h[1]; b=para_h[2]; a=para_h[3];  c= para_h[4]; neta=para_h[5] 
  
  nu=(1/(neta^2))*(((1-2*neta)^(1/2))-1)

  h = c()                                                        ####  A vector containing h from the model,
  h[1]=(w + a*(neta^4))/(1 - a*(neta^2) - b - (c*(neta^(-2))))   ####  The first value for h, Unconditional Variance
  
  for (i in 2:Z1){
    h[i]=w+b*h[i-1]+ c*(neta^(-1))*(ret[i-1]-rt[i-1]-(nu*h[i-1]))+((a*neta*(h[i-1])^2)/(ret[i-1]-rt[i-1]-(nu*h[i-1])))
  }
  return(h)  
}


Testq<-function(para_h,Data.ret){
  ret <- Data.ret$ret
  rt <- Data.ret$rt
  nub=Data.ret$S
  
  # para_h<-c() estimated parameters 
  w=para_h[1]; b=para_h[2]; a=para_h[3];  c= para_h[4]; neta=para_h[5] 
  
  # The volatility under the physical probability
  nu0=(1/(neta0^2))*(((1-2*neta0)^(1/2))-1)
  
  # Variable of risk neutral
  theta0= 0.5 - (1/neta0)
  zeta=(1-(1/neta^4)*( 1- ((nu/nu0)^2)*((neta/neta0)^2)))
  PI=zeta*(neta0/neta)^(3/2)
  w0=zeta*w*(neta0/neta)^(-3/2); b0=b; a0=(zeta^(-1))*a*(neta0/neta)^(-5/2);  c0=zeta*c*(neta0/neta)^(5/2)
  
  ht=c()    
  for(i in 1:length(nub)) {
    ht[i] = fht(para_h,Data.ret)[i]
  }
  
  
  phi0=c()    
  for(i in 1:length(nub)) {
    phi0[i] = 0.5*((ht[i]/(neta^2))^2 )*(1 - ((nu^2*neta^4)/((1-2*theta0*neta)*(1-sqrt(1-2*neta0))^2)))
  }
  
  epsa=c()    
  for(i in 1:length(nub)) {
    epsa[i] = -rt[i]*(theta0+1)-theta0*nu*ht[i] - ((ht[i]/neta^2)-sqrt(((ht[i]/neta^2)^2-2*phi0[i])*(1-2*theta0*neta)))- log((ht[i]/neta^2)/(sqrt((ht[i]/neta^2)^2 - 2*phi0[i])))
  
  }
  
  Mt=c()    
  for(i in 1:length(nub)) {
    Mt[i] = exp(epsa[i]+theta0*ret[i]+(neta*phi0[i]/(ret[i]-rt[i]-(nu*ht[i]))) )
  }
    
  
  te=c()    
  for(i in 1:length(nub)) {
    te[i] = Mt[i]*exp(ret[i])-1
  }
  
  Te=mean(te)/(sd(te)*sqrt(length(nub)))
  
  return(Te)  
}




Testq(para_h1,Data.ret)

