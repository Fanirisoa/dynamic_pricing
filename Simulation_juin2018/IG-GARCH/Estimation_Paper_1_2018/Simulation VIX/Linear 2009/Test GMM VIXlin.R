
###################################################################
###       Test the moment condition for the returns         #######
###################################################################
fht<-function(para_h,Data.ret){
  ret =Data.ret$ret   
  rt=Data.ret$rt/250        
  Z1=length(rt)
  
  ## set up the parameters of the model : para_h
  w=para_h[1]; b=para_h[2]; a=para_h[3];  c= para_h[4]; neta=para_h[5] ; nu=para_h[6]
  
  h = c()                                                        ####  A vector containing h from the model,
  h[1]=(w + a*(neta^4))/(1 - a*(neta^2) - b - (c*(neta^(-2))))   ####  The first value for h, Unconditional Variance
  
  for (i in 2:Z1){
    h[i]=w+b*h[i-1]+ c*(neta^(-1))*(ret[i-1]-rt[i-1]-(nu*h[i-1]))+((a*neta*(h[i-1])^2)/(ret[i-1]-rt[i-1]-(nu*h[i-1])))
  }
  return(h)  
}

Testl<-function(para_h,Data.ret){
ret <- Data.ret$ret
rt <- Data.ret$rt
nub=Data.ret$S

# para_h<-c() estimated parameters 
  # para_h<-c() set up the parameters of the model 
  w=para_h[1]; b=para_h[2]; a=para_h[3];  c= para_h[4]; neta=para_h[5] ; nu=para_h[6]

# Variable of risk neutral  
  theta0= (1/2)*((neta)^(-1) - (1/((nu^2)*(neta^3)))*(1 + ((nu^2)*(neta^3))/2)^2)
  neta0=neta/(1 - 2*neta*theta0)
  PI=(neta0/neta)^(3/2)
  w0=w*(neta0/neta)^(3/2); b0=b; a0=a*(neta0/neta)^(-5/2);  c0=c*(neta0/neta)^(5/2)



ht=c()    
for(i in 1:length(nub)) {
  ht[i] = fht(para_h,Data.ret)[i]
}

Mt=c()    
for(i in 1:length(nub)) {
  Mt[i] = exp(theta0*ret[i]+(-rt[i]*(theta0+1)-theta0*nu*ht[i] - (ht[i]/neta^2)*(1-sqrt(1-2*theta0*neta))))
}

te=c()    
for(i in 1:length(nub)) {
  te[i] = Mt[i]*exp(ret[i])-1
}

Te=mean(te)/(sd(te)*sqrt(length(nub)))

return(Te)  
}



Testl(para_h1,Data.ret)

