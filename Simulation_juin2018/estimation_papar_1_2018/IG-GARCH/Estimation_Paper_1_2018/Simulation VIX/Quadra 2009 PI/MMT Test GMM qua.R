#####################################################
###    Load both data.contract and data.ret   #######
#####################################################
load("DataPrice2009test.Rdata")

##############################
###   Table Test GMM   #######
##############################
Table_GMM_test <- function(para_h1, Data.returns) {
  
St=Data.returns$St
ret=Data.returns$ret
S=Data.N$S

returns=rep(0, length(S)) 
for(i in 1:length(S)) {
  B1=subset(Data.returns, St == Data.N$S[i])
  returns[i]= B1$ret[1]
}

Data.N<-data.frame(C=Data.N$C,K=Data.N$K,S=Data.N$S,T=Data.N$T,r=Data.N$r,d=Data.N$d,Pe=Data.N$Pe,Pelol=Data.N$Pelol,Per=Data.N$Per,Mod=Data.N$Mod,CsK=Data.N$CsK,Mat=Data.N$Mat,NexP=Data.N$NexP,retns=returns)

###################################################################
###       Test the moment condition for the option         #######
###################################################################
fht<-function(para_h,Data.N){
  ret =Data.N$ret   
  rt=Data.N$rt/250        
  Z1=length(rt)
  
 ## set up the parameters of the model : para_h
  w=para_h[1]; b=para_h[2]; a=para_h[3];  c= para_h[4]; neta=para_h[5] ; nu=para_h[6]  ; PI=para_h[7] ; ro=para_h[8]
    
  h = c()                                                        ####  A vector containing h from the model,
  h[1]=(w + a*(neta^4))/(1 - a*(neta^2) - b - (c*(neta^(-2))))   ####  The first value for h, Unconditional Variance
  
  for (i in 2:Z1){
    h[i]=w+b*h[i-1]+ c*(neta^(-1))*(ret[i-1]-rt[i-1]-(nu*h[i-1]))+((a*neta*(h[i-1])^2)/(ret[i-1]-rt[i-1]-(nu*h[i-1])))
  }
  return(h)  
}

Mt<-function(para_h,Data.N,Data.returns){
  ret =Data.returns$ret   
  rt=Data.returns$rt/250        
  Z1=length(rt)
  nub=Data.returns$S
  
  
  # para_h<-c() set up the parameters (physical probability) of the model 
  w=para_h[1]; b=para_h[2]; a=para_h[3];  c= para_h[4]; neta=para_h[5] ; nu=para_h[6]  ; PI=para_h[7] ; ro=para_h[8]
  
  
  # Variable of risk neutral
  neta0=cbrt(((PI/nu)^2)*(-1+ sqrt(1+(8*nu)/(27*PI)))) + cbrt(((PI/nu)^2)*(-1-sqrt(1+(8*nu)/(27*PI)))) 
  nu0= nu/PI
  w0=w*PI; b0=b; a0=(a*neta)/(neta0*PI);  c0=(c*neta0*PI)/neta
  theta0 =(1/2)*((1/neta) -(1/neta0))
 
  ht=c()    
  for(i in 1:length(nub)) {
    ht[i] =fht(para_h,Data.returns)[i]
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
  return(Mt)  
}

MT<-Mt(para_h1,Data.N,Data.returns)

Data.new<-data.frame(date =Data.returns$date,St=Data.returns$St,rt=Data.returns$rt,VIX=Data.returns$VIX,ret=Data.returns$ret,vixret=Data.returns$vixret,MT)

St=Data.returns$St
ret=Data.returns$ret
S=Data.N$S

Mtt=rep(0, length(S)) 
for(i in 1:length(S)) {
  B1=subset(Data.new, St == Data.N$S[i])
  Mtt[i]= B1$MT[1]
}

Data.N<-data.frame(C=Data.N$C,K=Data.N$K,S=Data.N$S,T=Data.N$T,r=Data.N$r,d=Data.N$d,Pe=Data.N$Pe,Pelol=Data.N$Pelol,Per=Data.N$Per,Mod=Data.N$Mod,CsK=Data.N$CsK,Mat=Data.N$Mat,NexP=Data.N$NexP,retns=Data.N$retns,Mtt=Mtt)

Testq<-function(para_h,Data.N){
  ret =Data.N$retns   
  rt=Data.N$r/250 
  NexP=Data.N$NexP
  C=Data.N$C
  Mtt=Data.N$Mtt
  nub=Data.N$S

  te=c()    
  for(i in 1:length(nub)) {
    te[i] = Mtt[i]*(NexP[i]/C[i])-1
  }
  
  Te=mean(te)/(sd(te)*sqrt(length(nub)))
  
  return(Te)  
}

 




########################################################
###             Moneyness S/K                    #######
########################################################

SK=c()    
S=Data.N$S
K=Data.N$K
for(i in 1:length(S)) {
  SK[i] = S[i]/K[i]
}

T1=c()    
T=Data.N$T
for(i in 1:length(S)) {
  T1[i] = T[i]*250
}
########################################################
###       Function to subset the data set        #######
########################################################
a=c(0,60,180,400)
b=c(0,0.975,1.00,1.025,1.05,1.075,100)

a1= c(0,20,40,60)
b1=c(0.8,0.835,0.87,0.905,0.94,0.975)


########################################################
###            sub set  Test GMM Table           #######
########################################################
Data.all<-data.frame(C=Data.N$C,K=Data.N$K,S=Data.N$S,T=Data.N$T,T1=T1,r=Data.N$r,d=Data.N$d,Pe=Data.N$Pe,Pelol=Data.N$Pelol,SK,Per=Data.N$Per,Mod=Data.N$Mod,CsK=Data.N$CsK,Mat=Data.N$Mat,NexP=Data.N$NexP,retns=Data.N$retns,Mtt=Data.N$Mtt)

ALD<- function(a,b) {subset(Data.all, a<T1 & T1<b )}
ALS<- function(a,b,e,f) {subset(ALD(a,b), e<SK & SK<f )}

A.L=function(j,i)
{
  VarA=ALS(a[j],a[j+1],b[i],b[i+1])
  rownames(VarA) <- NULL
  
  AVar<-data.frame(C=VarA$C,K=VarA$K,S=VarA$S,T=VarA$T,T1=VarA$T1,r=VarA$r,d=VarA$d,Pe=VarA$Pe,Pelol=VarA$Pelol,SK=VarA$SK,Per=VarA$Per,Mod=VarA$Mod,CsK=VarA$CsK,Mat=VarA$Mat,NexP=VarA$NexP,retns=VarA$retns,Mtt=VarA$Mtt)
  return(AVar)
}


B.L=function(i)
{
  VarA=ALS(0,400,b[i],b[i+1])
  rownames(VarA) <- NULL
  
  AVar<-data.frame(C=VarA$C,K=VarA$K,S=VarA$S,T=VarA$T,T1=VarA$T1,r=VarA$r,d=VarA$d,Pe=VarA$Pe,Pelol=VarA$Pelol,SK=VarA$SK,Per=VarA$Per,Mod=VarA$Mod,CsK=VarA$CsK,Mat=VarA$Mat,NexP=VarA$NexP,retns=VarA$retns,Mtt=VarA$Mtt)
  return(AVar)
}

H.L=function(i)
{
  VarA=ALS(0,60,b[i],b[i+1])
  rownames(VarA) <- NULL
  
  AVar<-data.frame(C=VarA$C,K=VarA$K,S=VarA$S,T=VarA$T,T1=VarA$T1,r=VarA$r,d=VarA$d,Pe=VarA$Pe,Pelol=VarA$Pelol,SK=VarA$SK,Per=VarA$Per,Mod=VarA$Mod,CsK=VarA$CsK,Mat=VarA$Mat,NexP=VarA$NexP,retns=VarA$retns,Mtt=VarA$Mtt)
  return(AVar)
}

M.L=function(j)
{
  VarA=ALS(a[j],a[j+1],0,100)
  rownames(VarA) <- NULL
  
  AVar<-data.frame(C=VarA$C,K=VarA$K,S=VarA$S,T=VarA$T,T1=VarA$T1,r=VarA$r,d=VarA$d,Pe=VarA$Pe,Pelol=VarA$Pelol,SK=VarA$SK,Per=VarA$Per,Mod=VarA$Mod,CsK=VarA$CsK,Mat=VarA$Mat,NexP=VarA$NexP,retns=VarA$retns,Mtt=VarA$Mtt)
  return(AVar)
}

Testq.All=matrix(nrow=7, ncol=4, dimnames = list(c("0<S/K<0.975","0.975<S/K<1.00","1.00<S/K<1.025","1.025<S/K<1.05","1.05<S/K<1.075","1.075<S/K","Total"), c("0<T<20", "20<T<80", "80<T<180","Total")))
for (i in 1:6){
  for (j in 1:3){
    Testq.All[i,j]=Testq(para_h1,A.L(j,i))
  }
}
for (i in 1:6){
  Testq.All[i,4] <-Testq(para_h1,B.L(i))
}
for (j in 1:3){
  Testq.All[7,j] <-Testq(para_h1,M.L(j))
} 
Testq.All[7,4] <- Testq(para_h1,ALS(0,400,0,100))

return(Testq.All)  
}


