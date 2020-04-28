rm(list=ls())
gc()
library(zoo)

Data <- data.frame()
for(i in 1:(10)) {Data  <- rbind(Data ,c(A=i+2,B=sqrt(i),C=1/i,D=i/120,E=i/250,F=i+3)); names(Data ) <- letters[1:6]}
Data 


f<-function(x,para,c,d,e){
  steps<-round(d*250,0)  
  
  #para_para<-c() set up the parameters of the model 
  y1=para[1]
  y2=para[2]
  y3=para[3]
 
  #  The volatility under the physical probability
  h=0.25
  # Terminal condition for the A and B at time T
  A=0
  B=0
  # Recursion back to time t
  
  for (i in 1:steps){
    A= A+ e*x +y1*B
    B= y2*B+y3 
  }
  
  f = exp(log(c)*x -A + B*h )
  return(f)
  }

para<-c(1,-0.001,0.5)
W<-f(x=0.5,para,c=0.1,d=0.2,e=0.3)
W
f(x=0.5,para,c=Data$c,d=Data$d,e=Data$e)

M1 = mapply(function(c,d,e) f(x=0.5,para,c,d,e),c=Data$c,d=Data$d,e=Data$e)


g<-function(x,Data){
  
  g= mapply(function(c,d,e) f(x,para,c,d,e),c=Data$c,d=Data$d,e=Data$e)
  return(g)
  }

g(x=0.5,Data)


L<-function(data){
  
  a=Data$a
  b=Data$b
  c=Data$c
  d=Data$d
  e=Data$e
  F=Data$f

  N=2^10          
  alpha=2           
  delta= 0.25      
  lambda=(2*pi)/(N*delta)
  
  j=seq(1,N,1)
  k=seq(1,N,1)
  b=(lambda*N)/2
  strike= -b+(k-1)*lambda
  strike= exp(strike)

  res=c()
    for (i in 1:N){
    w=delta*(i-1)      
    w_f= w-(alpha+1)*1i
    phi= f(w_f,a, b, c,d)
    phi=phi/(alpha^2+alpha-w^2+1i*(2*alpha+1)*w)
    phi=phi*exp(e*(d))
    phi=phi*exp(1i*w*b)
    res=rbind(res,phi)
  }  

 fastft=Re(fft(res))*exp(-alpha*(-b+(k-1)*lambda))/pi
  
  result=c()
  for (i in 1:length(F)){
    index=which(strike<=F[i])
    index=index[length(index)]
    result=rbind(result,fastft[index])
  }  
  
  return(result)
return(res)
  }

M<-L(Data)

warnings()


L1<-function(data){
  a=Data$a
  b=Data$b
  c=Data$c
  d=Data$d
  e=Data$e
  F=Data$f

  N=2^10          
  alpha=2           
  delta= 0.25      
  lambda=(2*pi)/(N*delta)
  
  j=seq(1,N,1)
  k=seq(1,N,1)
  b=(lambda*N)/2
  strike= -b+(k-1)*lambda
  strike= exp(strike)

  res=c()
    for (i in 1:N){
    w=delta*(i-1)      
    w_f= w-(alpha+1)*1i
    phi= g(w_f,Data)
    phi=phi/(alpha^2+alpha-w^2+1i*(2*alpha+1)*w)
    phi=phi*exp(e*(d))
    phi=phi*exp(1i*w*b)
    res=rbind(res,phi)
  }  

 fastft=Re(fft(res))*exp(-alpha*(-b+(k-1)*lambda))/pi
  
  result=c()
  for (i in 1:length(F)){
    index=which(strike<=F[i])
    index=index[length(index)]
    result=rbind(result,fastft[index])
  }  
  
  return(result)
  }

N<-L1(Data)










