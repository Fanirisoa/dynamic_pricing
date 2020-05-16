
Data.contract=Data.N2

########################################################
###             Function that may used S/K       #######
########################################################

SK=c()    ## Moneyness S/K
S=Data.contract$S
K=Data.contract$K
for(i in 1:length(S)) {
  SK[i] = S[i]/K[i]
}
####################################################
######   Black-Scholes Function for call          ##
####################################################
C_BS <-  function(S, K, T, r, sig,d, type="C"){
  d1 <- (log(S/K) + (r -d + sig^2/2)*T) / (sig*sqrt(T))
  d2 <- d1 - sig*sqrt(T)
  if(type=="C"){
    value <- S*pnorm(d1) - K*exp(-r*T)*pnorm(d2)
  }
  if(type=="P"){
    value <- K*exp(-r*T)*pnorm(-d2) - S*pnorm(-d1)
  }
  return(value)
}
##########################################################
### Function : Implied volatilityfrom call option ########
##########################################################
implied.vol <-   function(S, K, T, r, C,d, type="C"){
  sig <- 0.20
  sig.up <- 1
  sig.down <- 0.000001
  count <- 0
  C_market <- C
  err <- C_BS(S, K, T, r, sig,0 ,type="C") - C_market 
  
  ## repeat until error is sufficiently small or counter hits 1000
  while(abs(err) > 0.000001 && count<10000){
    if(err < 0){
      sig.down <- sig
      sig <- (sig.up + sig)/2
    }else{
      sig.up <- sig
      sig <- (sig.down + sig)/2
    }
    err <- C_BS(S, K, T, r, sig,0, type) - C_market
    count <- count + 1
  }
  
  ## return NA if counter hit 1000
  if(count==10000){
    return(-1)
  }else{
    return(sig)
  }
}
Ip <- function(Data.contract, type="C")
{  
  T=Data.contract$T       ####  Time to maturity expressed in terms of years in terms of days
  S=Data.contract$S       ####  Prix du sous-jacent: Data.contract$S
  K=Data.contract$K       ####  Strike  Prix d'exercice: data$strike
  r=Data.contract$r/250   ####  Interest rate Data.contract$r
  C=Data.contract$C       ####  Call price
  d=Data.contract$d*0     ####  Call dividende
  
  Ip <- rep(NA, length(C))
  for (i in 1:length(C)){
    Ip[i] = implied.vol(S[i], K[i], T[i], r[i], C[i], d[i], type="C")
  }
  return(Ip)
}
#I=Ip(Data.contract, type="C")
#mean(I)

########################################################
### Table 1: In-sample options data (2009-2010)  #######
########################################################

Dc<-data.frame(C=Data.contract$C,K=Data.contract$K,S=Data.contract$S,T=Data.contract$T,T1=Data.contract$T*250,r=Data.contract$r,d=Data.contract$d,SK,Pe=Data.contract$Pe,Per=Data.contract$Per,Pr=Data.contract$Pr,Er=Data.contract$Er)
Dc[1:10,]
D<- function(a,b) {subset(Dc, a<T1 & T1<b )}
S<- function(a,b,e,f) {subset(D(a,b), e<SK & SK<f )}
a=c(0,60,180,400)
b=c(0,0.975,1.00,1.025,1.05,1.075,100)

D(60,180)
S(0,60,0,0.975)

Data.in.card=matrix(nrow=7, ncol=4, dimnames = list(c("0<S/K<0.975","0.975<S/K<1.00","1.00<S/K<1.025","1.025<S/K<1.05","1.05<S/K<1.075","1.075<S/K","Total"), c("0<T<20", "20<T<80", "80<T<180","Total")))
for (i in 1:6){
  for (j in 1:3){
    Data.in.card[i,j]= dim(S(a[j],a[j+1],b[i],b[i+1]))[1]
  }
}
for (j in 1:3){
  Data.in.card[7,j] <- sum(Data.in.card[1:6,j])  
}  
for (i in 1:7){
  Data.in.card[i,4] <- sum(Data.in.card[i,1:3])  
}  

Data.in.pcall=matrix(nrow=7, ncol=4, dimnames = list(c("0<S/K<0.975","0.975<S/K<1.00","1.00<S/K<1.025","1.025<S/K<1.05","1.05<S/K<1.075","1.075<S/K","Total"), c("0<T<20", "20<T<80", "80<T<180","Total")))
for (i in 1:6){
  for (j in 1:3){
    Data.in.pcall[i,j]= mean(S(a[j],a[j+1],b[i],b[i+1])$C)
  }
}
for (j in 1:3){
  Data.in.pcall[7,j] <- mean(Data.in.pcall[1:6,j])  
}	
for (i in 1:7){
  Data.in.pcall[i,4] <- mean(Data.in.pcall[i,1:3])  
}	

Data.in.ipv=matrix(nrow=7, ncol=4, dimnames = list(c("0<S/K<0.975","0.975<S/K<1.00","1.00<S/K<1.025","1.025<S/K<1.05","1.05<S/K<1.075","1.075<S/K","Total"), c("0<T<20", "20<T<80", "80<T<180","Total")))
for (i in 1:6){
  for (j in 1:3){
    Data.in.ipv[i,j]= mean(Ip(S(a[j],a[j+1],b[i],b[i+1]), type="C"))
  }
}
for (j in 1:3){
  Data.in.ipv[7,j] <- mean(Data.in.ipv[1:6,j])  
}	
for (i in 1:7){
  Data.in.ipv[i,4] <- mean(Data.in.ipv[i,1:3])  
}	

Data.in.Prsim=matrix(nrow=7, ncol=4, dimnames = list(c("0<S/K<0.975","0.975<S/K<1.00","1.00<S/K<1.025","1.025<S/K<1.05","1.05<S/K<1.075","1.075<S/K","Total"), c("0<T<20", "20<T<80", "80<T<180","Total")))
for (i in 1:6){
  for (j in 1:3){
    Data.in.Prsim[i,j]= mean(S(a[j],a[j+1],b[i],b[i+1])$Pr)
  }
}
for (j in 1:3){
  Data.in.Prsim[7,j] <- mean(Data.in.Prsim[1:6,j])  
}	
for (i in 1:7){
  Data.in.Prsim[i,4] <- mean(Data.in.Prsim[i,1:3])  
}	

Data.in.Ersim=matrix(nrow=7, ncol=4, dimnames = list(c("0<S/K<0.975","0.975<S/K<1.00","1.00<S/K<1.025","1.025<S/K<1.05","1.05<S/K<1.075","1.075<S/K","Total"), c("0<T<20", "20<T<80", "80<T<180","Total")))
for (i in 1:6){
  for (j in 1:3){
    Data.in.Ersim[i,j]= mean(S(a[j],a[j+1],b[i],b[i+1])$Er)
  }
}
for (j in 1:3){
  Data.in.Ersim[7,j] <- mean(Data.in.Ersim[1:6,j])  
}	
for (i in 1:7){
  Data.in.Ersim[i,4] <- mean(Data.in.Ersim[i,1:3])  
}	

Data.in.card
Data.in.pcall
Data.in.ipv
Data.in.Prsim
Data.in.Ersim
