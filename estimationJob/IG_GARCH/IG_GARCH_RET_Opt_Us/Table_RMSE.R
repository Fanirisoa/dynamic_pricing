
########################################################
###             Function that may used S/K       #######
########################################################

##############################
###   Table Test GMM   #######
##############################
Table_RMSE <- function(para_h=para_h,Data.ret=Data.ret, Data.N=Data.N,N_hat ){
  
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
    err <- C_BS(S, K, T, r, sig,d ,type="C") - C_market 
    
    ## repeat until error is sufficiently small or counter hits 1000
    while(abs(err) > 0.00001 && count<10000){
      if(err < 0){
        sig.down <- sig
        sig <- (sig.up + sig)/2
      }else{
        sig.up <- sig
        sig <- (sig.down + sig)/2
      }
      err <- C_BS(S, K, T, r, sig,d, type) - C_market
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
  
  ############################################################
  #### Function that returns Root Mean Squared Error        ##
  ############################################################
  #Pr<-Price_fft(para_h=para_h,Data.ret=Data.ret, Data.contract=Data.contract)
  #Veg<-Vega(Data.contract=Data.contract, type="C")
  #Data.N=Data.contract
  Data.contract=Data.N
  para_h<-para_h1
  
  Pr<-Price_fft(para_h=para_h,Data.ret=Data.ret, Data.N=Data.N,N_hat)
  Veg<-Vega(Data.N=Data.N, type="C")
  
  
  nub=Data.N$S
  
  Ve=c()    
  for(i in 1:length(nub)) {
    Ve[i] = Veg[i]
  }
  
  P=c()    
  for(i in 1:length(nub)) {
    P[i] = Pr[i]
  }
  
  IVRMSE <- function(para_h,Data.ret,Data.contract)
  {  
    C=Data.contract$C       ####  Call price
    P=Data.contract$P       ####  Call price
    V=Data.contract$V       ####  Call price
    
    Norm_b= (1/sqrt((1/length(C))*sum((C)^2)))*100
    
    error <- rep(NA, length(C))
    for (i in 1:length(C)){
      error[i] = ((P[i]  -  C[i])/V[i])^2
    }
    rmse<-sqrt((mean(error)))
    norm_rmse<-Norm_b*sqrt((mean(error)))
    return(norm_rmse)
  }
  
  
  Data.contract=Data.N
  ########################################################
  ###             Moneyness S/K                    #######
  ########################################################
  SK=c()    
  S=Data.contract$S
  K=Data.contract$K
  for(i in 1:length(S)) {
    SK[i] = S[i]/K[i]
  }
  
  T1=c()    
  T=Data.contract$T
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
  
  Dc<-data.frame(C=Data.contract$C,K=Data.contract$K,S=Data.contract$S,T=Data.contract$T*250,r=Data.contract$r,d=Data.contract$d,SK,Pe=Data.contract$Pe,Per=Data.contract$Per)
  
  D<- function(x,y) {subset(Dc, x<T & T<y )}
  S<- function(x,y,e,f) {subset(D(x,y), e<SK & SK<f )}
  
  ########################################################
  ###            sub set  RMSE Table               #######
  ########################################################
  Data.all<-data.frame(C=Data.contract$C,K=Data.contract$K,S=Data.contract$S,T1=T1,T=Data.contract$T,r=Data.contract$r,d=Data.contract$d,SK,Pe=Data.contract$Pe,Per=Data.contract$Per,P=P,V=Ve)
  ALD<- function(a,b) {subset(Data.all, a<T1 & T1<b )}
  ALS<- function(a,b,e,f) {subset(ALD(a,b), e<SK & SK<f )}
  
  A.L=function(j,i)
  {
    VarA=ALS(a[j],a[j+1],b[i],b[i+1])
    rownames(VarA) <- NULL
    
    AVar<-data.frame(C=VarA$C,K=VarA$K,S=VarA$S,T1=VarA$T1,T=VarA$T,r=VarA$r,d=VarA$d,SK=VarA$SK,P=VarA$P,V=VarA$V)
    return(AVar)
  }
  
  B.L=function(i)
  {
    VarA=ALS(0,400,b[i],b[i+1])
    rownames(VarA) <- NULL
    
    AVar<-data.frame(C=VarA$C,K=VarA$K,S=VarA$S,T1=VarA$T1,T=VarA$T,r=VarA$r,d=VarA$d,SK=VarA$SK,P=VarA$P,V=VarA$V)
    return(AVar)
  }
  
  H.L=function(i)
  {
    VarA=ALS(0,60,b[i],b[i+1])
    rownames(VarA) <- NULL
    
    AVar<-data.frame(C=VarA$C,K=VarA$K,S=VarA$S,T1=VarA$T1,T=VarA$T,r=VarA$r,d=VarA$d,SK=VarA$SK,P=VarA$P,V=VarA$V)
    return(AVar)
  }
  
  M.L=function(j)
  {
    VarA=ALS(a[j],a[j+1],0,100)
    rownames(VarA) <- NULL
    
    AVar<-data.frame(C=VarA$C,K=VarA$K,S=VarA$S,T1=VarA$T1,T=VarA$T,r=VarA$r,d=VarA$d,SK=VarA$SK,P=VarA$P,V=VarA$V)
    return(AVar)
  }
  
  
  Data.in.RMSE.All=matrix(nrow=7, ncol=4, dimnames = list(c("0<S/K<0.975","0.975<S/K<1.00","1.00<S/K<1.025","1.025<S/K<1.05","1.05<S/K<1.075","1.075<S/K","Total"), c("0<T<20", "20<T<80", "80<T<180","Total")))
  for (i in 1:6){
    for (j in 1:4){
      Data.in.RMSE.All[i,j]= IVRMSE(para_h,Data.ret,A.L(j,i))
    }
  }
  for (i in 1:6){
    Data.in.RMSE.All[i,4] <- IVRMSE(para_h,Data.ret,B.L(i))
  }
  for (j in 1:3){
    Data.in.RMSE.All[7,j] <- IVRMSE(para_h,Data.ret,M.L(j))
  } 
  Data.in.RMSE.All[7,4] <- IVRMSE(para_h,Data.ret,ALS(0,400,0,100))
  
  return(Data.in.RMSE.All)  
}


