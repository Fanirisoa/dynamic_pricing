
######################################################################
######         Compute the variance given the paste variance        ##
######################################################################
variance<-function(para_h,innovation,h_t){
  
  if (h_t < 3){
    ht = h_t
  }else{
    v <- c(0.004791519,0.0002532843,0.4112256, 2.03423, 0.6579539, 1.82348, 0.5221694,0.0003014343, 0.6417815, 0.03610618,0.0004268574, 0.2940531, 0.03243031, 0.04024623, 0.007511524, 0.03384112, 0.1993153, 0.03679305, 0.0002753676,0.0001220703)
    ht = sample(v, 1)
  }
  
  
  # para_h<-c() set up the parameters of the model 
  a0=para_h[1]; a1=para_h[2]; gama=para_h[3];  b1= para_h[4] ;  lamda0= para_h[4] 
  
  # Parameter under the physical probability
  lamda0star= -(1/2)
  gamastar= gama+lamda0+(1/2)
 


  drapeau=0
  if (is.na(ht)){drapeau=1}
  if (is.nan(ht)){drapeau=1}
  if (drapeau==0){
    ht_next=a0 +b1*ht+a1*(innovation - gamastar*(sqrt(abs(ht))))^2
  }else{

    v <- c(0.004791519,0.0002532843,0.4112256, 2.03423, 0.6579539, 1.82348, 0.5221694,0.0003014343, 0.6417815, 0.03610618,0.0004268574, 0.2940531, 0.03243031, 0.04024623, 0.007511524, 0.03384112, 0.1993153, 0.03679305, 0.0002753676,0.0001220703)
    ht_next = sample(v, 1)
    
  }
  vol=sqrt(abs(ht_next))
  
  return(vol)  
}
####################################################
######          Simulation Monte Carlo            ##
####################################################

Sim<-function(para_h,para_distribution,ht){
  # vol contains current volatility
  # r contains the risk free rate 
  
  # para_h<-c() set up the parameters of the model 
  a0=para_h[1]; a1=para_h[2]; gama=para_h[3];  b1= para_h[4] ;  lamda0= para_h[4] 
  
  # para_distribution<-c() set up the parameters of the NIG distribution under P
  alpha=para_distribution[1];  beta=para_distribution[2];  delta=para_distribution[3];  mu=para_distribution[4]
  
  # mt contains the risk premium
  mt =  lamda0*ht  # lamda0*sqrt(ht)-(ht/2)
  
  # first part of theta
  A= ((alpha*mt+ sqrt(delta*ht)* beta* gama)^2)/(ht*delta*(gama^(3)))
  
  # second part of theta  
  B = ((4*(alpha^4)*(delta^2))/(ht*delta*(gama^(3)) + (alpha*mt+sqrt(delta*ht)* beta* gama  )^2)) -1
  
  # value of theta   
  theta =  -1/2 - ((alpha* beta*sqrt(delta))/(sqrt(ht)*(gama^(3/2)))) -(1/2)*((A*B)^(1/2))
  
  # change in parameter under RN distribution
  beta0=beta + sqrt(ht)*theta
  
  if ((abs(beta0) >= alpha) || is.nan(beta0) || is.na(beta0 )){
    result  <- 0.0246519
  }else{
    result  <- rgh(1,alpha,beta0,delta,mu,-1/2)[1]
  }
  

  
  return(result)
}


##########################################################################
##        Generate return Y_t=log(St)-log(St-1) from time 1 to T        ##
##########################################################################

Matrice_ret<-function(x,para_h1,para_distribution1){  
  # para_h<-c() set up the parameters of the model 
  
  a0=para_h1[1]; a1=para_h1[2]; gama=para_h1[3];  b1= para_h1[4] ;  lamda0= para_h1[4] 
  alpha=para_distribution1[1] ; beta=para_distribution1[2] ; delta=para_distribution1[3] ; mu=para_distribution1[4]
  
  # Parameter under the physical probability
  lamda0star= -(1/2)
  gamastar= gama+lamda0+(1/2)
  
  T=Data.N$T       ####  Time to maturity expressed in terms of years 
  S=Data.N$S       ####  Prix du sous-jacent: Data.contract$S
  K=Data.N$K       ####  Strike  Prix d'exercice: data$strike
  r=Data.N$r/250   ####  Interest rate Data.contract$r
  Z1=length(r)
  
  
  T=Data.N$T       
  T=T*250
  T=round(T,0)     ####  Time to maturity expressed in terms of days
  
  g1= b1 + a1*(gamastar^2)           ####  The percistence
  h0=(a0 + a1)/(1 - g1)              ####  The first value for h, Unconditional Variance
  

  base_sim=matrix(0,T[x],N)  
  for(j in 1:N)  
  {
    vol= c()                                         ####  Collecte vector of vol
    ht= c()                                           ####  Collecte vector of ht
    Inv= c()                                         ####  Collecte vector of innovation
    
    vol[1]= 0.0001220703      ####  sqrt(h0)             ####  volatility starting value #### vol= 10e-8 sqrt((a0 + a1)/(1 - b1 - a1*(gamastar)^2 ))   0.0001220703  
    ht[1]= vol[1]*vol[1]                                                     ####  ht starting value
    
    Inv[1]= Sim(para_h1,para_distribution1,ht[1])                            ####  innovation starting value

        
    base_sim[1,j]= r[x]-((ht[1])/2)+(vol[1])*Inv[1]
    

    
    for(i in 2:T[x])  
    {
      vol[i]=variance(para_h1,Inv[i-1] ,ht[i-1])
      ht[i]=(vol[i])^2
      
      # print(vol[i])
      # print("ht[i]")
      # print(ht[i])   
      # print("i")
      # print(i)
      # print("j")
      # print(j)
      
      drapeau=0
      if (is.na(ht[i])){drapeau=1}
      if (is.nan(ht[i])){drapeau=1}
      if (drapeau==0){
        Inv[i]= Sim(para_h1,para_distribution1,ht[i]) 
      }else{
        v <- c(0.004791519,0.0002532843,0.4112256, 2.03423, 0.6579539, 1.82348, 0.5221694,0.0003014343, 0.6417815, 0.03610618,0.0004268574, 0.2940531, 0.03243031, 0.04024623, 0.007511524, 0.03384112, 0.1993153, 0.03679305, 0.0002753676,0.0001220703)
        ht[i]= sample(v, 1)
        Inv[i]= Sim(para_h1,para_distribution1,ht[i]) 
        
      }
    
      # 
      # print("Inv[i]")
      # print(Inv[i])
      # print("ok_inv") 
      
      base_sim[i,j]= r[x]-((ht[i])/2)+(vol[i])*Inv[i]
    }
  }

  return(base_sim)  
}


#########################################################################     
##            Tranforme returns Yt into Monte-Carlos Prices St          #
######################################################################### 

MC_Sim_St<-function(B){  
  T=Data.N$T       ####  Time to maturity expressed in terms of years 
  S=Data.N$S       ####  Prix du sous-jacent: Data.contract$S
  K=Data.N$K       ####  Strike  Prix d'exercice: data$strike
  r=Data.N$r/250   ####  Interest rate Data.contract$r
  Z1=length(r)
  
  
  T=Data.N$T       
  T=T*250
  T=round(T,0)     ####  Time to maturity expressed in terms of days
  
  
  Y=list()
  for(i in 1:Z1)  
  {
    x=apply(B[[i]],2,cumsum)
    x=exp(x)
    Y[[i]]=S[i]*x
  }
  
  result=Y
  return(result) 
}

##############################################     
## Step 3 : Martingalisation of the sample  ##
##############################################  
Matrice_Mart<-function(k,B){ 
  T=Data.N$T       ####  Time to maturity expressed in terms of years 
  S=Data.N$S       ####  Prix du sous-jacent: Data.contract$S
  K=Data.N$K       ####  Strike  Prix d'exercice: data$strike
  r=Data.N$r/250   ####  Interest rate Data.contract$r
  Z1=length(r)
  
  
  T=Data.N$T       
  T=T*250
  T=round(T,0)     ####  Time to maturity expressed in terms of days
  
  
  S_t=B[[k]]
  M=apply(B[[k]],1,mean)
  d=c()
  base_mart=matrix(0,T[k],N) 
  for(i in 1:T[k])  
  {
    d[i]=S[k]*exp(r[k]*(i))/M[i]
    for(j in 1:N)  
    {
      base_mart[i,j]=S_t[i,j]*d[i]
    }
  }
  return(base_mart)  
}


Mar_St<-function(B){  
  T=Data.N$T       ####  Time to maturity expressed in terms of years 
  S=Data.N$S       ####  Prix du sous-jacent: Data.contract$S
  K=Data.N$K       ####  Strike  Prix d'exercice: data$strike
  r=Data.N$r/250   ####  Interest rate Data.contract$r
  Z1=length(r)
  
  
  T=Data.N$T       
  T=T*250
  T=round(T,0)     ####  Time to maturity expressed in terms of days
  
  
  Y=list()
  for(i in 1:Z1)  
  {
    Y[[i]]= Matrice_Mart(i,B)
  }
  result=Y
  return(result) 
}
#################################################     
## Step 4 : Computation of the option prices   ##
#################################################  

Compute_P<-function(k,B){ 
  T=Data.N$T       ####  Time to maturity expressed in terms of years 
  S=Data.N$S       ####  Prix du sous-jacent: Data.contract$S
  K=Data.N$K       ####  Strike  Prix d'exercice: data$strike
  r=Data.N$r/250   ####  Interest rate Data.contract$r
  Z1=length(r)
  
  
  T=Data.N$T       
  T=T*250
  T=round(T,0)     ####  Time to maturity expressed in terms of days
  
  
  
  St_Mar=B[[k]]
  S_T=St_Mar[T[k],]
  fST=S_T - K[k]
  value_fST=pmax(fST,0,na.rm=TRUE)
  Mean_fST=mean(value_fST,na.rm=TRUE) 
  P_T=Mean_fST*exp(-r[k]*T[k]) 
  
  return(P_T)  
}


P_T<-function(B){  
  # para_h<-c() set up the parameters of the model 
  
  a0=para_h[1]; a1=para_h[2]; gama=para_h[3];  b1= para_h[4] ;  lamda0= para_h[4] 
  
  # Parameter under the physical probability
  lamda0star= -(1/2)
  gamastar= gama+lamda0+(1/2)
  
  T=Data.N$T       ####  Time to maturity expressed in terms of years 
  S=Data.N$S       ####  Prix du sous-jacent: Data.contract$S
  K=Data.N$K       ####  Strike  Prix d'exercice: data$strike
  r=Data.N$r/250   ####  Interest rate Data.contract$r
  Z1=length(r)
  
  
  T=Data.N$T       
  T=T*250
  T=round(T,0)     ####  Time to maturity expressed in terms of days
  
  
  Y=list()
  for(i in 1:Z1)  
  {
    Y[[i]]= Compute_P(i,B)
  }
  
  P=c()
  for(i in 1:Z1)  
  {
    P[i]=  Y[[i]]
  }
  
  result=P
  return(result) 
}










######################################################################
######         Seuil pour vol[1] initial value of the volatility    ##
######################################################################
##Seuil<-function(para_h,para_distribution,ht){
##  a0=para_h1[1]; a1=para_h1[2]; gama=para_h1[3];  b1= para_h1[4] ;  lamda0= para_h1[4] 

##  alpha=para_distribution1[1];  beta=para_distribution1[2];  delta=para_distribution1[3];  mu=para_distribution1[4]

##  B = ((4*(alpha^4)*(delta^2))/(ht*delta*(gama^(3)) + (alpha*mt+sqrt(delta*ht)* beta*gama)^2)) -1

##  fun <- function (x)  ((4*(alpha^4)*(delta^2))/(x*delta*(gama^(3)) + (alpha*lamda0*x+sqrt(delta*x)* beta*gama)^2)) -1

##  uni <- uniroot(fun, c(0, 1))$root
##  return(uni)  
##}






####################################################
######          Simulation Monte Carlo            ##
####################################################
# 
# SimNIG<-function(para_h,para_distribution,ht){
#   # vol contains current volatility
#   # r contains the risk free rate 
#   
#   # para_h<-c() set up the parameters of the model 
#   a0=para_h[1]; a1=para_h[2]; gama=para_h[3];  b1= para_h[4] ;  lamda0= para_h[4] 
#   
#   # para_distribution<-c() set up the parameters of the NIG distribution under P
#   alpha=para_distribution[1];  beta=para_distribution[2];  delta=para_distribution[3];  mu=para_distribution[4]
#   
#   # mt contains the risk premium
#   mt = lamda0*sqrt(ht)-(ht/2)
#   
#   # first part of theta
#   A= ((alpha*mt+ sqrt(delta*ht)* beta* gama)^2)/(ht*delta*(gama^(3)))
#   
#   # second part of theta  
#   B = ((4*(alpha^4)*(delta^2))/(ht*delta*(gama^(3)) + (alpha*mt+sqrt(delta*ht)* beta* gama  )^2)) -1
#   
#   # value of theta   
#   theta =  -1/2 - (alpha* beta*sqrt(delta))/(sqrt(ht)*(gama^(3/2))) -(1/2)*((A*B)^(1/2))
#   
#   # change in parameter under RN distribution
#   beta=beta + ht*theta
#   
#   result=rnig(1, alpha, beta, delta, mu)[1]
#   
#   return(result)
# }

