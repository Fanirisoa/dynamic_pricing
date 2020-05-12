######################################################################
######         Compute the variance given the paste variance        ##
######################################################################
variance<-function(para_h,innovation,ht){
  # para_h<-c() set up the parameters of the model 
  a0=para_h[1]; a1=para_h[2]; gama=para_h[3];  b1= para_h[4] ;  lamda0= para_h[4] 
  
  ht_next=a0 +b1*ht+a1*(innovation - gama*(sqrt(ht)))^2
  vol=sqrt(ht_next)
  
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
  
  result=rgh(1,alpha,beta0,delta,mu,-1/2)[1]
  
  return(result)
}

##########################################################################
##        Generate return Y_t=log(St)-log(St-1) from time 1 to T        ##
##########################################################################

Matrice_ret<-function(i){  
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
  
  base_sim=matrix(0,T[i],N)  
  for(nsim in 1:N)  
  {
    vol= sqrt((a0 + a1)/(1 - b1 - a1*(gamastar)^2 ))                    ####  volatility starting value
    #### vol= 10e-8      
    vol_stock = c()
    innovation=0
    for(n_t in 1:T[i])  
    {
      ht=vol^2
      mt = lamda0*ht
      innovation= Sim(para_h,para_distribution,ht) 
      vol=variance(para_h,innovation,ht)
      vol_stock=c(vol_stock,vol)
      val=r[i]-(vol^2)/2+vol*innovation
      
      base_sim[n_t,nsim]=val
    }
  }
  return(base_sim)  
}

#########################################################################     
##            Tranforme returns Yt into Monte-Carlos Prices St          #
######################################################################### 

MC_Sim_St<-function(B){  
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
    Y[[i]]= Matrice_Mart(i,B)
  }
  result=Y
  return(result) 
}
#################################################     
## Step 4 : Computation of the option prices   ##
#################################################  

Compute_P<-function(k,B){ 
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

