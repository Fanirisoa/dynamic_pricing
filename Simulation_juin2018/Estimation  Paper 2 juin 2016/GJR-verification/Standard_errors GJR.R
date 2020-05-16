library(maxLik)

###########################################################
#####  The conditional density of the daily return     ####
###########################################################
Standard_errorsGJR <- function(para_h)
{
  # para_h<-c() set up the parameters (physical probability) of the model 
  a0=para_h[1]; a1=para_h[2]; a2=para_h[3];  b1= para_h[4] ;  lamda0= para_h[5] ; ro=para_h[6]   
  N=length(para_h)
  
  A=numericHessian(Heston_likelihood_MixViX,  t0=para_h,Data.returns=Data.returns)
  
  A_mod=matrix(0,N,N)  
  for(i in 1:N)  
  {
    for(j in 1:N)  
    {
      if (is.na(A[i,j])==FALSE){A_mod[i,j]=abs(A[i,j])}
      else{
        A_mod[i,j]=1
      }
    }
  }
  
  resultat=  sqrt(1/diag(A_mod))
  return(list(A=A,resultat=resultat))
}

###########################################################
#####  The conditional density of the daily return     ####
###########################################################
Standard_errorsNIG <- function(para_distribution,para_h)
{
  # para_h<-c() set up the parameters (physical probability) of the model 
  alpha=para_distribution[1];  beta=para_distribution[2];  delta=para_distribution[3];  mu=para_distribution[4]  
  
  N=length(para_distribution)
  
  A=numericHessian(NIG_likelihood_dens_QML, t0=para_distribution, para_h =para_h,Data.returns=Data.returns)
  
  A_mod=matrix(0,N,N)  
  for(i in 1:N)  
  {
    for(j in 1:N)  
    {
      if (is.na(A[i,j])==FALSE){A_mod[i,j]=abs(A[i,j])}
      else{
        A_mod[i,j]=1
      }
    }
  }
  
  resultat=  sqrt(1/diag(A_mod))
  return(list(A=A,resultat=resultat))
}
