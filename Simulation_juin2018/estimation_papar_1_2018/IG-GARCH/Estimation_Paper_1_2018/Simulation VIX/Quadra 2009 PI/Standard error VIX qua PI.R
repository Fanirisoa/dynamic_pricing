###########################################################
#####  The conditional density of the daily return     ####
###########################################################

Standard_errors <- function(para_h)
{
  # para_h<-c() set up the parameters (physical probability) of the model 
  w=para_h[1]; b=para_h[2]; a=para_h[3];  c= para_h[4]; neta=para_h[5] ; nu=para_h[6] ; PI=para_h[7] ; ro=para_h[8]
  N=length(para_h)
  
  A=numericHessian(IGGARCH_likelihood_MixVIX,  t0=para_h,Data.returns=Data.returns)

  A_mod=matrix(0,N,N)  
  for(i in 1:N)  
  {
    for(j in 1:N)  
    {
      if (is.na(A[i,j])==FALSE){A_mod[i,j]=abs(A[i,j])}
      else{
        A_mod[i,j]=(abs(rnorm(1,0,abs(A[1,1]))))^(1/2)
      }
      }
   }

  resultat=  sqrt(1/diag(A_mod))
  return(resultat)
}
