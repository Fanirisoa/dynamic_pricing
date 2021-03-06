#############################################################
#####  tHE LOGLIKELIHOODE of both the mixe return-option ####
#############################################################
Heston_likelihood_Mix <- function(para_M,Data.ret, Data.N,Data.returns,N) {
  ret =Data.returns$ret   
  T=Data.N$T
  N1=length(ret)
  N2=length(T)
  
  log_like=(N1+N2)*(NIG_likelihood_dens(para_M, Data.returns))*(1/(2*N1))  + (N1+N2)*(Heston_likelihood_opti(N,para_M,Data.ret, Data.N))*(1/(2*N2))
  
  return(-log_like)  
}



#############################################################
#####  tHE LOGLIKELIHOODE of both the mixe return-option ####
#############################################################
Heston_likelihood_Mix <- function(para_M, Data.N,Ret_sim,N) {
  ret =Data.returns$ret   
  T=Data.N$T
  N1=length(ret)
  N2=length(T)
  
  log_like=(N1+N2)*(NIG_likelihood_dens(para_M, Ret_sim))*(1/(2*N1))  + (N1+N2)*(Heston_likelihood_opti(N,para_M,Ret_sim, Data.N))*(1/(2*N2))
  
  return(-log_like)  
}

