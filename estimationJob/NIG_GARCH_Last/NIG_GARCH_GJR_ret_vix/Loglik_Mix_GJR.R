#############################################################
#####  tHE LOGLIKELIHOODE of both the mixe return-option ####
#############################################################
GJR_likelihood_Mix <- function(para_M,Data.ret, Data.N,Data.returns,N) {
  ret =Data.returns$ret   
  T=Data.N$T
  N1=length(ret)
  N2=length(T)
  
  log_like=(N1+N2)*(GJR_likelihood_ret(para_M, Data.returns))*(1/(2*N1)) + (N1+N2)*(GJR_likelihood_vix(para_M, Data.returns,Data.ret))*(1/(2*N2))
  
  return(-log_like)  
}

