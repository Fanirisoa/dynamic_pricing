#############################################################
#####  tHE LOGLIKELIHOODE of both the mixe return-option ####
#############################################################
Heston_likelihood_Mix <- function(para_h,Data.ret, Data.N,Data.returns) {
  ret =Data.returns$ret   
  T=Data.N$T
  N1=length(ret)
  N2=length(T)
  
  log_like=(N1+N2)*(Heston_likelihood_ret(para_h, Data.returns))*(1/(2*N1)) + (N1+N2)*(Heston_likelihood_opti(para_h, Data.ret, Data.N))*(1/(2*N2))

  return(-log_like)  
}

