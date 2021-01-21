#############################################################
#####  tHE LOGLIKELIHOODE of both the mixe return-option ####
#############################################################
Heston_likelihood_Mix <- function(para_h,Data.ret, Data.N,Data.returns,N) {
  ret =Data.returns$ret   
  T=Data.N$T
  N1=length(ret)
  N2=length(T)
  
  log_like <- (N1+N2)*(Heston_likelihood_ret(para_h, Data.returns))*(1/(2*N1)) + (N1+N2)*(Heston_likelihood_vix(para_h, Data.returns,Data.ret))*(1/(2*N2))
  
    return(-log_like)  
}




#############################################################
#####  tHE LOGLIKELIHOODE of both the mixe return-option ####
#############################################################
Heston_likelihood_Mix_sim <- function(para_h,Ret_sim, Vix_sim) {
  log_like <- Heston_likelihood_ret_sim(para_h, Ret_sim) + Heston_likelihood_vix_sim(para_h, Ret_sim,Vix_sim)
  return(-log_like)  
}
