#############################################################
#####  tHE LOGLIKELIHOODE of both the mixe return-option ####
#############################################################
Heston_likelihood_MixViX <- function(para_h,Data.returns) {

  log_like= Heston_likelihood_ret(para_h, Data.returns) + Heston_likelihood_vix(para_h, Data.returns)
  
  return(-log_like)  
}


