#############################################################
#####  tHE LOGLIKELIHOODE of both the mixe return-option ####
#############################################################
NGARCH_likelihood_MixViX <- function(para_h,Data.returns) {

  log_like= NGARCH_likelihood_ret(para_h, Data.returns) + NGARCH_likelihood_vix(para_h, Data.returns)
  
  return(-log_like)  
}
