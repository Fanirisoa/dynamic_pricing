#############################################################
#####  tHE LOGLIKELIHOODE of both the mixe return-option ####
#############################################################
IGGARCH_likelihood_MixVIX <- function(para_h,Data.returns) {

  log_like= IGGARCH_likelihood_ret(para_h, Data.returns) + IGGARCH_likelihood_vix(para_h, Data.returns)
  
  return(-log_like)  
}
