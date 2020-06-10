#############################################################
#####  tHE LOGLIKELIHOODE of both the mixe return-option ####
#############################################################
GJR_likelihood_Mix <- function(para_h,Data.returns) {

  log_like= GJR_likelihood_ret(para_h, Data.returns) + GJR_likelihood_vix(para_h, Data.returns)
  
  return(-log_like)  
}


#############################################################
#####  tHE LOGLIKELIHOODE of both the mixe return-option ####
#############################################################
Heston_likelihood_RET_solo<- function(para_h,Data.returns) {
  
  log_like= GJR_likelihood_ret(para_h, Data.returns) 
  
  return(-log_like)  
}

