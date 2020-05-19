########################################################################
#      Fonction densite chaque loi conditionelle pour les returns      # 
########################################################################

densite <- function(para_M,l){
  # para_M = c(para_distribution,para_h) 
  # alpha=para_distribution[1], beta=para_distribution[2], delta=para_distribution[3], mu=para_distribution[4]
  # a0=para_h[1]; a1=para_h[2]; gama=para_h[3];  b1= para_h[4] ;  lamda0= para_h[5] 
  
  ## set up the parameters of the model : para_M = c(para_distribution,para_h) 
  alpha=para_M[1];  beta=para_M[2];  delta=para_M[3];  mu=para_M[4]
  a0=para_M[5]; a1=para_M[6]; gama=para_M[7];  b1= para_M[8] ;  lamda0= para_M[9] 
  
  gamma_0 = sqrt((alpha^2) -(beta^2))
  
  ## Normalisation :
  sigma_z<- (delta*(alpha^2))/((gamma_0)^3)
  
  mu_z<- mu - ((delta*beta)/(gamma_0 ))
  
  ## Parametrization : 
  
  alpha_1 <- alpha*(sigma_z)
  beta_1 <- beta*(sigma_z)
  delta_1 <- delta/(sigma_z)
  mu_1 <- (1/(sigma_z))*(mu-mu_z)
  
  
  drapeau=0
  if (abs(alpha)<= abs(beta)){drapeau=1}
  if (alpha<=0){drapeau=1}
  if (alpha==Inf){drapeau=1}
  if (delta<=0){drapeau=1}
  
  ## if (abs(alpha_1)<= abs(beta_1)){drapeau=1}
  ## if (alpha_1<=0){drapeau=1}
  ## if (alpha_1==Inf){drapeau=1}
  ## if (delta_1<=0){drapeau=1}
  
  if (drapeau==0){
    resultat=dgh(l,alpha_1,beta_1,delta_1,mu_1,-1/2)
  }else{
    resultat=NA
  }
  return(resultat)
}

##########################################################
#                likelihood function                     # 
##########################################################  
NIG_likelihood_dens_QML <- function(para_M,Data.returns) {
  ret=Data.returns$ret   
  rt=Data.returns$rt/250        
  Z1=length(rt)
  
  # alpha=para_distribution[1], beta=para_distribution[2], delta=para_distribution[3], mu=para_distribution[4]
  # a0=para_h[1]; a1=para_h[2]; gama=para_h[3];  b1= para_h[4] ;  lamda0= para_h[5] 
  
  ## set up the parameters of the model : para_M = c(para_distribution,para_h) 
  alpha=para_M[1];  beta=para_M[2];  delta=para_M[3];  mu=para_M[4]                   ## para_distribution<-c() set up the parameters of NIG
  a0=para_M[5]; a1=para_M[6]; gama=para_M[7];  b1= para_M[8] ;  lamda0= para_M[9]     ## para_h<-c() set up the parameters of the model


  h <- c()                                                        ####  A vector containing h from the model,
  h_1<- (a0 + a1)/(1 - b1 - a1*(gama)^2 )                         ####  The first value for h, Unconditional Variance
  
  if (is.nan(h_1) || (h_1 > 6) ){
    v <- c(0.004791519,0.0002532843,0.4112256, 2.03423, 0.6579539, 1.82348, 0.5221694,0.0003014343, 0.6417815, 0.03610618,0.0004268574, 0.2940531, 0.03243031, 0.04024623, 0.007511524, 0.03384112, 0.1993153, 0.03679305, 0.0002753676,0.0001220703)
    h[1] = sample(v, 1)
  }else{
    if (h_1 < 0){
      h[1] = abs(h_1)
    }else{
      h[1] = h_1
    }
  }
  
  
  
  z <- c()                                                        ####  A vector containing z from the model,  innovation
  z[1] <- (ret[1]-rt[1]-lamda0*(h[1]))/sqrt(h[1])
  
  dens <- densite(para_M,z[1])
  
  for (i in 2:Z1){
    h_i=a0 +b1*h[i-1]+a1*(((ret[i-1]-rt[i-1]-lamda0*(h[i-1]))/(sqrt(h[i-1]))) - gama*(sqrt(h[i-1])))^2
    
    if (is.nan(h_i) || (h_i > 4) ){
      v <- c(0.004791519,0.0002532843,0.4112256, 2.03423, 0.6579539, 1.82348, 0.5221694,0.0003014343, 0.6417815, 0.03610618,0.0004268574, 0.2940531, 0.03243031, 0.04024623, 0.007511524, 0.03384112, 0.1993153, 0.03679305, 0.0002753676,0.0001220703)
      h[i] = sample(v, 1)
    }else{
      if (h_i < 0){
        h[i] = abs(h_i)
      }else{
        h[i] = h_i
      }
    }
    
    z[i]=(ret[i]-rt[i]-lamda0*(h[i]))/sqrt(h[i])
    temp=densite(para_M,z[i])
    dens<-dens+log(temp)
  }
  
  return(dens/length(dens))
}




