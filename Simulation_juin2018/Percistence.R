############################################### 
###          Resultat Generale          #######
############################################### 

############################################### 
###      1- HN_GARCH_ESS_Returns        #######
############################################### 
##a0=para_h[1]; a1=para_h[2]; gama=para_h[3];  b1= para_h[4] ;  lamda0= para_h[5]  

para_h1=c(3.854858e-08, 2.254897e-05, 5.379865e+01, 8.272017e-01, 1.020885e+00)

Persistence1<-function(para_h){
# para_h<-c() set up the parameters of the model 
a0=para_h[1]; a1=para_h[2]; gama=para_h[3];  b1= para_h[4] ;  lamda0= para_h[5]
# Parameter under the physical probability
gamastar= gama+lamda0+(1/2)

#Persistence under Historic proba
P=b1 + a1*(gama)^2  

#Persistence under Risk neutral
Pstar=b1 + a1*(gamastar)^2  

return(list(PersiHisto=P, PersiNeutral=Pstar)) 
}

Persistence1(para_h1)

> Persistence1(para_h1)
$`PersiHisto`
[1] 0.8924651

$PersiNeutral
[1] 0.8962072



############################################# 
###        2- GJR_GARCH_Returns      #######
############################################# 
##a0=para_h[1]; a1=para_h[2]; a2=para_h[3];  b1= para_h[4] ;  lamda0= para_h[5] 
para_h1<-c( 3.049628e-06, 1.243608e-01, 2.208629e-02, 8.509495e-01, 2.288715e-01) 

Persistence2<-function(para_h){
  
  # para_h<-c() set up the parameters of the model 
  a0=para_h[1]; a1=para_h[2]; a2=para_h[3];  b1= para_h[4] ;  lamda0= para_h[5]   
  
  #Persistence under Historic proba
  P=b1+ a1+ a2/2  
  
  #Persistence under Risk neutral
  Pstar=b1+ (a1+a2*(pnorm(lamda0)))*(1+lamda0^2)+a2*lamda0*dnorm(lamda0) 
  
  return(list(PersiHisto=P, PersiNeutral=Pstar)) 
}
Persistence2(para_h1)

> Persistence2(para_h1)
$`PersiHisto`
[1] 0.9863534

$PersiNeutral
[1] 0.9975146

####################################### 
###     3- NGARCH_Returns      #######
####################################### 
# para_h<-c() set up the parameters of the model 
#a0=para_h[1]; b1=para_h[2]; a1=para_h[3];  gama= para_h[4]; lambda= para_h[5];
para_h1<-c(1.677898e-06, 8.446999e-01, 6.174999e-02, 1.174353e+00, 8.911031e-07)


Persistence3<-function(para_h){
  
  # para_h<-c() set up the parameters of the model 
  a0=para_h[1]; b1=para_h[2]; a1=para_h[3];  gama= para_h[4]; lambda= para_h[5];   
  
  #Persistence under Historic proba
  P=b1+a1*(1+gama^2) 
  
  #Persistence under Risk neutral
  gamastar =  gama+lambda
  Pstar=b1 + a1*(gamastar)^2 
  
  return(list(PersiHisto=P, PersiNeutral=Pstar)) 
}


Persistence3(para_h1)

> Persistence3(para_h1)
$`PersiHisto`
[1] 0.9916096

$PersiNeutral
[1] 0.9298597



############################################### 
###      4- HN_GARCH_ESS_Returns_Option #######
############################################### 
##a0=para_h[1]; a1=para_h[2]; gama=para_h[3];  b1= para_h[4] ;  lamda0= para_h[5]  
para_h1=c(1.859301e-07, 1.542535e-06, 4.586904e+02, 6.500814e-01, 8.596183e+00)

Persistence1(para_h1)
> Persistence1(para_h1)
$`PersiHisto`
[1] 0.974626

$PersiNeutral
[1] 0.9876255

############################################### 
###      5- HN_GARCH_ESS_Returns_VIX    #######
############################################### 
### a0=para_h[1]; a1=para_h[2]; gama=para_h[3];  b1= para_h[4] ;  lamda0= para_h[5] ; ro=para_h[6]
para_h1=c(3.757071e-12, 2.252838e-05, 1.423105e+01, 9.117358e-01, 1.513781e+00, 9.999262e-01)

Persistence1(para_h1)
> Persistence1(para_h1)
$`PersiHisto`
[1] 0.9162983

$PersiNeutral
[1] 0.9176809

############################################### 
###    6- GJR_GARCH_ESS_Returns_VIX    #######
############################################### 
##a0=para_h[1]; a1=para_h[2]; a2=para_h[3];  b1= para_h[4] ;  lamda0= para_h[5]  ; ro=para_h[6]
para_h1<-c(4.966114e-06, 1.240920e-01, 2.314276e-02, 8.504266e-01, 1.989254e-01, 8.924053e-01)  

Persistence2(para_h1)


############################################### 
###    7- NGARCH_ESS_Returns_VIX        #######
############################################### 
##  a0=para_h[1]; b1=para_h[2]; a1=para_h[3];  gama= para_h[4]; lambda= para_h[5];  ro=para_h[6]
para_h1<-c(3.557112e-06, 7.956829e-01 ,6.172013e-02, 4.701267e-08, 8.452460e-01, 9.542031e-01)  

Persistence3(para_h1)

> Persistence3(para_h1)
$`PersiHisto`
[1] 0.857403

$PersiNeutral
[1] 0.8397783


############################### 
###    8-14  NIG-GARCH  #######
############################### 
Les Persistence sont ceux des Gaussian-GARCH

############################################### 
###    15- HN_GARCH_Qua_Returns_OPtion  #######
############################################### 
##a0=para_h[1]; a1=para_h[2]; gama=para_h[3];  b1= para_h[4] ;  lamda0= para_h[5]   ; Pi=para_h[6] 
para_h1=c(5.754790e-14, 1.513999e-06,4.586904e+02, 6.500812e-01, 8.596185e+00, 1.672330e+00)

Persistence4<-function(para_h){
  
  # para_h<-c() set up the parameters of the model 
  a0=para_h[1]; a1=para_h[2]; gama=para_h[3];  b1= para_h[4] ;  lamda0= para_h[5]   ; Pi=para_h[6] 

  #Persistence under Historic proba
  P=b1 + a1*(gama)^2
  
  #Persistence under Risk neutral
  gamastar= (gama/Pi)+(lamda0/Pi)+(1/2)
  a1star=Pi*Pi*a1
  
  Pstar=b1 + a1star*(gamastar^2)  
 
  return(list(PersiHisto=P, PersiNeutral=Pstar)) 
}
Persistence4(para_h1) 

> Persistence4(para_h1)
$`PersiHisto`
[1] 0.9686219

$PersiNeutral
[1] 0.9818573



############################################### 
###    16- HN_GARCH_Qua_Returns_VIX     #######
############################################### 
##a0=para_h[1]; a1=para_h[2]; gama=para_h[3];  b1= para_h[4] ;  lamda0= para_h[5]   ; Pi=para_h[6] ; ro=para_h[7]
para_h1=c(1.001455e-12, 1.504840e-06, 4.586904e+02, 6.512107e-01, 8.672445e+00, 1.722330e+00, 8.099270e-01)

Persistence4(para_h1) 


> Persistence4(para_h1)
$`PersiHisto`
[1] 0.9678243

$PersiNeutral
[1] 0.9811224


############################################### 
###    17- GJR_GARCH_Qua_Returns_VIX    #######
############################################### 
##a0=para_h[1]; a1=para_h[2]; a2=para_h[3];  b1= para_h[4] ;  lamda0= para_h[5]  ; Pi=para_h[6]  ; ro=para_h[7]
para_h1 = c(4.966114e-06, 1.240920e-01, 2.314276e-02, 8.504266e-01, 1.989254e-01,  1.278596e+00, 8.924053e-01) 

Persistence2(para_h1)
$`PersiHisto`
[1] 0.98609

############################################
###    18- NGARCH_Qua_Returns_VIX    #######
############################################
## a0=para_h[1]; b1=para_h[2]; a1=para_h[3];  gama= para_h[4]; lambda= para_h[5];  ; Pi=para_h[6]  ; ro=para_h[7]
para_h1=c(1.780075e-06, 9.329418e-01, 3.877833e-02, 1.277575e-07, 4.583290e-01,  1.24163e+00, 9.170840e-01) 

Persistence3(para_h1)

> Persistence3(para_h1)
$`PersiHisto`
[1] 0.9717201

############################################
###    19- NGARCH_EGP_Returns_VIX    #######
############################################
##    a0=para_h[1]; b1=para_h[2]; a1=para_h[3];  gama= para_h[4]; lambda= para_h[5]
para_h1=c( 1.896830e-06, 9.329402e-01,  3.877904e-02,  7.110437e-01,  9.937950e-02)


Persistence3(para_h1)

> Persistence3(para_h1)
$`PersiHisto`
[1] 0.9913253


###################################### 
###    20- IG_GARCH_Returns      ######
######################################
##  w=para_h[1]; b=para_h[2]; a=para_h[3];  c= para_h[4]; neta=para_h[5] ; nu=para_h[6];
para_h=c(9.817584e-06,  1.215612e-03,  3.322312e+03,  4.542147e-05, -7.531277e-03,  1.258401e+02)
para_h=c(1.206116e-06,  2.305289e-03,  3.317425e+03,  4.902499e-05, -7.972150e-03,  1.258400e+02)


Persistence5<-function(para_h){
  # para_h<-c() set up the parameters of the model 
  w=para_h[1]; b=para_h[2]; a=para_h[3];  c= para_h[4]; neta=para_h[5]; nu=para_h[6] 
  
  #Persistence under Historic proba
  P=(a*(neta^2) +b +(c*(neta^(-2)))) 
  
  #Persistence under Risk neutral
  theta0= (1/2)*((neta)^(-1) - (1/((nu^2)*(neta^3)))*(1 + ((nu^2)*(neta^3))/2)^2)
  neta0=neta/(1 - 2*neta*theta0)
  PI=(neta0/neta)^(3/2)
  w0=w*(neta0/neta)^(3/2); b0=b; a0=a*(neta0/neta)^(-5/2);  c0=c*(neta0/neta)^(5/2)
  
  # The volatility under the physical probability
  nu0=(1/(neta0^2))*(((1-2*neta0)^(1/2))-1)
  Pstar =  ( a0*(neta0^2) + b0+ (c0*(neta0^(-2)))) 
  return(list(PersiHisto=P, PersiNeutral=Pstar)) 
}

Persistence5(para_h1)
> Persistence5(para_h1)
$`PersiHisto`
[1] 0.9904574

$PersiNeutral
[1] 0.9608998

########################################### 
###    21- IG_GARCH_Returns_VIX       ######
###########################################
##  w=para_h[1]; b=para_h[2]; a=para_h[3];  c= para_h[4]; neta=para_h[5] ; nu=para_h[6]; ro=para_h[7];
para_h1=c(1.013422e-05,  2.073402e-03,  3.318154e+03,  4.506369e-05, -7.471968e-03,  1.258604e+02 , 9.955283e-01)
para_h1=c(2.234137e-06,  2.318400e-03,  3.317424e+03,  4.894911e-05, -7.955236e-03,  1.258397e+02,  9.978291e-01)


Persistence5(para_h1)
> Persistence5(para_h1)
$`PersiHisto`
[1] 0.9944816

$PersiNeutral
[1] 0.9599964

> Persistence(para_h1)
$`PersiHisto`
[1] 0.9857256

$PersiNeutral
[1] 0.9886



#####################################################
###        22- IG_GARCH_Returns_option         #######
#####################################################
##w=para_h[1]; b=para_h[2]; a=para_h[3];  c= para_h[4]; neta=para_h[5] ; nu=para_h[6]  
para_h1=c(9.769906e-06,  1.015940e-03,  3.331743e+03,  4.537934e-05, -7.531415e-03,  1.259401e+02)


Persistence5(para_h1)
> Persistence5(para_h1)
$`PersiHisto`
[1] 0.9900275

$PersiNeutral
[1] 0.9609959




##################################################
###        23- IG_GARCH_Ushp_Returns_option  #####
##################################################
#   w=para_h[1]; b=para_h[2]; a=para_h[3];  c= para_h[4]; neta=para_h[5] ; nu=para_h[6] ; PI=para_h[7] 
para_h1=c(1.018542e-05,  1.721115e-03,  3.317425e+03 , 4.511892e-05 ,-7.493686e-03  ,1.257392e+02,  1.400530e+00)

cbrt <- function(x) {
  sign(x) * abs(x)^(1/3)
}


Persistence6<-function(para_h){
  
  
  # para_h<-c() set up the parameters of the model 
  w=para_h[1]; b=para_h[2]; a=para_h[3];  c= para_h[4]; neta=para_h[5] ; nu=para_h[6] ; PI=para_h[7] ; ro=para_h[8]
  
    #Persistence under Historic proba
  P=  (a*(neta^2) +b +(c*(neta^(-2)))) 
  
  #Persistence under Risk neutral
  neta0=cbrt(((PI/nu)^2)*(-1+ sqrt(1+(8*nu)/(27*PI)))) + cbrt(((PI/nu)^2)*(-1-sqrt(1+(8*nu)/(27*PI)))) 
  nu0= nu/PI
  w0=w*PI; b0=b; a0=(a*neta)/(neta0*PI);  c0=(c*neta0*PI)/neta
  
  Pstar =  ( a0*(neta0^2) + b0+ (c0*(neta0^(-2)))) 
  
  return(list(PersiHisto=P, PersiNeutral=Pstar)) 
}

Persistence6(para_h1)
> Persistence6(para_h1)
$`PersiHisto`
[1] 0.9914786

$PersiNeutral
[1] 0.9595832

##################################################
###        24- IG_GARCH_Ushp_Returns_VIX    ######
##################################################
#   w=para_h[1]; b=para_h[2]; a=para_h[3];  c= para_h[4]; neta=para_h[5] ; nu=para_h[6] ; PI=para_h[7] ; ro=para_h[8]
para_h1=c(1.023104e-05,  1.911214e-03,  3.317425e+03,  4.504673e-05, -7.483091e-03 , 1.258400e+02 , 1.632461e+00 , 9.939197e-01)

para_h=c(5.173036e-06,  1.907469e-03, 3.317425e+03,  4.830306e-05, -7.937727e-03,  1.258399e+02,  1.400300e+00,  9.936888e-01)

para_h=c(5.322126e-06,  1.936518e-03  3.317425e+03,  4.829985e-05, -7.937417e-03,  1.258399e+02,  1.400300e+00,  9.974558e-01)

[1]  5.315654e-06  1.860314e-03  3.317425e+03  4.823345e-05 -7.928495e-03  1.258398e+02  1.632578e+00  9.938687e-01

Persistence6(para_h1)
> Persistence6(para_h1)
$`PersiHisto`
[1] 0.9921299

$PersiNeutral
[1] 0.9603102



