############################################################
#####            Parameter of th Ushp  Returns vix     #####
############################################################
para_h<-c( 1.993036e-06 , 6.446413e-01  ,6.745918e+02,  8.488463e-06, -5.032286e-03 , 1.944558e+02  ,1.313840e+00  ,9.916137e-01)

############################################################
#####            Parameter of th Ushp  Returns Option  #####
############################################################
para_h<-c(6.494506e-06 , 5.404506e-01 , 3.635925e+02  ,1.103127e-05 ,-5.032834e-03 , 1.946021e+02, 1.245316e+00)

####################################################
##### Real cube root of a negative number in R    ##
####################################################
cbrt <- function(x) {
  sign(x) * abs(x)^(1/3)
}



# para_h<-c() set up the parameters (physical probability) of the model 
w=para_h[1]; b=para_h[2]; a=para_h[3];  c= para_h[4]; neta=para_h[5] ; nu=para_h[6]  ; PI=para_h[7] ; ro=para_h[8]


# Variable of risk neutral
neta0=cbrt(((PI/nu)^2)*(-1+ sqrt(1+(8*nu)/(27*PI)))) + cbrt(((PI/nu)^2)*(-1-sqrt(1+(8*nu)/(27*PI)))) 


theta = (1/2)*((1/neta)-(1/neta0))

a1=(nu^2)*(neta^4)
a2=1-(2*theta*neta)
a3=(1-(1-2*neta0)^(1/2))^2
rho = 1- (a1/(a2*a3))

theta
rho
