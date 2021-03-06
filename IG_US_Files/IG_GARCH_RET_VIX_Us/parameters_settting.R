#####################################################
###    Load both data.contract and data.ret   #######
#####################################################
load("DataPrice20092010.Rdata")

#####################################################
###                 Data set                  #######
#####################################################
#Data.N=Data.N2

Data.N=Data.N2[-c(506,1462,1638,1645),]
N_val = 2^5
#####################################################
###         Parameters of the model           #######
#####################################################
# With the parameter a: para_h<-c() set up the parameters of the model 
# w=para_h[1]; b=para_h[2];  a=para_h[3];  c= para_h[4]; neta=para_h[5]; nu=para_h[6] ; PI=para_h[7] ; ro=para_h[8]

# Without the parameter a: para_h<-c() set up the parameters of the model 
# w=para_h[1]; b=para_h[2]; c= para_h[3]; neta=para_h[4]; nu=para_h[5]  ; PI=para_h[6] ; ro=para_h[7]

###   Initial parameter  ####
# para_h<-c(3.238940e-06 , 2.058376e-03 , 5.058743e-05, -8.281782e-03 , 1.2584e+02, 1.632707e+00 ,  9.83599e-01 ) 
# para_h<-c(3.238940e-06 , 2.058376e-03 , 5.058743e-05, -8.281782e-03 , 1.2584e+02, 1.1003707e+00,  9.83599e-01 )   


#para_h<-c(3.238940e-06 , 2.058376e-03 , 5.058743e-05, -8.281782e-03 , 1.2584e+02, 1.1003707e+00,  9.83599e-01 )   

# para_h<-c(3.238940e-04 , 2.058376e-03 , 5.058743e-05, -8.281782e-03 , 100, 1.1003707e+00,  9.83599e-01 )    OK

# para_h<-c(3.438940e-04 , 2.058376e-02 , 5.058743e-05, -8.281782e-03 , 100, 0.903707e+00,  9.73599e-01 )    
# IGGARCH_likelihood_ret(para_h, Data.returns) ; 6592.562
# IGGARCH_likelihood_vix(para_h,Data.returns) ;  NA 


# para_h<-c(3.438940e-04 , 2.058376e-02 , 5.058743e-05, -8.281782e-03 , 100, 1.903707e+00,  9.73599e-01 )    
# IGGARCH_likelihood_ret(para_h, Data.returns) ; 6592.562
# IGGARCH_likelihood_vix(para_h,Data.returns) ;  -18082.78
# ILK : 11490.22


# Optimisation
# Time difference of 26.6229 secs
# > Sol
# $par
# [1]  1.895750e-04  1.268234e-02  6.777842e-05 -9.181324e-03  1.000047e+02  1.885036e+00  9.981974e-01
# 
# $value
# [1] 7854.24
# 
# $counts
# function gradient 
# 1151       NA 
# 
# $convergence
# [1] 0
# 
# $message
# NULL

# With the parameter a: para_h<-c() set up the parameters of the model 
# w=para_h[1]; b=para_h[2];  a=para_h[3];  c= para_h[4]; neta=para_h[5]; nu=para_h[6] ; PI=para_h[7] ; ro=para_h[8]
# para_h<-c(3.238940e-06 , 2.058376e-03  ,3.317425e+03 , 5.058743e-05, -8.281782e-03 , 1.2584e+02, 1.632707e+00 ,  9.83599e-01 ) 
# para_h<-c(3.238940e-06 , 2.058376e-03  ,3.317425e+03 , 5.058743e-05, -8.281782e-03 , 1.2584e+02, 1.1003707e+00 ,  9.83599e-01 )  


# Without the parameter a: para_h<-c() set up the parameters of the model 
# w=para_h[1]; b=para_h[2]; c= para_h[3]; neta=para_h[4]; nu=para_h[5]  ; PI=para_h[6] ; ro=para_h[7]
# para_h<-c(3.238940e-06 , 2.058376e-03   , 5.058743e-05, -8.281782e-03 , 1.2584e+02, 1.632707e+00 ,  9.83599e-01 ) 
# para_h<-c(3.238940e-06 , 2.058376e-03   , 5.058743e-05, -8.281782e-03 , 1.2584e+02, 1.1003707e+00 ,  9.83599e-01 )  



# para_h<-c(3.238940e-04 , 2.058376e-03   , 5.058743e-05, -8.281782e-03 , 1.2584e+02, 1.232707e+00 ,  9.83599e-01 ) 


para_h<-c(3.238940e-06 , 2.058376e-03   , 5.058743e-05, -8.281782e-03 , 1.2584e+02, 1.1003707e+00 ,  9.83599e-01 ) 




