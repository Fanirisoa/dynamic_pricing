

#####################################################
###    Load both data.contract and data.ret   #######
#####################################################
load("DataPrice20092010.Rdata")

#####################################################
###                 Data set                  #######
#####################################################
#Data.N=Data.N2

Data.N=Data.N2[-c(506,1462,1638,1645),]
N_val = 2^7
#####################################################
###         Parameters of the model           #######
#####################################################
#   w=para_h[1]; b=para_h[2]; a=para_h[3];  c= para_h[4]; neta=para_h[5] ; nu=para_h[6] ; PI=para_h[7] ; ro=para_h[8]

###   Initial parameter  ####
### para_h<-c(3.238940e-06 , 2.058376e-03  ,3.317425e+03 , 5.058743e-05, -8.281782e-03 , 1.2584e+02, 1.4003707e+00 ,  9.83599e-01 ) 

### ILK
### [1] 9487.409
### IGGARCH_likelihood_ret(para_h, Data.returns)
### [1] 7242.892
### IGGARCH_likelihood_vix(para_h,Data.returns)
### [1] -16730.3


### para_h<-c(4.238940e-06 , 2.058376e-03  ,3.317425e+03 , 5.058743e-05, -8.281782e-03 , 1.2584e+02, 1.110707e+00 ,  9.84599e-01 )  

### ILK
### [1] 9579.067
### IGGARCH_likelihood_ret(para_h, Data.returns)
### [1] 7102.041
### IGGARCH_likelihood_vix(para_h,Data.returns)
### [1] -16681.11


### para_h<-c(4.238940e-06 , 2.058376e-03  ,3.317425e+03 , 5.058743e-05, -8.281782e-03 , 1.2584e+02, 1.100707e+00 ,  9.83599e-01 ) 

### ILK
### [1] 9646.565
###  IGGARCH_likelihood_ret(para_h, Data.returns)
### [1] 7102.041
###  IGGARCH_likelihood_vix(para_h,Data.returns)
### [1] -16748.61

###  para_h<-c(9.876201e-06,  8.626756e-03,  3.317423e+03 , 4.491365e-05, -7.516695e-03 , 1.258370e+02,  1.097198e+00 , 9.962589e-01 )   

### ILK
### [1] 4625.001
### IGGARCH_likelihood_ret(para_h, Data.returns)
### [1] 7876.5
###  IGGARCH_likelihood_vix(para_h,Data.returns)
### [1] -12501.5


###para_h<-c(1.010747e-05,  2.282316e-03,  3.317425e+03,  4.514664e-05, -7.499894e-03,  1.258394e+02,  1.100010e+00,  9.945118e-01 ) 
### ILK
### [1] 4710.403
### IGGARCH_likelihood_ret(para_h, Data.returns)
### [1] 7871.022
### IGGARCH_likelihood_vix(para_h,Data.returns)
### [1] -12581.42
