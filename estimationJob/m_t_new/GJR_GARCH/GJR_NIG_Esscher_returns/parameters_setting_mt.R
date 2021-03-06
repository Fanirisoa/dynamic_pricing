#####################################################
###    Load both data.contract and data.ret   #######
#####################################################
load("DataPrice20092010.Rdata")

#####################################################
###                 Data set                  #######
#####################################################
#Data.N=Data.N2

Data.N=Data.N2[-c(506,1462,1638,1645),]

#####################################################
###         Parameters of the model           #######
#####################################################
## a0=para_M[5]; a1=para_M[6]; a2=para_M[7];  b1= para_M[8] ;  lamda0= para_M[9] 

###   Initial parameter  ####``
###   Initial parameter  para_h<-c() set up the parameters of the volatility  ####

## para_h<-c(6.094e-11, 1.240e-01, 2.314e-02, 4.011e-01, 3.025e-02) 

##para_h<-c(5.192896e-06, 1.240918e-01, 2.314273e-02, 8.504267e-01, 1.821112e-01)
#para_h<-c(5.987174e-06,  1.240911e-01,  2.314265e-02,  8.504269e-01,  3.784983e-02)
para_h<-c(5.987174e-06,  1.240911e-01,  2.314265e-02,  8.504269e-01,  3.784983e-02)

###   Initial parameter  para_distribution<-c() set up the parameters of the distribution   ####
### alpha=para_distribution[1];  beta=para_distribution[2];  delta=para_distribution[3];  mu=para_distribution[4];

para_distribution<-c(5, 1.397610234,   0.007012446,  -0.286886696)
###para_distribution<-c(48.4956802775, -5.5025899512 , 0.0091739853 , 0.0003863926)
#para_distribution<-c(2.237095e+01, -1.047143e+01,  7.135328e-03,  8.048087e-04)
#para_h <-c(1.445949e-05,  3.107684e-01,  1.055816e-01,  6.311811e-01,  4.208730e-03)


change_para <- function(para_h_int) {
  a0=para_h_int[1]; a1=para_h_int[2]; a2=para_h_int[3];  b1= para_h_int[4] ;  lamda0= para_h_int[5] 
  h0=(a0 )/(1 - b1 - a1- a2/2) 
  lamda1 = (lamda0/sqrt(h0))-(1/2)
  return(c(a0, a1, a2,  b1,  lamda1))  
}

para_h<-change_para(para_h)
para_M = c(para_distribution,para_h)






