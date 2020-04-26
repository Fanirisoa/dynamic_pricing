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
###      Parameters of the silulation         #######
#####################################################
N= 2

#####################################################
###         Parameters of the model           #######
#####################################################`
## a0=para_h[1]; a1=para_h[2]; a2=para_h[3];  b1= para_h[4] ;  lamda0= para_h[5] ; ro=para_h[6]   
## a0=para_M[5]; a1=para_M[6]; a2=para_M[7];  b1= para_M[8] ;  lamda0= para_M[9]  ; ro=para_M[10]

###   Initial parameter  ####``
###   Initial parameter  para_h<-c() set up the parameters of the volatility  ####

para_h<-c(5.192896e-06, 1.240918e-01, 2.314273e-02, 8.504267e-01, 1.821112e-01, 9.657773e-01)
para_h<-c(5.987174e-06,  1.240911e-01,  2.314265e-02,  8.504269e-01,  3.784983e-02, 9.546611e-01)  
para_h<-c(2.275545e-13, 1.496457e-06, 4.634467e+02, 6.713043e-01, 1.799994e-01, 9.646982e-016)



###   Initial parameter  para_distribution<-c() set up the parameters of the distribution   ####
### alpha=para_distribution[1];  beta=para_distribution[2];  delta=para_distribution[3];  mu=para_distribution[4];

para_distribution<-c(5, 1.397610234,   0.007012446,  -0.286886696)
#para_distribution<-c(2.237095e+01, -1.047143e+01,  7.135328e-03,  8.048087e-04)


para_M = c(para_distribution,para_h)


