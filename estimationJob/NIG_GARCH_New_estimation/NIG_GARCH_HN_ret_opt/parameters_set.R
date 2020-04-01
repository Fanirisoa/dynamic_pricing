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
N= 100

#####################################################
###         Parameters of the model           #######
#####################################################
##a0=para_h[1]; a1=para_h[2]; gama=para_h[3];  b1= para_h[4] ;  lamda0= para_h[5]   

###   Initial parameter  ####``
###   Initial parameter  para_h<-c() set up the parameters of the volatility  ####

para_h<-c(1.180234e-12, 1.547729e-06, 4.550518e+02, 6.500111e-01 ,8.596182e+00)

#para_h=c(1.854299e-04, 3.345238e-04 ,0.142406e+01 ,1.124012e-03 ,6.573458e-01)
#para_h<-c( 5.881028e-07,  1.506407e-06,  4.550518e+02,  6.500114e-01,  8.596182e+00)

###   Initial parameter  para_distribution<-c() set up the parameters of the distribution   ####
### alpha=para_distribution[1];  beta=para_distribution[2];  delta=para_distribution[3];  mu=para_distribution[4];

para_distribution<-c(5, 1.397610234,   0.007012446,  -0.286886696)
#para_distribution<-c(2.237095e+01, -1.047143e+01,  7.135328e-03,  8.048087e-04)


para_M = c(para_distribution,para_h)


