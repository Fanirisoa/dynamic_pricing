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
#####################################################
##a0=para_h[1]; a1=para_h[2]; gama=para_h[3];  b1= para_h[4] ;  lamda0= para_h[5]   

###   Initial parameter  ####``
###   Initial parameter  para_h<-c() set up the parameters of the volatility  ####

para_h<-c(1.180234e-12, 1.547729e-06, 4.550518e+02, 6.500111e-01 ,8.596182e+00, 9.646967e-01)



### para_h<-c(3.313135e-15, 1.366366e-06, 4.274284e+02, 7.324341e-01, 8.531595e+00, 9.95507e-01)  ## RMSE2$rmse : 0.0154167 ## RMSE3$rmse : 0.01615634
### para_h<-c(3.788371e-13, 1.520721e-06, 4.649298e+02, 6.619980e-01, 4.400007e-01, 9.646967e-01)  ## RMSE2$rmse : 0.01082776 ## RMSE3$rmse : 0.03113633
### para_h<-c(3.788371e-13, 1.520721e-06, 4.649298e+02, 6.619980e-01, 6.400007e-01, 9.646967e-01)  ## RMSE2$rmse : 0.0154167 ## RMSE3$rmse : 0.03113633
### para_h<-c(1.905978e-08, 1.522443e-06, 4.642720e+02, 6.620030e-01, 6.399963e-01, 9.646983e-01) ## 0.01083887



#para_h=c(1.854299e-04, 3.345238e-04 ,0.142406e+01 ,1.124012e-03 ,6.573458e-01)
#para_h<-c( 5.881028e-07,  1.506407e-06,  4.550518e+02,  6.500114e-01,  8.596182e+00)

###   Initial parameter  para_distribution<-c() set up the parameters of the distribution   ####
### alpha=para_distribution[1];  beta=para_distribution[2];  delta=para_distribution[3];  mu=para_distribution[4];

para_distribution<-c(5, 1.397610234,   0.007012446,  -0.286886696)
#para_distribution<-c(2.237095e+01, -1.047143e+01,  7.135328e-03,  8.048087e-04)


para_M = c(para_distribution,para_h)


