############################################################
####             VIX Heston Nandi estimation              ##
############################################################
##   Initial parameter  ##
### a0=para_h[1]; a1=para_h[2]; gama=para_h[3];  b1= para_h[4] ;  lamda0= para_h[5] ; ro=para_h[6]

para_h<-c(4.27e-07, 1.51e-06, 462.6, 0.662,0.64, 9.646950e-01)

###   Solution 
para_h<-c(2.038488e-13, 1.523228e-06, 4.638213e+02, 6.621352e-01, 2.309781e+00, 9.646944e-01)

> Sol
$par
[1] 2.038488e-13 1.523228e-06 4.638213e+02 6.621352e-01 2.309781e+00 9.646944e-01

$value
[1] -6725.236

$counts
function gradient 
503       NA 

$convergence
[1] 0

$message
NULL
> time.taken
Time difference of 18.04956 secs


###  RMSE (Gaussian)  2009 = 0.05582381

###  RMSE (Gaussian)  2010 = 


##########################################################
#                QML estimation  NIG                     # 
##########################################################
###   Initial parameter  para_h<-c() set up the parameters of the model   ####
### alpha=para_distribution[1];  beta=para_distribution[2];  delta=para_distribution[3];  mu=para_distribution[4];

para_distribution<-c(1.785, 0.175, 1.1749, 0.174)

###   Solution 
para_h<-c(9.2429344642,  6.0545758000,  0.0063905794, -0.0006337919)

> QMLSol
$par
[1]  9.2429344642  6.0545758000  0.0063905794 -0.0006337919

$value
[1] -7948.515

$counts
function gradient 
379       NA 

$convergence
[1] 0

$message
NULL


















