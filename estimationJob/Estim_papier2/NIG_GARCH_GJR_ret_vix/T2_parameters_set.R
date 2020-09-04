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
N= 200


#####################################################
###             date to considere             #######
#####################################################
index_ht = which(Data.returns$date == "2010-01-04")
index_vix = which(Data.ret$date == "2010-01-04")



#####################################################
###         Parameters of the model           #######
#####################################################
### a0=para_h[1]; a1=para_h[2]; a2=para_h[3];  b1= para_h[4] ;  lamda0= para_h[5] ; ro=para_h[6]   
 

###   Initial parameter  ####``
###   Initial parameter  para_h<-c() set up the parameters of the volatility  ####

###   para_h<-c(5.192896e-06, 1.240918e-01, 2.314273e-02, 8.504267e-01, 1.821112e-01, 9.657773e-01)

para_h<-c(4.966e-06, 1.240e-01, 2.314e-02, 8.504e-01, 1.989e-01, 0.8924)

## para_h<-c(5.987174e-06,  1.240911e-01,  2.314265e-02,  8.504269e-01,  3.784983e-02, 9.546611e-01)  ## RMSE1$rmse : 0.06265758 RMSE3$rmse :0.07367674

## para_h1<-c(2.275545e-13, 1.496457e-06, 4.634467e+02, 6.713043e-01, 1.799994e-01, 9.646982e-016)



###   Initial parameter  para_distribution<-c() set up the parameters of the distribution   ####
### alpha=para_distribution[1];  beta=para_distribution[2];  delta=para_distribution[3];  mu=para_distribution[4];

## para_distribution<-c(5, 1.397610234,   0.007012446,  -0.286886696)

## para_distribution<-c(2.237095e+01, -1.047143e+01,  7.135328e-03,  8.048087e-04)

## para_distribution<-c(5, 1.397610234,   0.007012446,  -0.286886696)

## para_distribution<-c(200.237095e+01 ,-1.047143e+01,  7.135328e-03 , 80.048087e-04) #### RMSE2$rmse: 0.01082957

## ## para_distribution=c(1.269943539, -0.002488772,  1.620073612,  2.088112106)

para_distribution=c(1.3589, -0.0058 ,  1.5336, 7.9908)
# 
# Time difference of 11.50057 secs
# > 
#   > Sol
# $par
# [1] 6.322308e-06 1.240908e-01 2.314260e-02 8.504271e-01 1.510303e-01 9.982321e-01
# 
# $value
# [1] -7978.421
# 
# $counts
# function gradient 
# 377       NA 
# 
# $convergence
# [1] 0
# 
# $message
# NULL
# 
# > para_h1<-Sol$par
# > para_h
# [1] 5.192896e-06 1.240918e-01 2.314273e-02 8.504267e-01 1.821112e-01 9.657773e-01
# > para_h1
# [1] 6.322308e-06 1.240908e-01 2.314260e-02 8.504271e-01 1.510303e-01 9.982321e-01
# > 
#   > time.taken
# Time difference of 6.283401 secs
# > QMLSol
# $par
# [1]  1.44765637 -0.07609839  1.71016068  2.24960222
# 
# $value
# [1] 3584.715
# 
# $counts
# function gradient 
# 165       NA 
# 
# $convergence
# [1] 0
# 
# $message
# NULL
# 7.609e-02
# > 