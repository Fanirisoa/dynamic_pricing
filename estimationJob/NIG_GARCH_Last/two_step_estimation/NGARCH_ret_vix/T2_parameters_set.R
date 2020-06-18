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
##    a0=para_h[1]; b1=para_h[2]; a1=para_h[3];  gama= para_h[4]; lambda= para_h[5];  ro=para_h[6]
 

###   Initial parameter  ####``
###   Initial parameter  para_h<-c() set up the parameters of the volatility  ####
  

##para_h<-c(4.705257e-06,  7.957262e-01 , 6.170762e-02 , 1.394690e+00  ,5.144851e-02  ,1.795145e+00 ,-2.685911e-01 , 9.541714e-01)


###   Initial parameter  para_distribution<-c() set up the parameters of the distribution   ####
### alpha=para_distribution[1];  beta=para_distribution[2];  delta=para_distribution[3];  mu=para_distribution[4];

para_h<-c(1.603e-06 , 0.7957  ,0.06175 , 1.146, 0.03736 , 0.95417) 

###   Initial parameter  para_distribution<-c() set up the parameters of the distribution   ####
### alpha=para_distribution[1];  beta=para_distribution[2];  delta=para_distribution[3];  mu=para_distribution[4];

para_distribution=c(1.269943539, -0.002488772,  1.8970073612,  2.088112106)


##para_distribution<-c(5, 1.397610234,   0.007012446,  -0.286886696)
## para_distribution<-c(2.237095e+01, -1.047143e+01,  7.135328e-03,  8.048087e-04)
## para_distribution<-c(5, 1.397610234,   0.007012446,  -0.286886696)
## para_distribution<-c(200.237095e+01 ,-1.047143e+01,  7.135328e-03 , 80.048087e-04) #### RMSE2$rmse: 0.01082957
##para_distribution=c(1.269943539, -0.002488772,  1.620073612,  2.088112106)


# 
# time.taken
# Time difference of 10.72467 secs
# > 
#   > Sol
# $par
# [1] 3.583930e-06 7.956879e-01 6.174894e-02 1.487816e+00 2.839657e-03 9.541882e-01
# 
# $value
# [1] -7615.632
# 
# $counts
# function gradient 
# 433       NA 
# 
# $convergence
# [1] 0
# 
# $message
# NULL
# 
# > para_h1<-Sol$par
# > para_h
# [1] 1.6030e-06 7.9570e-01 6.1750e-02 1.1460e+00 3.7360e-02 9.5417e-01
# > para_h1
# [1] 3.583930e-06 7.956879e-01 6.174894e-02 1.487816e+00 2.839657e-03 9.541882e-01
# > ##########################################################
# 
# time.taken
# Time difference of 8.785261 secs
# > QMLSol
# $par
# [1]  1.665984840 -0.002144004  2.141671838  1.234367968
# 
# $value
# [1] 3491.764
# 
# $counts
# function gradient 
# 271       NA 
# 
# $convergence
# [1] 0
# 
# $message
# NULL
# 
# > 