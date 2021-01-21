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
###      Parameters of the simulation         #######
#####################################################
N= 200
r_0 = 0.013975
list.SK <- c(0.85,0.9,0.95,1,1.05,1.1,1.15,1.2)
list.T <- c(22,46,109,173,234)
l=40
N_sim <- 10
#####################################################
###             date to considere             #######
#####################################################
index_ht = which(Data.returns$date == "2010-01-04")
index_vix = which(Data.ret$date == "2010-01-04")



#####################################################
###         Parameters of the model           #######
#####################################################
##   a0=para_h[1]; a1=para_h[2]; gama=para_h[3];  b1= para_h[4] ;  lamda0= para_h[5] ; ro=para_h[6]
 

###   Initial parameter  ####``
###   Initial parameter  para_h<-c() set up the parameters of the volatility  ####


### ###  para_h<-c(1.180234e-12, 1.547729e-06, 4.550518e+02, 6.500111e-01 ,8.596182e+00, 9.646967e-01)
para_h<-c(3.757e-12, 2.252e-05, 1.423e+01, 9.117e-01 ,1.513e+00, 0.9992)



### para_h<-c(3.313135e-15, 1.366366e-06, 4.274284e+02, 7.324341e-01, 8.531595e+00, 9.95507e-01)  ## RMSE2$rmse : 0.0154167 ## RMSE3$rmse : 0.01615634
### para_h<-c(3.788371e-13, 1.520721e-06, 4.649298e+02, 6.619980e-01, 4.400007e-01, 9.646967e-01)  ## RMSE2$rmse : 0.01082776 ## RMSE3$rmse : 0.03113633
### para_h<-c(3.788371e-13, 1.520721e-06, 4.649298e+02, 6.619980e-01, 6.400007e-01, 9.646967e-01)  ## RMSE2$rmse : 0.0154167 ## RMSE3$rmse : 0.03113633
### para_h<-c(1.905978e-08, 1.522443e-06, 4.642720e+02, 6.620030e-01, 6.399963e-01, 9.646983e-01) ## 0.01083887

#para_h=c(1.854299e-04, 3.345238e-04 ,0.142406e+01 ,1.124012e-03 ,6.573458e-01)
#para_h<-c( 5.881028e-07,  1.506407e-06,  4.550518e+02,  6.500114e-01,  8.596182e+00)

###   Initial parameter  para_distribution<-c() set up the parameters of the distribution   ####
### alpha=para_distribution[1];  beta=para_distribution[2];  delta=para_distribution[3];  mu=para_distribution[4];

## #para_distribution<-c(5, 1.397610234,   0.007012446,  -0.286886696)

## para_distribution<-c(2.237095e+01, -1.047143e+01,  7.135328e-03,  8.048087e-04)

## para_distribution<-c(5, 1.397610234,   0.007012446,  -0.286886696)

## para_distribution<-c(200.237095e+01 ,-1.047143e+01,  7.135328e-03 , 80.048087e-04) #### RMSE2$rmse: 0.01082957

## ## para_distribution=c(1.269943539, -0.002488772,  1.620073612,  2.088112106)

para_distribution=c(1.4365, -0.0538,  1.3920,  11.6243)

#######################################################################
###    Parameters of the model  tow step return-option         #######
#######################################################################
##a0=para_h[1]; a1=para_h[2]; gama=para_h[3];  b1= para_h[4] ;  lamda0= para_h[5]   

###   Initial parameter  ####``
###   Initial parameter  para_h<-c() set up the parameters of the volatility  ####

para_h2=c(1.854299e-04, 3.345238e-04 ,0.142406e+01 ,1.124012e-03 ,6.573458e-01, 9.646967e-01)

#para_h<-c( 5.881028e-07,  1.506407e-06,  4.550518e+02,  6.500114e-01,  8.596182e+00)

###   Initial parameter  para_distribution<-c() set up the parameters of the distribution   ####
### alpha=para_distribution[1];  beta=para_distribution[2];  delta=para_distribution[3];  mu=para_distribution[4];

para_distribution2=c(8.478143e-01, -1.399045e-01,  1.973412e+00, 2.303577e+00)

para_M = c(para_distribution2,para_h2)


#####################################################
###         Parameters of the model           #######
#####################################################
##   a0=para_h[1]; a1=para_h[2]; gama=para_h[3];  b1= para_h[4] ;  lamda0= para_h[5] ; ro=para_h[6]
## para_h<-c(3845e-08, 2.254e-05, 8.272e-01, 5.379e+01 , 1.020e+00, 9.646967e-01)

### alpha=para_distribution[1];  beta=para_distribution[2];  delta=para_distribution[3];  mu=para_distribution[4];
## para_distribution=c(1.2501, -0.0106,  1.4728,  2.7086)


# 
# > time.taken <- end.time - start.time
# > time.taken
# Time difference of 7.31059 secs
# > 
#   > Sol
# $par
# [1] 4.283064e-07 1.537493e-06 4.607389e+02 6.510114e-01 8.596181e+00 9.646984e-01
# 
# $value
# [1] -7788.42
# 
# $counts
# function gradient 
# 309       NA 
# 
# $convergence
# [1] 0
# 
# $message
# NULL
# 
# > para_h1<-Sol$par
# > para_h
# [1] 1.180234e-12 1.547729e-06 4.550518e+02 6.500111e-01 8.596182e+00 9.646967e-01
# > para_h1
# [1] 4.283064e-07 1.537493e-06 4.607389e+02 6.510114e-01 8.596181e+00 9.646984e-01
# > #                QML estimation  NIG                     # 
#   > ##########################################################
# 
# > ##########################################################
# > start.time <- Sys.time()
# > QMLSol=optim(para_distribution,NIG_likelihood_dens_QML ,para_h =para_h1,Data.returns=Data.returns, method="Nelder-Mead",control = list(maxit = 5000))
# > end.time <- Sys.time()
# > time.taken <- end.time - start.time
# > time.taken
# Time difference of 4.003996 secs
# > QMLSol
# $par
# [1]  1.27567837 -0.05863686  1.52691996  2.29519637
# 
# $value
# [1] 3552.764
# 
# $counts
# function gradient 
# 129       NA 
# 
# $convergence
# [1] 0
# 
# $message
# NULL
# 
# > 
#   > para_distribution1= QMLSol$par
# > 