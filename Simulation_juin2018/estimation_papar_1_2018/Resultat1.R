
#####################################################
###                 Data set                  #######
#####################################################
load("DataPrice2009.Rdata")
Data.N=Data.N2

Sol

para_h1<-Sol$par

################################
###   Values of indicator  #####
################################
Persistence5(para_h1)

PLAL(para_h1)

######################
####  Values RMSE   ##
######################
RMSE1=RMSE(para_h1,Data.ret,Data.N)

RMSE1$rmse
RMSE1$norm_rmse

##############################
###   Table Test GMM   #######
##############################
Table_RMSE(para_h=para_h,Data.ret=Data.ret, Data.N=Data.N)

##########################
###   Values Test  #######
##########################
Testl(para_h1,Data.returns)

#####################################################
###    Load both data.contract and data.ret   #######
#####################################################
load("DataPrice2009test.Rdata")

##############################
###   Table Test GMM   #######
##############################
Table_GMM_test(para_h1, Data.returns)

#################################
###   2010 out of sample   ######
#################################
load("DataPrice2010.Rdata")
######################
####  Values RMSE   ##
######################
RMSE1=RMSE(para_h1,Data.ret,Data.N)

RMSE1$rmse
RMSE1$norm_rmse

##############################
###   Table Test GMM   #######
##############################
Table_RMSE(para_h=para_h,Data.ret=Data.ret, Data.N=Data.N)

