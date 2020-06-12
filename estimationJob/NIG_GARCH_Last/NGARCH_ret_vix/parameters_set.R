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

#####################################################
###         Parameters of the model           #######
#####################################################`
## a0=para_h[1]; b1=para_h[2]; a1=para_h[3];  gama= para_h[4]; lambda= para_h[5];  ro=para_h[6]   
## a0=para_M[5]; b1=para_M[6]; a1=para_M[7];  gamma= para_M[8] ;  lamda= para_M[9]  ; ro=para_M[10]

###   Initial parameter  ####``
###   Initial parameter  para_h<-c() set up the parameters of the volatility  ####

## para_h<-c(1.603e-06 , 0.8447  ,0.06175 , 1.146, 0.03736, 9.546611e-01)   
## para_h<-c(2.157e-03 , 0.6358  ,0.0954 , 0.545, 0.08762, 9.546611e-01 )
## para_h<-c(1.157e-03 , 0.5358  ,0.0954 , 0.745, 0.08762, 9.546611e-01 )  
para_h<-c(3.205257e-06  ,7.957262e-01  ,6.170762e-02 , 1.394690e+00 , 5.144851e-02, 9.546611e-01)


###   Initial parameter  para_distribution<-c() set up the parameters of the distribution   ####
### alpha=para_distribution[1];  beta=para_distribution[2];  delta=para_distribution[3];  mu=para_distribution[4];

 para_distribution<-c(5, 1.397610234,   0.007012446,  -0.286886696)
### para_distribution<-c(48.4956802775, -5.5025899512 , 0.0091739853 , 0.0003863926)
###para_distribution<-c(2.237095e+01, -1.047143e+01,  7.135328e-03,  8.048087e-04)

  
 para_M = c(para_distribution,para_h)
 
 > time.taken
 Time difference of 31.62563 secs
 > 
   > Sol
 $par
 [1]  5.090424e+00  1.556652e+00  6.213178e-03  8.240468e-04  2.659985e-05  7.956720e-01  6.163473e-02  1.105530e+00 -2.493983e-02  9.541983e-01
 
 $value
 [1] -5728.951
 
 $counts
 function gradient 
 907       NA 
 
 $convergence
 [1] 0
 
 $message
 NULL
 