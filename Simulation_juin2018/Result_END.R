############################################### 
###          Resultat Generale          #######
############################################### 

############################################### 
###      1- HN_GARCH_ESS_Returns        #######
############################################### 
##a0=para_h[1]; a1=para_h[2]; gama=para_h[3];  b1= para_h[4] ;  lamda0= para_h[5]  

para_h=c(2.697046e-13, 2.250781e-05, 8.973734e+00, 8.793940e-01, 1.037815e+00)


Time difference of 24.47941 secs

> Sol
$`par`
[1] 3.854858e-08 2.254897e-05 5.379865e+01 8.272017e-01 1.020885e+00

$value
[1] -8318.233

$counts
function gradient 
795       NA 

$convergence
[1] 0

$message
NULL


se_1=c(1.404920e-02, 1.468525e-06, 1.405362e-02, 1.450208e-02, 1.423384e-02)

> RMSE1$in
[1] 0.05742586
> RMSE2$out
[1] 0.07281952
> RMSE1$we
[1] 0.06380394

> MPE
[1] -0.03518883
> MAE
[1] 0.05757677
> Vrmse
[1] 28.29303
> VRP
[1] 2.486516e-15


> Persistence(para_h1)
$`PersiHisto`
[1] 0.892465

$PersiNeutral
[1] 0.8962072

############################################# 
###        2- GJR_GARCH_Returns      #######
############################################# 
##a0=para_h[1]; a1=para_h[2]; a2=para_h[3];  b1= para_h[4] ;  lamda0= para_h[5] 
para_h<-c(6.094e-11, 1.240e-01, 2.314e-02, 4.011e-01, 2.025e-02) 

> Sol
$`par`
[1] 3.049628e-06 1.243608e-01 2.208629e-02 8.509495e-01 2.288715e-01

$value
[1] -8418.936

$counts
function gradient 
488       NA 

$convergence
[1] 0

$message
NULL

se_11=C(1.698409e-06, 1.139433e-02, 9.096206e-03, 1.105324e-02, 8.979822e-03)


# RMSE
> RMSE1$in
[1] 0.05753115
> RMSE2$out
[1] 0.07227221
> RMSE2$we
[1] 0.06368036


> MPE
[1] -0.0608578
> MAE2
[1] 0.06351164
> Vrmse
[1] 18.02739
> VRP
[1] 7.711468e-15

> Persistence(para_h1)
$`PersiHisto`
[1] 0.9863535

$PersiNeutral
[1] 0.9975146


####################################### 
###     3- NGARCH_Returns      #######
####################################### 
# para_h<-c() set up the parameters of the model 
#a0=para_h[1]; b1=para_h[2]; a1=para_h[3];  gama= para_h[4]; lambda= para_h[5];

para_h<-c(4.705257e-06  ,7.957262e-01  ,6.170762e-02 , 1.394690e+00 , 5.144851e-02)

> Sol
$`par`
[1] 1.677898e-06 8.446999e-01 6.174999e-02 1.174353e+00 8.911031e-07

$value
[1] -8473.561

$counts
function gradient 
365       NA 

$convergence
[1] 0

$message
NULL


> S_e
[1] 3.059868e-07 1.781444e-03 1.726538e-03 1.490254e-03 1.490800e-03

# RMSE
> RMSE1$in
[1] 0.0566190
> RMSE2$out
[1] 0.0725719
> RMSE2$we
[1] 0.0636625

> MPE
[1] -0.9941091
> MAE
[1] 0.997299
> MAE2
[1] 0.997299
> Vrmse
[1] 25.2913
> 
> Persistence(para_h1)
$`PersiHisto`
[1] 0.9916096

$PersiNeutral
[1] 0.9298598


############################################### 
###      4- HN_GARCH_ESS_Returns_Option #######
############################################### 
##a0=para_h[1]; a1=para_h[2]; gama=para_h[3];  b1= para_h[4] ;  lamda0= para_h[5]  
para_h=c(1.180234e-12, 1.547729e-06, 4.550518e+02, 6.500111e-01, 8.596182e+00)


> time.taken
Time difference of 26010.08 secs ### 7h
> 
  > para_h1<-Sol$par
> Sol
$`par`
[1] 1.859301e-07 1.542535e-06 4.586904e+02 6.500814e-01 8.596183e+00

$value
[1] -8367.809

$counts
function gradient 
268       NA 

$convergence
[1] 0

$message
NULL

# RMSE 
> RMSE1$in
[1] 0.05589917
> RMSE1$out
[1] 0.0651246
> RMSE1$we
[1] 0.0580351


> MPE
[1] -1.156855
> MAE
[1] 1.156855
> MAE2
[1] 1.156855
> Vrmse
[1] 27.74537

> Persistence(para_h1)
$`PersiHisto`
[1] 0.9746259

$PersiNeutral
[1] 0.9876255


############################################### 
###      5- HN_GARCH_ESS_Returns_VIX    #######
############################################### 
### a0=para_h[1]; a1=para_h[2]; gama=para_h[3];  b1= para_h[4] ;  lamda0= para_h[5] ; ro=para_h[6]
para_h=c(2.264912e-12 ,2.252835e-05 ,1.293543e+01, 9.140285e-01 ,1.514392e+00, 9.999262e-01)

> Sol
$`par`
[1] 3.757071e-12 2.252838e-05 1.423105e+01 9.117358e-01 1.513781e+00 9.999262e-01

$value
[1] -6003.379

$counts
function gradient 
409       NA 

$convergence
[1] 0

$message
NULL

> MPE
[1] -0.02864174
> MAE
[1] 0.5453176
> MAE2
[1] 0.5453176
> Vrmse
[1] 21.75459

> Persistence(para_h1)
$`PersiHisto`
[1] 0.9162983

$PersiNeutral
[1] 0.9176809

############################################### 
###    6- GJR_GARCH_ESS_Returns_VIX    #######
############################################### 
##a0=para_h[1]; a1=para_h[2]; a2=para_h[3];  b1= para_h[4] ;  lamda0= para_h[5]  ; ro=para_h[6]
para_h<-c(5.987174e-06,  1.240911e-01,  2.314265e-02,  8.504269e-01,  3.784983e-02, 9.546611e-01)  ## RMSE1$rmse :  0.06265758 RMSE3$rmse :0.07367674
time.taken
Time difference of  33.06102 secs

# Solution
> Sol
$`par`
[1] 4.966114e-06 1.240920e-01 2.314276e-02 8.504266e-01 1.989254e-01 8.924053e-01

$value
[1] -6075.361

$counts
function gradient 
303       NA 

$convergence
[1] 0

$message
NULL

# Standard error
[1] 1.233970e-06 3.752923e-03 4.757924e-03 3.692452e-03 3.462100e-03 3.419816e-03

> MPE
[1] -0.3462374
> MAE
[1] 0.4104046
> MAE2
[1] 0.4104046
> Vrmse
[1] 13.94554

> Persistence(para_h1)
$`PersiHisto`
[1] 0.9860899

$PersiNeutral
[1] 0.9951557

############################################### 
###    7- NGARCH_ESS_Returns_VIX        #######
############################################### 
##  a0=para_h[1]; b1=para_h[2]; a1=para_h[3];  gama= para_h[4]; lambda= para_h[5];  ro=para_h[6]## 
para_h<-c(1.603e-06 , 0.7957  ,0.06175 , 1.146, 0.03736 , 0.95417)  


> time.taken
Time difference of 50.6441 secs
> Sol
$`par`
[1] 3.557112e-06 7.956829e-01 6.172013e-02 4.701267e-08 8.452460e-01 9.542031e-01

$value
[1] 15830.48

$counts
function gradient 
881       NA 

$convergence
[1] 0

$message
NULL

> S_e
[1] 4.791244e-07 5.650684e-04 5.392512e-04 9.600268e-08 7.414498e-04 5.357432e-04

> MPE
[1] -0.7090141
> MAE
[1] 0.7682272
> MAE2
[1] 0.7682272
> Vrmse
[1] 20.98261
> 

> Persistence(para_h1)
$`PersiHisto`
[1] 0.8574031

$PersiNeutral
[1] 0.8397783







#########################################
###      8-NIG_HN_GARCH_ret       #######
#########################################
##a0=para_h[1]; a1=para_h[2]; gama=para_h[3];  b1= para_h[4] ;  lamda0= para_h[5]  

para_h1=c(3.854858e-08, 2.254897e-05, 5.379865e+01, 8.272017e-01, 1.020885e+00)

para_distribution=c(1.21413539, -0.00152772,  1.51273612,  2.07810600)

> QMLSol
$`par`
[1]  1.2501213 -0.0106488  1.4728206  2.7086839

$value
[1] 3575.137

$counts
function gradient 
187       NA 

$convergence
[1] 0

$message
NULL

> S_e2
[1]   0.10450492   0.01213755   0.14500503  0.00578045


> MPE
[1] -0.3953658
> MAE
[1] 0.4582596
> MAE2
[1] 0.4582596
> Vrmse
[1] 13.87001

#########################################
###      9-NIG_GJR_GARCH_ret      #######
#########################################
##a0=para_h[1]; a1=para_h[2]; a2=para_h[3];  b1= para_h[4] ;  lamda0= para_h[5] 
para_h1<-c(3.049628e-06, 1.243608e-01, 2.208629e-02, 8.509495e-01, 2.288715e-01) 

para_distribution=c(1.337329911, -0.004432882 , 1.551758651 , 1.424519069)

> time.taken
Time difference of 4.183846 secs
> QMLSol
$`par`
[1]  1.270250581 -0.002520285  1.620497081  1.973447871

$value
[1] 3474.53

$counts
function gradient 
91       NA 

$convergence
[1] 0

$message
NULL


#########################################
###     10-NIG_NGARCH_ret         #######
#########################################
# para_h<-c() set up the parameters of the model 
#a0=para_h[1]; b1=para_h[2]; a1=para_h[3];  gama= para_h[4]; lambda= para_h[5];

para_h1<-c(1.677898e-06, 8.446999e-01, 6.174999e-02, 1.174353e+00, 8.911031e-07)
 


para_distribution<-c(1.270250581, -0.002520285 , 1.620497081,  1.973447871)

> time.taken
Time difference of 6.337003 secs
> QMLSol
$`par`
[1]  1.463069410 -0.006153308  1.445472528  2.160217780

$value
[1] 3824.152

$counts
function gradient 
95       NA 

$convergence
[1] 0

$message
NULL

#########################################
###     11-NIG_HN_GARCH_ret_opt   #######
#########################################
##a0=para_h[1]; a1=para_h[2]; gama=para_h[3];  b1= para_h[4] ;  lamda0= para_h[5]  
para_h1 <- c(1.859301e-07, 1.542535e-06, 4.586904e+02, 6.500814e-01, 8.596183e+00)


para_distribution<-c(1.270250581, -0.002520285 , 1.620497081,  1.973447871)


> time.taken
Time difference of 5.990831 secs
> QMLSol
$`par`
[1]  1.43631193 -0.05383502  1.39190718  2.12215480

$value
[1] 3843.215

$counts
function gradient 
95       NA 

$convergence
[1] 0

$message
NULL

> S_e2
[1]  0.115861351  0.009600369  0.001317073 0.095675607 

#########################################
###     12-NIG_HN_GARCH_ret_VIX   #######
#########################################
### a0=para_h[1]; a1=para_h[2]; gama=para_h[3];  b1= para_h[4] ;  lamda0= para_h[5] ; ro=para_h[6]
para_h=c(3.757071e-12, 2.252838e-05, 1.423105e+01, 9.117358e-01, 1.513781e+00, 9.999262e-01)

> time.taken
Time difference of 9.802468 secs
> QMLSol
$`par`
[1]  1.43658868 -0.05383938  1.39204138 11.62439886

$value
[1] 3843.215

$counts
function gradient 
253       NA 

$convergence
[1] 0

$message
NULL


> S_e2
[1]  0.000011618  0.009638893  0.00132145  0.0096070326


#########################################
###    13-NIG_GJR_GARCH_ret_VIX   #######
#########################################
##a0=para_h[1]; a1=para_h[2]; a2=para_h[3];  b1= para_h[4] ;  lamda0= para_h[5]  ; ro=para_h[6]
para_h<-c(4.966114e-06, 1.240920e-01, 2.314276e-02, 8.504266e-01, 1.989254e-01, 8.924053e-01)  

> time.taken
Time difference of 17.63525 secs
> QMLSol
$`par`
[1]  1.358945710 -0.005873246  1.533674479  7.990851698

$value
[1] 3641.984

$counts
function gradient 
271       NA 

$convergence
[1] 0

$message
NULL

> S_e2
[1]  0.0212293356  0.006829313  0.000147233 0.0086535929




#########################################
###    14-NIG_NGARCH_ret_VIX      #######
#########################################
##a0=para_h[1]; a1=para_h[2]; a2=para_h[3];  b1= para_h[4] ;  lamda0= para_h[5]  ; ro=para_h[6]
para_h1<-c(4.966114e-06, 1.240920e-01, 2.314276e-02, 8.504266e-01, 1.989254e-01, 8.924053e-01)  

Time difference of 5.884556 secs
> QMLSol
$`par`
[1]  1.453623310 -0.006191899  1.453848549  2.017873092

$value
[1] 3807.652

$counts
function gradient 
91       NA 

$convergence
[1] 0

$message
NULL

> S_e2
[1]  0.099042750  0.008793694  0.00135326644  0.0087537762021



############################################### 
###    15- HN_GARCH_Qua_Returns_OPtion  #######
############################################### 
##a0=para_h[1]; a1=para_h[2]; gama=para_h[3];  b1= para_h[4] ;  lamda0= para_h[5]   ; Pi=para_h[6] 
para_h=c( 1.859301e-07, 1.542535e-06, 4.586904e+02, 6.500814e-01, 8.596183e+00, 1.672332e+00)
          
> time.taken
Time difference of 1.654441 mins
> 
  > Sol
$`par`
[1] 5.754790e-14 1.513999e-06 4.586904e+02 6.500812e-01 8.596185e+00 1.672330e+00  

$value
[1] -6085.395

$counts
function gradient 
540       NA 

$convergence
[1] 0

$message
NULL

> MAE
[1] 1.201684
> MAE2
[1] 1.201684
> Vrmse
[1] 34.5983
> 
  
  
> Persistence(para_h)
$`PersiHisto`
[1] 0.974626

$PersiNeutral
[1] 0.9881108


############################################### 
###    16- HN_GARCH_Qua_Returns_VIX     #######
############################################### 
##a0=para_h[1]; a1=para_h[2]; gama=para_h[3];  b1= para_h[4] ;  lamda0= para_h[5]   ; Pi=para_h[6] 
para_h=c( 3.757071e-12, 2.252838e-05, 1.423105e+01, 9.117358e-01, 1.513781e+00, 1.452332e+00, 9.999262e-01)

> Sol
$`par`
[1] 1.001455e-12 1.504840e-06 4.586904e+02 6.512107e-01 8.672445e+00 1.722330e+00 8.099270e-01

$value
[1] -6083.805

$counts
function gradient 
524       NA 

$convergence
[1] 0

$message
NULL

> Persistence(para_h1)
$`PersiHisto`
[1] 0.8950536

$PersiNeutral
[1] 0.8966328

> Persistence(para_h)
$`PersiHisto`
[1] 0.9162983

$PersiNeutral
[1] 0.9178476

> MPE
[1] 0.01875063
> MAE
[1] 0.6028241
> MAE2
[1] 0.6028241
> Vrmse
[1] 23.32204

#para_h1=c(5.754790e-10, 1.5627529e-06, 4.586904e+02 ,6.512112e-01, 8.672445e+00, 1.722330e+00, 8.099267e-01 )
#para_h1=c(5.754790e-10, 1.5627529e-06, 4.586904e+02 ,6.500812e-01, 8.596185e+00, 1.722330e+00, 8.099267e-01 )
#para_h=c(5.754790e-10, 1.5627529e-06, 4.586904e+02 ,6.500812e-01, 8.596185e+00, 1.722330e+00, 8.099267e-01 )

############################################### 
###    17- GJR_GARCH_Qua_Returns_VIX    #######
############################################### 
##a0=para_h[1]; a1=para_h[2]; a2=para_h[3];  b1= para_h[4] ;  lamda0= para_h[5]  ; Pi=para_h[6]  ; ro=para_h[7]

para_h = c(5.987174e-06, 1.240911e-01, 2.314265e-02, 8.504269e-01, 3.784983e-02 ,1.243163e+00, 9.546611e-01) 

> Sol
$`par`
[1] 4.966114e-06 1.240920e-01 2.314276e-02 8.504266e-01 1.989254e-01  1.278596e+00 8.924053e-01

$value
[1] -6075.361

$counts
function gradient 
303       NA 

$convergence
[1] 0

$message
NULL

> Persistence(para_h1)
$`PersiHisto`
[1] 0.9860899


MPE
[1] -0.3462374
> MAE
[1] 0.4104046
> MAE2
[1] 0.4104046
> Vrmse
[1] 13.94554

############################################
###    18- NGARCH_Qua_Returns_VIX    #######
############################################
## a0=para_h[1]; b1=para_h[2]; a1=para_h[3];  gama= para_h[4]; lambda= para_h[5];  ; Pi=para_h[6]  ; ro=para_h[7]
para_h=c(1.783635e-06 ,9.329418e-01, 3.877832e-02, 8.775196e-08, 4.582560e-01,1.243163e+00, 9.170033e-01) 

> Sol
$`par`
[1] 1.780075e-06 9.329418e-01 3.877833e-02 1.277575e-07 4.583290e-01  1.24163e+00 9.170840e-01

$value
[1] 17015.94

$counts
function gradient 
387       NA 

$convergence
[1] 0

$message
NULL

> MPE
[1] -0.3958877
> MAE
[1] 0.4588599
> MAE2
[1] 0.4588599
> Vrmse
[1] 13.88213

> Persistence(para_h1)
$`PersiHisto`
[1] 0.9717201


############################################
###    19- NGARCH_EGP_Returns_VIX    #######
############################################
##    a0=para_h[1]; b1=para_h[2]; a1=para_h[3];  gama= para_h[4]; lambda= para_h[5]; a=para_h[6]; b=para_h[7] ; c=para_h[8]; d=para_h[9] ; ro=para_h[10]
para_h=c(1.783635e-06 ,9.329418e-01, 3.877832e-02, 8.775196e-08, 4.582560e-01,2.7878 , -1.07865,1.478454, 0.6203576, 9.170033e-01)

> Sol
$`par`
[1]  1.896830e-06  9.329402e-01  3.877904e-02  7.110437e-01  9.937950e-02  2.960932e+00 -9.441804e-01
[8]  1.587764 0.5341928  9.163709e-01

$value
[1] 12471.15

$counts
function gradient 
915       NA 

$convergence
[1] 0

$message
NULL

> Persistence(para_h1)
$`PersiHisto`
[1] 0.9913253

$PersiNeutral
[1] 0.9584097

> MPE
[1] -0.3706225
> MAE
[1] 0.4150804
> MAE2
[1] 0.4150804
> Vrmse
[1] 12.751

###################################### 
###    20- IG_GARCH_Returns      ######
######################################
##  w=para_h[1]; b=para_h[2]; a=para_h[3];  c= para_h[4]; neta=para_h[5] ; nu=para_h[6];

> Sol
$`par`
[1]  9.817584e-06  1.215612e-03  3.322312e+03  4.542147e-05 -7.531277e-03  1.258401e+02

$value
[1] -7873.833

$counts
function gradient 
311       NA 

$convergence
[1] 0

$message
NULL

> S_e
[1] 2.133501e-07 2.578211e-04 2.201296e-04 1.168901e-07 2.010895e-05 2.201303e-04
> 
# RMSE
> RMSE1$in
[1] 0.03436143
> RMSE2$out
[1] 0.0454152
> RMSE2$we
[1] 0.04040568


> MPE
[1] 1.587222
> MAE
[1] 1.587713
> MAE2
[1] 1.587713
> Vrmse
[1] 38.32732

> Persistence(para_h1)
$`PersiHisto`
[1] 0.9904574

$PersiNeutral
[1] 0.9608998


########################################### 
###    21- IG_GARCH_Returns_VIX       ######
###########################################
##  w=para_h[1]; b=para_h[2]; a=para_h[3];  c= para_h[4]; neta=para_h[5] ; nu=para_h[6]; ro=para_h[7];
para_h=c(9.427303e-06,  2.051123e-03 , 3.317425e+03 , 4.725221e-05 ,-7.973112e-03 , 1.258399e+02  ,9.746611e-01)


$`par`
[1]  1.013422e-05  2.073402e-03  3.318154e+03  4.506369e-05 -7.471968e-03  1.258604e+02  9.955283e-01

$value
[1] 4429.299

$counts
function gradient 
770       NA 

$convergence
[1] 0

$message
NULL



> Persistence(para_h)
$`PersiHisto`
[1] 0.9946466

$PersiNeutral
[1] 0.9595388

> MPE
[1] 1.318778
> MAE
[1] 1.335544
> MAE2
[1] 1.335544
> Vrmse
[1] 31.90826
#####################################################
###        22- IG_GARCH_Returns_option         #######
#####################################################
##w=para_h[1]; b=para_h[2]; a=para_h[3];  c= para_h[4]; neta=para_h[5] ; nu=para_h[6]  

> para_h
[1]  9.918251e-06  1.015820e-03  3.331742e+03  4.543874e-05 -7.531312e-03  1.259401e+02
> Sol
$`par`
[1]  9.769906e-06  1.015940e-03  3.331743e+03  4.537934e-05 -7.531415e-03  1.259401e+02

$value
[1] -7874.518

$counts
function gradient 
335       NA 

$convergence
[1] 0

$message
NULL

> Persistence(para_h1)
$`PersiHisto`
[1] 0.9900273

$PersiNeutral
[1] 0.9609958

> MPE
[1] 1.305777
> MAE
[1] 1.324154
> MAE2
[1] 1.324154
> Vrmse
[1] 31.71919
> 



##################################################
###        24- IG_GARCH_Ushp_Returns_option  #####
##################################################
#   w=para_h[1]; b=para_h[2]; a=para_h[3];  c= para_h[4]; neta=para_h[5] ; nu=para_h[6] ; PI=para_h[7] 
para_h=c( 3.238940e-06, 2.058376e-03,3.317425e+03,5.058743e-05,-8.281782e-03, 1.258400e+02, 1.235371e+00)


> Sol
$`par`
[1]  1.018542e-05  1.721115e-03  3.317425e+03  4.511892e-05 -7.493686e-03  1.257392e+02  1.400530e+00

$value
[1] 5071.5

$counts
function gradient 
1125       NA 

$convergence
[1] 0

$message
NULL



> Persistence(para_h1)
$`PersiHisto`
[1] 0.9910078

$PersiNeutral
[1] 0.9609989

> Persistence(para_h)
$`PersiHisto`
[1] 0.9909854

$PersiNeutral
[1] 0.9610848

> MPE
[1] 1.110999
> MAE
[1] 1.16621
> MAE2
[1] 1.16621
> Vrmse
[1] 27.79215

##################################################
###        23- IG_GARCH_Ushp_Returns_VIX    ######
##################################################
#   w=para_h[1]; b=para_h[2]; a=para_h[3];  c= para_h[4]; neta=para_h[5] ; nu=para_h[6] ; PI=para_h[7] ; ro=para_h[8]

para_h=c(1.003502e-05 , 2.417558e-03 , 3.317749e+03,  4.525727e-05, -7.519624e-03 , 1.258809e+02,  1.100000e+00,  9.959896e-01)

> Sol
$`par`
[1]  1.023104e-05  1.911214e-03  3.317425e+03  4.504673e-05 -7.483091e-03  1.258400e+02  1.632461e+00  9.939197e-01

$value
[1] 5302.434

$counts
function gradient 
1049       NA 

$convergence
[1] 0

$message
NULL

> Persistence(para_h1)
$`PersiHisto`
[1] 0.99213

$PersiNeutral
[1] 0.9603104


> MPE
[1] 1.041717
> MAE
[1] 1.153224
> MAE2
[1] 1.153224
> Vrmse
[1] 27.66127