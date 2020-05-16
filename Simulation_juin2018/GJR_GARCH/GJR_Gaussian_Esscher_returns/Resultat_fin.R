############################################### 
###          Resultat Generale          #######
############################################### 
##a0=para_h[1]; a1=para_h[2]; a2=para_h[3];  b1= para_h[4] ;  lamda0= para_h[5] 

para_h<-c(6.094e-11, 1.240e-01, 2.314e-02, 4.011e-01, 3.025e-02) 

para_h1<-c(1.445949e-05, 3.107684e-01, 1.055816e-01, 6.311811e-01, 4.208730e-03)
time.taken
Time difference of 10.57245 secs

# Solution
> Sol
$`par`
[1] 1.445949e-05 3.107684e-01 1.055816e-01 6.311811e-01 4.208730e-03

$value
[1] -8284.952

$counts
function gradient 
556       NA 

$convergence
[1] 0

$message
NULL

# Standard error
[1] 1.698409e-06 1.139433e-02 9.096206e-03 1.105324e-02 8.979822e-03

# RMSE
> RMSE1$in
[1] 0.0575311
> RMSE2$out
[1] 0.07227221
> RMSE2$we
[1] 0.06368361

> MPE
[1] 0.4439605
> MAE
[1] 0.6298235
> MAE2
[1] 0.6298235
> Vrmse
[1] 18.45451




