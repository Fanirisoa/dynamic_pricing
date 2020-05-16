############################################### 
###          Resultat Generale          #######
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

# RMSE
> RMSE1$in
[1] 0.056915937
> RMSE2$out
[1] 0.07219975
> RMSE2$we
[1] 0.0631625

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


