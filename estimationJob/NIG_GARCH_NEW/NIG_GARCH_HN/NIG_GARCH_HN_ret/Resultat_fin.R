############################################### 
###          Resultat Generale          #######
############################################### 
## a0=para_h[1]; b1=para_h[2]; a1=para_h[3];  gama= para_h[4]; lambda= para_h[5]; a=para_h[6]; b=para_h[7] ;  ro=para_h[8]## ; c=para_h[5]; d=para_h[6] ; ro=para_h[8]

para_h<-c(2.697046e-13 ,2.250781e-05, 8.973734e+00, 8.793940e-01, 1.037815e+00)

time.taken
Time difference of 123.454843 sec

# Solution
> Sol
$`par`
[1] 1.862873e-12 2.250036e-05 4.804450e+01 8.274026e-01 4.921045e+00

$value
[1] -8308.58

$counts
function gradient 
735       NA 

$convergence
[1] 0

$message
NULL


# Standard error
[1]  2.091005e-12 2.091005e-08 2.091004e-08 2.091004e-08 2.091004e-08


> QMLSol
$`par`
[1]  1.24017703 -0.03604831  1.42421603  1.78017616

$value
[1] 3605.19

$counts
function gradient 
201       NA 

$convergence
[1] 0

$message
NULL

[1]  0.099042750  0.008793694  0.00135326644 0.0087537762021

# RMSE
> RMSE1$in
[1] 0.05351607
> RMSE2$out
[1] 0.07095312
> RMSE2$we
[1] 0.06034915

> MPE
[1] -0.5589676
> MAE
[1] 0.566944
> MAE2
[1] 0.566944
> Vrmse
[1] 18.14527


