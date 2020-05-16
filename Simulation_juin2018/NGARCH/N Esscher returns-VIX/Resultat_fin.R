############################################### 
###          Resultat Generale          #######
############################################### 
## a0=para_h[1]; b1=para_h[2]; a1=para_h[3];  gama= para_h[4]; lambda= para_h[5]; a=para_h[6]; b=para_h[7] ;  ro=para_h[8]## ; c=para_h[5]; d=para_h[6] ; ro=para_h[8]

para_h<-c(1.603e-06 , 0.7957  ,0.06175 , 1.146, 0.03736 , 3.805 , -0.7806, 0.95417)

time.taken
Time difference of 123.454843 sec

# Solution
> Sol
$`par`
[1]  4.705257e-06  7.957262e-01  6.170762e-02  1.394690e+00  5.144851e-02  1.795145e+00 -2.685911e-01  9.541714e-01

$value
[1] 12770.5

$counts
function gradient 
1267       NA 

$convergence
[1] 0

$message
NULL

# Standard error
[1] 2.507650e-07 6.415229e-04 5.511385e-04 5.550372e-04 5.545759e-04 5.367108e-04 5.367107e-04 5.386347e-04

# RMSE
> RMSE1$in
[1] 0.05729999
> RMSE2$out
[1] 0.072078389
> RMSE2$we
[1] 0.06307517

> MPE
[1] -0.5589676
> MAE
[1] 0.566944
> MAE2
[1] 0.566944
> Vrmse
[1] 18.14527

> Persistence(para_h1)
$`PersiHisto`
[1] 0.9774649

$PersiNeutral
[1] 0.9247763

