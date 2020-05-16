
#####################################################
###         Parameters of the model           #######
#####################################################
### a0=para_h[1]; a1=para_h[2]; gama=para_h[3];  b1= para_h[4] ;  lamda0= para_h[5] ; ro=para_h[6]


para_h<-c(5.282379e-13, 2.252557e-05 ,8.143868e+00 ,9.154310e-01 ,1.026485e-0, 8.784247e-01)

para_h<-c(2.264912e-12, 2.252835e-05, 1.293543e+01, 9.140285e-01, 1.514392e+00, 9.999262e-01)
Time difference of 1.030301 mins
> 
  > para_h1<-Sol$par
> Sol
$`par`
[1] 2.264912e-12 2.252835e-05 1.293543e+01 9.140285e-01 1.514392e+00 9.999262e-01

$value
[1] -6001.114

$counts
function gradient 
1179       NA 

$convergence
[1] 0

$message
NULL


> # Standard error
  > Hess=fdHess(para_h1,Heston_likelihood_MixViX, Data.returns=Data.returns)
> S_e <- sqrt(diag(solve(nearPD(Hess$Hessian)$mat)))
> S_e
[1] 8.991119e-03 2.690445e-06 8.991132e-03 9.539580e-03 8.991142e-03 8.991118e-03

> Persistence(para_h1)
$`PersiHisto`
[1] 0.9177981

$PersiNeutral
[1] 0.9190635

> MPE
[1] -0.00473326
> MAE
[1] 0.0549846
> MAE2
[1] 0.0549846
> Vrmse
[1] 0.275581
















