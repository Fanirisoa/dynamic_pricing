#####################################################
###         Parameters of the model           #######
#####################################################
##a0=para_h[1]; a1=para_h[2]; gama=para_h[3];  b1= para_h[4] ;  lamda0= para_h[5]   

###   Initial parameter  ####
para_h<-c(5.282379e-13, 2.252557e-05 ,8.143868e+00 ,9.154310e-01 ,1.026485e-0) ##  RMSE2$rmse : 0.02233918 RMSE3$rmse : 0.01818576

> time.taken
Time difference of 9.480663 secs
> 
  > para_h1<-Sol$par
> Sol
$`par`
[1] 2.697046e-13 2.250781e-05 8.973734e+00 8.793940e-01 1.037815e+00

$value
[1] -8280.123

$counts
function gradient 
397       NA 

$convergence
[1] 0

$message
NULL


> # Standard error
  > Hess=fdHess(para_h1,Heston_likelihood_ret, Data.returns=Data.returns)
> S_e <- sqrt(diag(solve(nearPD(Hess$Hessian)$mat)))
> S_e
[1] 1.404920e-02 1.468525e-06 1.405362e-02 1.450208e-02 1.423384e-02


> MPE
[1] -0.04905288
> MAE
[1] 0.06570952
> MAE2
[1] 0.06570952
> Vrmse
[1] 0.2867499

> Persistence(para_h1)
$`PersiHisto`
[1] 0.8812065

$PersiNeutral
[1] 0.881881
