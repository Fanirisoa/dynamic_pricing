#####################################################
###                 Solution 1                #######
#####################################################

#####################################################
###                step 1                     #######
#####################################################
### a0=para_h[1]; a1=para_h[2]; gama=para_h[3];  b1= para_h[4] ;  lamda0= para_h[5] ; ro=para_h[6]
para_h<-c(5.282379e-08, 2.252557e-04 ,8.143868e+00 ,9.154310e-01 ,0.7426485e-00, 9.784247e-01) ##  RMSE2$rmse :0.05127656 RMSE3$rmse : 0.01818576

### Time

Time difference of 42.23594 secs

### Sol

> Sol
$par
[1] 5.079420e-06 2.438567e-04 8.338232e+00 4.666841e-01 1.041037e+00 9.999992e-01

$value
[1] -5542.751

$counts
function gradient 
849       NA 

$convergence
[1] 0

$message
NULL

#####################################################
###                step 2                     #######
#####################################################
### alpha=para_distribution[1];  beta=para_distribution[2];  delta=para_distribution[3];  mu=para_distribution[4];
> para_distribution<-c(5, 1.397610234,   0.007012446,  -0.286886696)
### Time

Time difference of 14.2111 secs

### Sol

> QMLSol
$par
[1]  0.65669399 -0.01053073  0.85871693  2.26139849

$value
[1] 3245.572

$counts
function gradient 
241       NA 

$convergence
[1] 0

$message
NULL

#####################################################
###                RMSE 2009                  #######
#####################################################
> RMSE2$rmse
[1] 0.05217287

#####################################################
###                RMSE 2010                  #######
#####################################################
> RMSE1$rmse
[1] 0.06288734


#####################################################
###        Average Volatility Risk Premium    #######
#####################################################
MVRP = mean(ts.VRP_vix_NIG) 
> MVRP
[1] -0.03373284
> 100*MVRP
[1] -3.373284


###################################### 
###               Gaussian     #######
###################################### 
#####################################################
###        Average Volatility Risk Premium    #######
#####################################################
MVRP = mean(ts.VRP_vix_Gaus) 
> MVRP
[1] 2.65348e-07
> 100*MVRP
[1] 2.65348e-05





#####################################################
###                 Solution ony returns      #######
#####################################################

#####################################################
###                step 1                     #######
#####################################################
### a0=para_h[1]; a1=para_h[2]; gama=para_h[3];  b1= para_h[4] ;  lamda0= para_h[5] ; ro=para_h[6]

para_h<-c(5.282379e-08, 2.252557e-04 ,8.143868e+00 ,9.154310e-01 ,0.7426485e-00) ##  RMSE2$rmse :0.05127656 RMSE3$rmse : 0.01818576

### Time

Time difference of  17.07768 secs

### Sol

> Sol_ret
$par
[1]  3.969848e-06  2.131449e-04  1.035300e+01  6.023343e-01 -4.999999e-01 

$value
[1] -7916.801

$counts
function gradient 
815       NA 

$convergence
[1] 0

$message
NULL

#####################################################
###                step 2                     #######
#####################################################
> para_distribution<-c(5, 1.397610234,   0.007012446,  -0.286886696)
### Time

Time difference of 17.43437 secs

### Sol


> QMLSol
$par
[1] 0.710752756 0.003912676 1.157927889 2.824464773

$value
[1] 3029.421

$counts
function gradient 
261       NA 

$convergence
[1] 0

$message
NULL


#####################################################
###                RMSE 2009                  #######
#####################################################
> RMSE2$rmse
[1] 0.05691639


#####################################################
###                RMSE 2010                  #######
#####################################################
> RMSE1$rmse
[1] 0.06770124


#####################################################
###        Average Volatility Risk Premium    #######
#####################################################
MVRP = mean(ts.VRP_vix_NIG) 
> MVRP
[1] -0.03141664
> 100*MVRP
[1] -3.141664

###################################### 
###               Gaussian     #######
###################################### 
#####################################################
###        Average Volatility Risk Premium    #######
#####################################################
MVRP = mean(ts.VRP_vix_Gaus) 
> MVRP
[1] 1.761025e-14
> 100*MVRP
[1] 1.761025e-12

