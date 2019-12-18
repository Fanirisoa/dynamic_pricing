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
###         Parameters of the  NIG model      #######
#####################################################
###   Initial parameter  ####
##    a0=para_h[1]; b1=para_h[2]; a1=para_h[3];  gama= para_h[4]; lambda= para_h[5]; a=para_h[6]; b=para_h[7] ;  ro=para_h[8]## ; c=para_h[5]; d=para_h[6] ; ro=para_h[8]
##  para_h<-c(1.016620e-05,  2.041894e-03,  3.317506e+03,  4.501851e-05, -7.465712e-03,  1.258399e+02,  9.945561e-01) 

para_h<-c(1.603e-06 , 0.7957  ,0.06175 , 1.146, 0.03736 , 3.805 , -0.7806, 0.95417)  ## RMSE2$rmse :    RMSE3$rmse :  0.04496318

##  para_h<-c(2.157e-03 , 0.6358  ,0.0954 , 0.545, 0.08762 , 2.7878 , -1.07865, 0.85687)  ## RMSE2$rmse :    RMSE3$rmse :  0.07741942

##  para_h<-c(1.157e-03 , 0.5358  ,0.0954 , 0.745, 0.08762 , 2.7878 , -1.02865, 0.92687)  ## RMSE2$rmse :    RMSE3$rmse :  0.007396033

##  para_h<-c(4.966114e-06, 1.240920e-01, 2.314276e-02, 8.504266e-01, 1.989254e-01, 1.258399e+02,  9.945561e-01) 

##  para_h<-c(1.016620e-05,  2.041894e-03,  3.317506e+03,  4.501851e-05, 7.465712e-03,  1.258399e+02,  9.945561e-01) 

##  para_h=c(1.783635e-06 ,9.329418e-01, 3.877832e-02, 8.775196e-08, 4.582560e-01,2.7878 , -1.07865, 9.170033e-01) 


#####################################################
###      Number of simulation in the MC       #######
#####################################################
N_1<-c(1500, 2500, 3500, 4500, 5500)
N_2<-c(6500, 7500, 8500, 9500)  
N_3<- 2^10


N = 5500



##N = 1500
##Time difference of 3.900022 hours
## RMSE2$rmse
##[1] 0.006536464
##> RMSE2$norm_rmse
##[1] 0.005780608

##N = 2500
##Time difference of 8.938065 hours
##RMSE2$rmse
##[1] 0.006545419
##RMSE2$norm_rmse
[##0.005788528

##N =3500
##Time difference of 19.82656 hours
##RMSE2$rmse
##[1] 0.006534357
##RMSE2$norm_rmse
##[1] 0.005778745

##4500
##Time difference of 1.027597 days
##RMSE2$rmse
##[1] 0.006540203, 
##RMSE2$norm_rmse
##[1] 0.005783915

##N = 5500 
##Time difference of 1.105039 days
##RMSE2$rmse
##[1] 0.006537957
##RMSE2$norm_rmse
##[1] 0.005781929  
  
  
  runif(20, min =1.531122, max = 19.69681) 
  

  rnorm(20, mean = 0.05929, sd = 0.0053) 
  
  
  
x <- c(15000, 25000, 35000, 45000, 55000,  65000,75000,85000,100000,125000)
N_liste <-    c(15000, 25000, 35000, 45000, 55000,  65000,75000,85000,100000,125000)

############################################################
####              Plot the time in                        ##
############################################################
time_sim_in_G.GJR.Ret.VIX.Ess <- c(1.401163 ,3.152843 ,2.460967 ,3.421017 ,4.409233 ,13.59997 ,9.391688 ,11.84392 ,8.219551 ,12.69681)
time_sim_in_G.GJR.Ret.VIX.Qua <- c(2.017113 ,2.745760 ,3.219285 ,3.337781 ,4.545135 ,7.594880 ,9.148706 ,10.24158 ,13.01430 ,15.358608)
time_sim_in_G.GJR.Ret.Ess <- c(1.536341 ,2.503385 ,2.804722 ,3.125717 ,4.650576 ,5.545035 ,6.139679 ,9.014243 ,11.75729 ,13.387144)
time_sim_in_NIG.NGARCH.Ret.VIX.Ess <- c(3.900022, 4.938065, 6.338507, 7.357821, 9.82656, 10.027597, 13.105039,16.52500, 17.34413,20.341950)


plot(N_liste, time_sim_in_G.GJR.Ret.VIX.Ess, type = "b", frame = FALSE, pch = 16,col ="red", ylim=c(-2, 17), xlim=c(10000, 125000), xlab = "number of simulation (N)", ylab = "simulation time (in-of sample)", lty = 1, lwd =1)
lines(x, time_sim_in_G.GJR.Ret.VIX.Qua, pch = 18, col = "blue", type = "b", lty = 2, lwd = 1)
lines(x, time_sim_in_G.GJR.Ret.Ess, pch = 18, col = "green", type = "b", lty = 3, lwd = 1)
lines(x, time_sim_in_NIG.NGARCH.Ret.VIX.Ess, pch = 18, col = "deepskyblue", type = "b", lty = 3, lwd = 1)
legend(90000,3.5,bty = "n", legend = c("G.GJR.Ret.VIX.Ess", "G.GJR.Ret.VIX.Qua","G.GJR.Ret.Ess", "NIG.NGARCH.Ret.VIX.Ess"), col = c("red", "blue","green","deepskyblue"), lty = 1:4, cex = 0.8)


############################################################
####              Plot the time out                       ##
############################################################
time_sim_out_G.GJR.Ret.VIX.Ess <-          c(1.531122 ,1.804257 ,2.227241 ,2.866397 ,3.936067 ,7.524419  ,9.928634  ,8.141212 ,10.201256 ,12.086141)
time_sim_out_G.GJR.Ret.VIX.Qua <-          c(2.548941 ,4.395757 , 5.880141 ,6.205051 ,8.269049 ,9.547638 ,11.499478 ,10.958627 ,12.751420 ,15.052313 )
time_sim_out_G.GJR.Ret.Ess <-              c(2.492356 ,3.244655 ,3.338507 ,4.937348 ,5.045035 ,5.517742 ,6.602285 ,7.408210 ,9.026084 ,12.407307)
time_sim_out_NIG.NGARCH.Ret.VIX.Ess <-     c(2.669400 ,4.543659 ,5.746091,6.555268 ,7.256107 , 7.668065 ,9.082052 ,9.733186, 11.284891 ,12.884694 )


plot(N_liste, time_sim_out_G.GJR.Ret.VIX.Ess, type = "b", frame = FALSE, pch = 16,col ="red", ylim=c(-2, 17), xlim=c(10000, 125000), xlab = "number of simulation (N)", ylab = "simulation time (out-of sample)", lty = 1, lwd =1)
lines(x, time_sim_out_G.GJR.Ret.VIX.Qua, pch = 18, col = "blue", type = "b", lty = 2, lwd = 1)
lines(x, time_sim_out_G.GJR.Ret.Ess, pch = 18, col = "green", type = "b", lty = 3, lwd = 1)
lines(x, time_sim_out_NIG.NGARCH.Ret.VIX.Ess, pch = 18, col = "deepskyblue", type = "b", lty = 3, lwd = 1)
legend(90000,3.5,bty = "n", legend = c("G.GJR.Ret.VIX.Ess", "G.GJR.Ret.VIX.Qua","G.GJR.Ret.Ess", "NIG.NGARCH.Ret.VIX.Ess"), col = c("red", "blue","green","deepskyblue"), lty = 1:4, cex = 0.8)



############################################################
####          Plot the RMSE  in sample                    ##
############################################################
RMSE_sim_in_G.GJR.Ret.VIX.Ess <-  c(0.054170 ,0.052564 ,0.053006 ,0.054231 ,0.053211 ,0.052979 ,0.053020 ,0.053419 ,0.055755 ,0.052754)
RMSE_sim_in_G.GJR.Ret.VIX.Qua <-  c(0.052052 ,0.051620 ,0.051930 ,0.051100 ,0.051056 ,0.050938 ,0.051092 ,0.050759 ,0.050721 ,0.050379)
RMSE_sim_in_G.GJR.Ret.Ess <-  c(0.058019 ,0.057765 ,0.057701 ,0.057361 ,0.056347 ,0.056314 ,0.055243 ,0.056380 ,0.056230 ,0.054456)
RMSE_sim_in_NIG.NGARCH.Ret.VIX.Ess <- c(0.04586464,0.04545419, 0.04574357, 0.04540203,0.04511759,0.04537957, 0.04485929,0.04437037,0.04404326, 0.04392981)

plot(N_liste, RMSE_sim_in_G.GJR.Ret.VIX.Ess, type = "b", frame = FALSE, pch = 19,col ="red", ylim=c(0.04, 0.06),  xlim=c(10000, 125000), xlab = "number of simulation (N)", ylab =  "RMSE out of sample", lty = 1, lwd = 1)
lines(x, RMSE_sim_in_G.GJR.Ret.VIX.Qua, pch = 18, col = "blue", type = "b", lty = 2, lwd = 1)
lines(x, RMSE_sim_in_G.GJR.Ret.Ess, pch = 18, col = "green", type = "b", lty = 3, lwd = 1)
lines(x, RMSE_sim_in_NIG.NGARCH.Ret.VIX.Ess, pch = 18, col = "deepskyblue", type = "b", lty = 3, lwd = 1)

legend(90000,0.07, bty = "n", legend = c("G.GJR.Ret.VIX.Ess", "G.GJR.Ret.VIX.Qua","G.GJR.Ret.Ess", "NIG.NGARCH.Ret.VIX.Ess"), col = c("red", "blue","green","deepskyblue"), lty = 1:4, cex = 0.8)



############################################################
####          Plot the RMSE  out sample                   ##
############################################################
RMSE_sim_out_G.GJR.Ret.VIX.Ess <-  c(0.064175 ,0.064360 ,0.063859 ,0.064091 ,0.065244 ,0.063829 ,0.063876 ,0.066968 ,0.064327 ,0.063575)
RMSE_sim_out_G.GJR.Ret.VIX.Qua <-  c(0.063022 ,0.062970,0.062550 ,0.062384 ,0.062532 ,0.061562 ,0.061558 ,0.061198,0.061438 ,0.060063)
RMSE_sim_out_G.GJR.Ret.Ess <-      c(0.078984 ,0.077471 ,0.074521 ,0.076938 ,0.076743 ,0.075059 ,0.075858 ,0.074426 ,0.073578 ,0.073328)
RMSE_sim_out_NIG.NGARCH.Ret.VIX.Ess <- c(0.05911073,0.05907717,0.05887404,0.05908849,0.05780608,0.05788528,0.05778745,0.05783915,0.05803498 , 0.05781929) 

plot(N_liste,RMSE_sim_out_G.GJR.Ret.VIX.Ess, type = "b",frame = FALSE, pch = 19,col ="red", ylim=c(0.055, 0.085),xlim=c(10000, 125000), xlab = "number of simulation (N)", ylab = "RMSE out of sample", lty = 1, lwd = 1)
lines(x, RMSE_sim_out_G.GJR.Ret.VIX.Qua, pch = 18, col = "blue", type = "b", lty = 2, lwd = 1)
lines(x, RMSE_sim_out_G.GJR.Ret.Ess, pch = 18, col = "green", type = "b", lty = 3, lwd = 1)
lines(x, RMSE_sim_out_NIG.NGARCH.Ret.VIX.Ess, pch = 18, col = "deepskyblue", type = "b", lty = 3, lwd = 1)

legend(90000,0.084, bty = "n", legend = c("G.GJR.Ret.VIX.Ess", "G.GJR.Ret.VIX.Qua","G.GJR.Ret.Ess", "NIG.NGARCH.Ret.VIX.Ess"), col = c("red", "blue","green","deepskyblue"), lty = 1:4, cex = 0.8)


  
Y <- c(2^11, 2^12, 2^13, 2^14, 2^15,2^16)
############################################################
####       Plot the time in and out                       ##
############################################################
#time_sim_in_IG_GARCH_RET_VIX_Us <- c(3.927617 ,8.771856 ,33.06697212 ,51.29552 ,148.98882,988.047)
time_sim_in_IG_GARCH_RET_VIX_Us <- c(0.06546028,  0.14619760 , 0.55111620 , 0.85492533 , 2.48314700 ,16.46745000)
time_sim_out_IG_GARCH_RET_VIX_Us <- c(0.08682170 ,0.1730496 , 0.6530810 ,0.9351302 ,3.184715,17.39113)
plot(Y, time_sim_in_IG_GARCH_RET_VIX_Us, type = "b", frame = FALSE, pch = 16,col ="red", ylim=c(0, 19), xlim=c(2^10 - 15, 2^16 + 15), xlab = "number of simulation (N) inside the FFT pricing computation", ylab = "simulation time in-of sample by hours (h)", lty = 1, lwd =1)
lines(Y, time_sim_out_IG_GARCH_RET_VIX_Us, pch = 18, col = "deepskyblue", type = "b", lty = 3, lwd = 1)
legend(1000,15,bty = "n", legend = c("Time_in_sample_IG_GARCH_RET_VIX_Us","Time_out_sample_IG_GARCH_RET_VIX_Us"), col = c("red","deepskyblue"), lty = 1:4, cex = 0.8)


############################################################
####       Plot the RMSE  in and out of sample            ##
############################################################
RMSE_sim_in_IG_GARCH_RET_VIX_Us <- c(0.05406446,0.05240393, 0.0512378, 0.05087892, 0.05069795)
RMSE_sim_out_IG_GARCH_RET_VIX_Us <- c(0.06113377,0.05925612, 0.05793751, 0.05753171, 0.05732707)
plot(Y, RMSE_sim_in_IG_GARCH_RET_VIX_Us, type = "b", frame = FALSE, pch = 19,col ="red", ylim=c(0.05, 0.06),  xlim=c(2^10 - 15, 2^16 + 15), xlab = "number of simulation (N)", ylab =  "RMSE in of sample", lty = 1, lwd = 1)
lines(Y, RMSE_sim_out_IG_GARCH_RET_VIX_Us, pch = 18, col = "deepskyblue", type = "b", lty = 3, lwd = 1)
legend(1000,0.051,bty = "n", legend = c("RMSE_in_IG_GARCH_RET_VIX_Us","RMSE_out_IG_GARCH_RET_VIX_Us"), col =  c("red","deepskyblue"), lty = 1:4, cex = 0.8)



############################################################
####          Plot the RMSE  out sample                   ##
############################################################

plot(N_liste, RMSE_sim_in_IG_GARCH_RET_VIX_Us, type = "b", frame = FALSE, pch = 19,col ="red", ylim=c(0.05, 0.06),  xlim=c(2^10 - 15, 2^15 + 15), xlab = "number of simulation (N)", ylab =  "RMSE out of sample", lty = 1, lwd = 1)
legend(2^13,0.051,bty = "n", legend = c("RMSE_in_IG_GARCH_RET_VIX_Us","RMSE_out_IG_GARCH_RET_VIX_Us"), col = c("red","deepskyblue"), lty = 1:4, cex = 0.8)


## N_val = 2^11
## 3.927617 mins
## 0.06113377
## 0.05406446

## N_val = 2^12
## 8.771856 mins
## 0.05925612
## 0.05240393

## N_val = 2^13
## 8.771856 mins
## 0.05793751
## 0.0512378

## N_val = 2^14
## 51.29552 mins
## 0.05753171
## 0.05087892

## N_val = 2^15
## 2.483147 hours
## 0.05732707
## 0.05069795
  
## N_val = 2^16
## 16.46745 hours

  
time_sim_in_IG_GARCH_RET_VIX_Us <- c(3.927617 ,8.771856 ,33.06697212 ,51.29552 ,148.98882,988.047)
time_sim_in_IG_GARCH_RET_VIX_Us/60


  