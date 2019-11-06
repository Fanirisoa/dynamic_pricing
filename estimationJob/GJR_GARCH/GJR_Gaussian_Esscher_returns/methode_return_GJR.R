###################################################################################
###               Clean the repertoir and laod all the library used         #######
###################################################################################
rm(list=ls())
gc()
library(compiler)
enableJIT(1)
enableJIT(3)
library("fBasics")
#library("pracma")
library("numDeriv")
library("nlme")
library("Matrix")
library(xts)

##################################################################################################
###              Load : Data source,    Parameters of the model,  function to use          #######
##################################################################################################
setwd("/Users/leafanirisoa/Documents/projetGit/dynamic_pricing/data_used")  
path = "/Users/leafanirisoa/Documents/projetGit/dynamic_pricing/estimationJob/GJR_GARCH/GJR_Gaussian_Esscher_returns"

source(paste(path,"/parameters_settting.R",sep=""))
source(paste(path,"/Loglik_Return_GJR.R",sep=""))
source(paste(path,"/Function_Pricer_VIX_GJR.r",sep=""))
source(paste(path,"/Simulation_MC.r",sep=""))
source(paste(path,"/RMSE_VIX_GJR.r",sep=""))


######################################################################################
###               Volatility   plot under the initial parameters               #######
######################################################################################
ts.vol_P= shape_vol_P (para_h, Data.returns) 
ts.plot(ts.vol_P , col = "steelblue", main = "GJR Garch Model",xlab="Time",ylab="Volatility")
grid()

ts.vol_Q= shape_vol_Q (para_h, Data.returns) 
ts.plot(ts.vol_Q, col = "steelblue", main = "GJR Garch Model",xlab="Time",ylab="Volatility")
grid()

ts.plot(cbind(ts.vol_P, ts.vol_Q),  gpars = list(col = c("black", "red")), main = "GJR Garch Model",xlab="Time",ylab="Volatility")
grid()

#####################################################
###              Log values returns           #######
#####################################################
start.time <- Sys.time()
ILK=GJR_likelihood_ret(para_h, Data.returns)
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken

ILK

#####################################################
###      Optimization  of the model           #######
#####################################################
start.time <- Sys.time()
Sol=optim(para_h, GJR_likelihood_ret , Data.returns=Data.returns, method="Nelder-Mead",control = list(maxit = 5000))
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken
Sol
para_h1<-Sol$par


####################################################
######         RMSE compare by values of N       ##
####################################################
RMSE_by_N <- function(set_N,para_h, Data.returns) {
  Ni=length(set_N)      ####  length of the vector containing N,

  RMSE <-rep(NA, Ni)
  RMSE_simple <-rep(NA, Ni)
  RMSE_norm <- rep(NA, Ni)
  time_Computation <- rep(NA, Ni)
  N <- rep(NA, Ni)
  
  
  print(paste0("level: ", 1))
  
  for (i in 1:length(set_N)){
    N[i] = set_N[i]
    
    print(paste0("level: ", 2))
    start.time <- Sys.time()
    RMSE=RMSEsim(para_h1,Data.N,N[i])
    end.time <- Sys.time()
    
    print(paste0("level: ", 9))
  
    time.taken <- end.time - start.time
    time_Computation[i]=time.taken  
    RMSE_simple[i] = RMSE$rmse
    RMSE_norm[i] = RMSE$norm_rmse
    
  }

  return(list(RMSE_simple=RMSE_simple,RMSE_norm = RMSE_norm,time_Computation = time_Computation, set_N = set_N)) 

}

N_1<-c(1500, 2500, 3500, 4500, 5500)
start.time <- Sys.time()
RMSE_N_1=RMSE_by_N(N_1,para_h1,Data.N)
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken

RMSE_N_1$RMSE_simple
RMSE_N_1$RMSE_norm
RMSE_N_1$time_Computation



N_2<-c(6500, 7500, 8500, 9500)  

start.time <- Sys.time()
RMSE_N_2=RMSE_by_N(N_2,para_h1,Data.N)
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken

RMSE_N_2$RMSE_simple
RMSE_N_2$RMSE_norm
RMSE_N_2$time_Computation




N_3<-c(12500)  

start.time <- Sys.time()
RMSE_N_3=RMSE_by_N(N_3,para_h1,Data.N)
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken

RMSE_N_3$RMSE_simple
RMSE_N_3$RMSE_norm
RMSE_N_3$time_Computation



sales = 7237782.949999999
galion= 6.353094601999997E7
filiales = 4.095520544000003E7
jdbc = 22499.43999999997
escale= 1325268

T = sales + galion + filiales + jdbc  + escale
T

X_sales = sales * 100 / T
X_galion = galion * 100 / T
X_filial = filiales * 100 / T
X_jdbc = jdbc * 100 / T
X_escale = escale * 100 / T

X_sales
X_galion
X_filial
X_jdbc
X_escale


N_liste <-    c(15000, 25000, 35000, 45000, 55000,  65000,75000,85000,100000,125000)
RMSE_liste_1 <- c(0.09517586, 0.09336052, 0.09385983,0.09524498, 0.0940919,0.09382993,0.09387618,0.09432774,0.09696839,0.09357514)
RMSE_liste_2 <-  c(0.08417003, 0.0825646, 0.08300617,0.08423116, 0.08321141,0.08297974,0.08302064, 0.08341998,0.08575528,0.08275441)
time_liste <- c(1.401163, 3.152843, 2.460967, 3.421017, 4.409233,13.59997 ,9.391688 ,11.84392,8.219551,12.69681)




dat <- matrix(runif(40,1,20),ncol=4) # make data
matplot(dat, type = c("b"),pch=1,col = 1:4) #plot
legend("topleft", legend = 1:4, col=1:4, pch=1) # optional legend


dat_2 <- cbind(N_liste, RMSE_liste, RMSE_norm,time_liste)
matplot(dat_2, type = c("b"),pch=1,col = 1:4) #plot
legend("topleft", legend = 1:4, col=1:4, pch=1) # optional legend


# 1. Create some variables
x <- c(15000, 25000, 35000, 45000, 55000,  65000,75000,85000,100000,125000)
y2 <- RMSE_liste_1
y1 <- time_liste 

# 2. Plot a first line
plot(x, y1, type = "b", frame = FALSE, pch = 19,col = "red", xlab = "x", ylab = "y", lty = 1, lwd = 1)

# 3. Add a second line
lines(x, y2, pch = 18, col = "blue", type = "b", lty = 2, lwd = 1)

# 4. Add a legend to the plot and set legend lty
legend("topleft", legend = c("Line 1", "Line 2"),
       col = c("red", "blue"), lty = 1:2, cex = 0.8)



N_liste <-    c(15000, 25000, 35000, 45000, 55000,  65000,75000,85000,100000,125000)

############################################################
####              Plot the time                           ##
############################################################
time_sim_in <- c(1.401163, 3.152843, 2.460967, 3.421017, 4.409233,13.59997 ,9.391688 ,11.84392,8.219551,12.69681)
time_sim_out <- c(1.531122, 1.804257, 2.227241, 2.866397, 3.936067,7.524419 ,9.928634 ,8.141212,10.201256,12.086141)
plot(N_liste, time_sim_in, type = "b", frame = FALSE, pch = 19,col ="red",  xlim=c(10000, 125000), xlab = "number of simulation (N)", ylab =  "simulation time (in h)", lty = 1, lwd = 1)
lines(x, time_sim_out, pch = 18, col = "blue", type = "b", lty = 2, lwd = 1)
legend(90000,3.5, legend = c("time_sim_in", "time_sim_out"), col = c("red", "blue"), lty = 1:2, cex = 0.8)

############################################################
####               Plot the RMSE                          ##
############################################################
RMSE_sim_in <-  c(0.05417003, 0.0525646, 0.05300617,0.05423116, 0.05321141,0.05297974,0.05302064, 0.05341998,0.05575528,0.05275441)
RMSE_sim_ou <-  c(0.06417586, 0.06436052,0.06385983, 0.0640919,0.06524498,0.06382993,0.06387618,0.06696839,0.06432774,0.06357514)

plot(N_liste, RMSE_sim_in, type = "b", frame = FALSE, pch = 19,col ="red", ylim=c(0.05, 0.085),  xlim=c(10000, 125000), xlab = "number of simulation (N)", ylab =  "RMSE", lty = 1, lwd = 1)
lines(x, RMSE_sim_ou, pch = 18, col = "blue", type = "b", lty = 2, lwd = 1)
legend(20000,0.075, legend = c("RMSE_in", "RMS_out"), col = c("red", "blue"), lty = 1:2, cex = 0.8)


data.frame(x = runif(n = 12, min = min(time_liste), max = max(time_liste)),
           y = runif(n = 12, min = min(time_liste), max = max(time_liste)))


############################################################
####                        RMSE                          ##
############################################################


para_h <-para_h1 
start.time <- Sys.time()
RMSE1=RMSEsim(para_h1,Data.N)
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken

RMSE1$rmse
RMSE1$norm_rmse




#####################################################
###    Load both data.contract and data.ret   #######
#####################################################
load("DataPrice20112012.Rdata")

Data.N=Data.N2
############################################################
####        RMSE  out of sample 2011-2012                 ##
############################################################
start.time <- Sys.time()
RMSE2=RMSEsim(para_h1,Data.N2)
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken

RMSE2$rmse
RMSE2$norm_rmse


############################################################
####                       Compare VIX                    ##
############################################################
source("C:/Users/fanir/Desktop/Simulation_juin2018/GJR_GARCH/GJR_Gaussian_Esscher_returns/Comparing VIX_GJR_Garch.R")


start.time <- Sys.time()
C_VIX= Compa_vix(para_h1,Data.returns)
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken



############################################################
####                Simulation Price                      ##
############################################################    
start.time <- Sys.time()
P_T_SimMC1=Pricer(N,para_h1,Data.N)
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken

P_T_SimMC1$P
P_T_SimMC1$Yt
P_T_SimMC1$St
P_T_SimMC1$St_Mar


start.time <- Sys.time()
P_T_SimMC1=Pricer(N,para_h1,Data.N2)
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken

P_T_SimMC1$P


############################################################
####                        RMSE                          ##
############################################################

start.time <- Sys.time()
RMSE2=RMSEsim (para_h1,Data.N)
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken

RMSE2$rmse
RMSE2$P



##########################################################
####                  DATA change                         ##
############################################################

start.time <- Sys.time()
RMSE1=RMSE(para_h,Data.ret,Data.N)
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken

RMSE1$rmse
A=RMSE1$error 

max(A)

D1=c()    
for(i in 1:length(A)) {
  D1[i] = A[i]
}


Data.N<-data.frame(C=Data.N$C,K=Data.N$K,S=Data.N$S,T=Data.N$T,r=Data.N$r,d=Data.N$d,Pe=Data.N$Pe,Per=Data.N$Per,Mod=Data.N$Mod,CsK=Data.N$CsK,D1)


Data.modif=Data.N[!Data.N$D1 <= 0.02,]
Data.modif
Data.N=Data.N[-c(506,1462,1638,1645),]
