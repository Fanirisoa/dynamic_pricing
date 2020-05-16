#####################################################
###    Load both data.contract and data.ret   #######
#####################################################
load("DataPrice20112012.Rdata")

Data.N=Data.N2
#########################################
###             VIX plot          #######
#########################################

Data_vix = Data.returns

Data.returns = Data_vix 

A=Compa_vix(para_h1,Data_vix)

MPE=A$MPE
MAE=A$MAE
MAE2=A$MAE2
Vrmse=A$Vrmse

VIX_Market=A$VIX_Market
VIX_Model=A$VIX_Model

MPE
MAE
MAE2
Vrmse
library(xts)

Dates_I= seq(as.Date('2011-01-04'),as.Date('2012-12-31'), "day") 
Dates_II =  sample(seq(as.Date('2011-01-05'),as.Date('2012-12-30'), "day"),251)
Dates= Dates_I[! Dates_I %in% Dates_II]

ts.vol_1 <- xts(VIX_Model, order.by = Dates)
ts.vol_2 <- xts(VIX_Market, order.by = Dates)

plot(index(ts.vol_2),ts.vol_2 , type="l" ,col = "blue", main = "IG-GARCH-Ess-Ret-option",xaxt="n",xlab="Times",ylab="VIX",ylim=c(10,60))
months= seq(min(Dates), max(Dates), "5 month")
axis(1, months, format(months, "%b %Y"))
points (index(ts.vol_1), ts.vol_1, type="l", col="red")
grid()
legend("top",60, c("VIX_Model", "VIX_Market"), col = c("blue", "red"), lty = 1)

#########################################
###         Persistence           #######
#########################################
Persistence<-function(para_h){
  
  
  # para_h<-c() set up the parameters of the model 
  w=para_h[1]; b=para_h[2]; a=para_h[3];  c= para_h[4]; neta=para_h[5]; nu=para_h[6] 
  
  
  g1=(a*(neta^2) +b +(c*(neta^(-2)))) 
  
  
  theta0= (1/2)*((neta)^(-1) - (1/((nu^2)*(neta^3)))*(1 + ((nu^2)*(neta^3))/2)^2)
  neta0=neta/(1 - 2*neta*theta0)
  PI=(neta0/neta)^(3/2)
  w0=w*(neta0/neta)^(3/2); b0=b; a0=a*(neta0/neta)^(-5/2);  c0=c*(neta0/neta)^(5/2)
  
  # The volatility under the physical probability
  nu0=(1/(neta0^2))*(((1-2*neta0)^(1/2))-1)
  
 
  
  g0=  ( a0*(neta0^2) + b0+ (c0*(neta0^(-2)))) 
  
  return(list(PersiHisto=g1, PersiNeutral=g0)) 
}

Persistence(para_h1)
Persistence(para_h)
MPE
MAE
MAE2
Vrmse