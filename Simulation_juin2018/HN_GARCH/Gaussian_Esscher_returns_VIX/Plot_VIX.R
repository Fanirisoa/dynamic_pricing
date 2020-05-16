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

plot(index(ts.vol_2),ts.vol_2 , type="l" ,col = "blue", main = "Gaussian-HN-GARCH-Ret-Vix",xaxt="n",xlab="Times",ylab="VIX",ylim=c(15,60))
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
  a0=para_h[1]; a1=para_h[2]; gama=para_h[3];  b1= para_h[4] ;  lamda0= para_h[5]
  
  g1=b1 + a1*(gama)^2  
  
  # Parameter under the physical probability
  lamda0star= -(1/2)
  gamastar= gama+lamda0+(1/2)
  
  
  h0=(a0 + a1)/(1 - b1 - a1*(gamastar)^2)    
  g0=b1 + a1*(gamastar)^2  
  
  return(list(PersiHisto=g1, PersiNeutral=g0)) 
}

Persistence(para_h1)
Persistence(para_h)