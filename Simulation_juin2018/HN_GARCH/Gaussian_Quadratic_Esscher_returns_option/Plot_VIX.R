Data_vix = Data.returns

Data.returns = Data_vix 


A=Compa_vix(para_h,Data_vix)

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


ts.vol_1= VIX_Model
ts.vol_2= VIX_Market
ts.plot(ts.vol_2 , col = "steelblue", main = "IG-GARCH",xlab="2011-2012",ylab="VIX",ylim=c(15,60),xlim=c(0,470))
lines(ts.vol_1)
grid()


legend(200,60, c("VIX_Model", "VIX_Market"), col = c("black", "blue"), lty = 1)
