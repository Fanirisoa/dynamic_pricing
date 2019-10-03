###########################################
### Code to transforme the Base_SJ #######
###########################################
setwd("C:/Users/Leacalin/Desktop/Simulation 2015 chorro/Dataset chorro/Dataset") 

x=read.table("Base_SJ.csv",header=FALSE,sep=";")

date=x[,1]
prix_SJ=x[,2]      #### Prix du SJ:
tsr=x[,3]/100      #### Taux d'interet sans risque:
VIX_t=x[,4]        #### VIX
ret <- diff(log(prix_SJ))

length(ret)
