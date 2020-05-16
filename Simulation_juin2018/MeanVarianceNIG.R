A=(1.2501,1.1550,1.2702,1.4630,1.4365 ,1.3589,1.4536,2.961)
B=(-0.0106,-0.1432,-0.0025,-0.0061,-0.0538,-0.0058,-0.0061,-9.441) 
C=(1.4728,1.0623,1.6204,1.4454,1.3920,1.5336,1.4538,1.5877) 
D=(2.7086,0.1327,1.9734,2.1602,11.6243,7.9908,2.0178,0.5341) 

##############################################################
######     Conditional variance with risk netral Proba      ##
##############################################################
meanNIG<-function(a,b,c,d){
  Mean = c()
  variance = c()
  Gamma = c()
  for (i in 1:8){
    Gamma[i] = sqrt((a[i])^2 - (b[i])^2)
    Mean[i]= d[i] + (c[i]*b[i])/Gamma[i]
    variance[i]= (c[i]*(a[i]^2))/(Gamma[i]^3)
  }
  return(list(Mean=Mean, variance=variance)) 
}


MV01<-function(a,b,c,d){

meanvar = meanNIG(a,b,c,d)

a1 = c()
b1 = c()
c1 = c()
d1 = c()

for (i in 1:8){
V=sqrt(meanvar$variance)
m = meanvar$Mean
a1[i]= V[i]*a[i]
b1[i]= V[i]*b[i]
c1[i]=c[i]/V[i]
d1[i]= (-m[i]/V[i])+(d[i]/V[i])
}
VarMean=meanNIG(a1,b1,c1,d1)
variance= VarMean$variance
Mean = VarMean$Mean
return(list(Mean=Mean, variance=variance)) 
}


MV01(A,B,C,D)





