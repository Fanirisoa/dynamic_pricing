###########################################
### Code to transforme the data set #######
###########################################
setwd("C:/Users/e0g411k03dt/Desktop/New Simulation Juillet/Dataset chorro") 

##setwd("H:/Sim_nov_2017_paper_2/Dataset chorro") 


path_data=paste(getwd(),"/Dataset/",sep="")
dates_ini=as.Date("07/01/2009","%d/%m/%Y")
dates_fini=as.Date("18/04/2012","%d/%m/%Y")
dates_courantes=seq(dates_ini,dates_fini,7)
n_dates=length(dates_courantes)
dataset=list() # Liste des donn?es
index_list=1
for (i in 1:n_dates){
  nom=paste(path_data,dates_courantes[i],".csv",sep="")
  x=read.delim(nom,header=FALSE,sep=";")
  # Traitement des donn?es
  strike=x[,3]
  Maturity=as.matrix(x[,4])
  SJ=x[,5]
  tsr=x[,6]/100
  prix=x[,7]  
  div=x[,8]
  ttm=x[,9] 
  
  #R?cup?ration des prix
  dataset[[index_list]]=""
  dataset[[index_list]]$strike=strike          
  dataset[[index_list]]$Maturity=Maturity
  dataset[[index_list]]$SJ=SJ
  dataset[[index_list]]$tsr=tsr
  dataset[[index_list]]$prix=prix
  dataset[[index_list]]$div=div
  dataset[[index_list]]$ttm=ttm
  index_list=index_list+1
} 
# Export des donn?es au bon format pour le pricer 
exportation<-function(dataset){
  n=length(dataset)
  strike=list()
  prix=list()
  ttm=c()
  SJ=c()
  tsr=c()
  div=c()
  index=1
  for (i in 1:n){
    strike[[index]]=dataset[[i]]$strike
    prix[[index]]=dataset[[i]]$prix
    ttm[[index]]=dataset[[i]]$ttm
    SJ[[index]]=dataset[[i]]$SJ
    tsr[[index]]=dataset[[i]]$tsr
    div[[index]]=dataset[[i]]$div
    index=index+1
  }
  return(list(prix=prix,strike=strike,ttm=ttm,SJ=SJ,tsr=tsr,div=div))
}
data=exportation(dataset)

price_C <- data$prix    #### Prix du call: data$prix .... Call_market price
Strike_k <-data$strike  #### Prix d'exercice: data$strike
T<-data$ttm             #### Time to maturity: data$ttm
S<- data$SJ             #### Prix du sous-jacent: data$SJ
r<- data$tsr            #### Taux d'interet sans risque: data$tsr
div_d<-data$div         #### dividende: data$div

###############################################
###  Transforming the data to data frame ######
###############################################
Pe=c(0,5,4,5,5,5,4,5,5,5,5,5,5,5,3,5,5,4,5,5,4,5,5,5,5,5,4,5,5,5,5,5,5,5,4,4,5,5,5,5,5,5,5,5,5,5,5,4,5,5,5,3,4,5,4,5,5,5,4,5,5,5,5,5,5,3,5,5,5,4,5,5,5,4,5,5,5,5,4,5,5,5,5,5,5,5,4,4,5,5,5,5,5,5,5,5,5,5,5,4,5,5,5,2) #2010


d2=c()
d2[1]=250
for(i in 2:length(Pe)) {
  d2[i]<- d2[i-1]+Pe[i]
}
Data.function <- function(s) {data.frame(C=data$prix[[s]],K=data$strike[[s]],S =data$SJ[[s]], T =data$ttm[[s]], r=data$tsr[[s]], d=data$div[[s]],Pe=Pe[s],Per=d2[s]) }
Serie.data <- setNames(lapply(1:104, function(s) Data.function(s)),paste0("d_", 1:104))  

###  53:104 2010
###  1:52  2009

Data.contract <- data.frame()
for(i in 1:length(Serie.data)) {
  Data.contract <- rbind(Data.contract, cbind(Serie.data[[i]]))
}


Mod=c()    ## Define the equivalent class
Mod[1]=1
T=Data.contract$T
for(i in 2:length(T)) {
  if(T[i]==T[i-1]){
    Mod[i] = Mod[i-1]
  }else{
    Mod[i] = Mod[i-1]+1
  }
}

Data.contract<-data.frame(C=Data.contract$C,K=Data.contract$K,S=Data.contract$S,T=Data.contract$T,r=Data.contract$r,d=Data.contract$d,Pe=Data.contract$Pe,Per=Data.contract$Per,Mod)

#####################################################
###    Filtred data                           #######
#####################################################

Data=Data.contract
nub=Data.contract$S
C=Data.contract$C
K=Data.contract$K
S=Data.contract$S

DSK=c()    
for(i in 1:length(nub)) {
  S=Data.contract$S
  K=Data.contract$K
  DSK[i] = S[i]-K[i]
}

CsK=c()    
for(i in 1:length(nub)) {
  CsK[i] = C[i]-DSK[i]
}

Data.new<-data.frame(C=Data.contract$C,K=Data.contract$K,S=Data.contract$S,T=Data.contract$T,r=Data.contract$r,d=Data.contract$d,Pe=Data.contract$Pe,Per=Data.contract$Per,Mod,CsK)

##########################################
###   Filtration of the contract   #######
##########################################

Data.modif=Data.new[!Data.new$CsK <= 0,]
Data.N1=Data.modif[!Data.modif$C <= 3.5/8,]

BAD=c(85,115,146,198,199,204,357,377,405,441,463,469,489,490,512,522,571,621,668,704,726,784,726,760,784,805,832,867,983,1056,1063,1080,1109,1116,1135,1161,1182,1186,1187,1307,1333,1340,1365,1399,1428,1455,1478,1508,1555,1564,1586,1594,1611,1618,1637,1669,1780,1787,1808,1809,1815,1841,1870,1877,1907,1914,2025,2066,2093,2100,2101,2119,2147,2182,2189,2209,2233,2234,2261,2266,2284,2288,2310,2357,2382,2413,2440,2448,2467,2474,2496,2520,2552,2578,2583,2602,2607,2633,2721,2785)

Data.N2=Data.N1[-BAD,]   #2009-2010

##Data.N2=Data.N1[-c(198,489,521,726,805,832,983,1135,1182,1187),]   #2009
##Data.N3=Data.N2[-c(85,115,146,198,203,356,376,404,440,462,488,519,568,618,665,701,756,780,861,1049,1056,1073,1102,1153,1177,1297),] #2009
##Data.N4=Data.N3[-c(1,123,176,450,457,478,818,880,904,1029,1250,1255,1279),]  #2010
##Data.N5=Data.N4[-c(7,32,66,92,144,150,178,221,230,252,260,277,284,285,304,336,359,366,406,473,479,505,511,535,542,572,579,690,731,758,765,766,784,846,853,896,923,928,946,950,972,1005,1044,1075,1102,1110,1129,1136,1158,1381,1445),]   #2010
##########################################
### Code to extracte the base_SJ   #######
##########################################
setwd("C:/Users/e0g411k03dt/Desktop/New Simulation Juillet/Dataset chorro/Dataset") 

##setwd("H:/Sim_nov_2017_paper_2/Dataset chorro/Dataset")  


##setwd("C:/Users/Leacalin/Desktop/New Simulation June/CalibrationOption2009/Dataset chorro/Dataset") 


x=read.table("Base_SJ.csv",header=FALSE,sep=";")

date=x[,1]
prix_SJ=x[,2]               #### Prix du SJ:
tsr=x[,3]/100               #### Taux d'interet sans risque:
VIX_t=x[,4]                 #### VIX
ret <- diff(log(prix_SJ))   #### returns

date=date[-1]
prix_SJ=prix_SJ[-1]
tsr=tsr[-1]      
VIX_t=VIX_t[-1]  

Data.BSJ<-data.frame(date=date,St=prix_SJ,rt=tsr,VIX =VIX_t,ret=ret)

##########################################
###    Data returns for option     #######
##########################################
Data.transf <- function(s) {data.frame(date=Data.BSJ$date[[s]],St=Data.BSJ$St[[s]],rt=Data.BSJ$rt[[s]],VIX=Data.BSJ$VIX[[s]], ret =Data.BSJ$ret[[s]] ) }
Serie.dataBSJ <- setNames(lapply(4391:5132, function(s) Data.transf(s)),paste0("d_", 4391:5132))  

## 4391:4884  dans le cas 2009 de 07/01/2008 a 31/12/2009
## 4638:5132  dans le cas 2010 de 31/12/2008 a 31/12/2010
## 4391:5132  dans le cas 2009-2010 de 07/01/2008 a 31/12/2010


Data.ret <- data.frame()
for(i in 1:length(Serie.dataBSJ)) {
  Data.ret <- rbind(Data.ret, cbind(Serie.dataBSJ[[i]]))
}

##########################################
###  Data returns for returs and VIX   ###
##########################################
Serie.dataBSJ.ret <- setNames(lapply(2415:5132, function(s) Data.transf(s)),paste0("d_", 2415:5132))  

## 2172:4884  dans le cas 2009 de 1999-01-07 a 31/12/2009
## 2415:5132  dans le cas 2010 de 2000-01-04 a 31/12/2010
## 2172:5132  dans le cas 2009-2010 de 1999-01-07  a 31/12/2010


Data.returns <- data.frame()
for(i in 1:length(Serie.dataBSJ.ret)) {
  Data.returns <- rbind(Data.returns, cbind(Serie.dataBSJ.ret[[i]]))
}
#####################################################
###    Save both data.N and data.ret          #######
#####################################################
####save(Data.ret,Data.N1,Data.N2,Data.N3,Data.N4,Data.N5,Data.returns, file="DataPrice20092010.Rdata")

save(Serie.data,Data.ret,Data.N1,Data.N2,Data.returns, file="DataPrice20092010.Rdata")










