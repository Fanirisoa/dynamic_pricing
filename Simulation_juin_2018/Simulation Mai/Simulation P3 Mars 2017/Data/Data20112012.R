###########################################
### Code to transforme the data set #######
###########################################
setwd("C:/Users/e0g411k03dt/Desktop/New Simulation Juillet/Dataset chorro") 
##setwd("C:/Users/Leacalin/Desktop/New Simulation June/CalibrationOption2009/Dataset chorro") 


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
Pe=c(0,5,4,5,5,5,5,4,5,5,5,5,5,5,5,5,3,3,5,5,5,4,5,5,5,5,4,5,5,5,5,5,5,5,4,4,5,5,5,5,5,5,5,5,5,5,5,4,5,5,5,3,4,5,4,5,5,5,5,4,5,5,5,5,5,5,3,5) #2011-2012

d2=c()
d2[1]=0 #250
for(i in 2:length(Pe)) {
  d2[i]<- d2[i-1]+Pe[i]
}

Data.function <- function(s) {data.frame(C=data$prix[[s]],K=data$strike[[s]],S =data$SJ[[s]], T =data$ttm[[s]], r=data$tsr[[s]], d=data$div[[s]],Pe=Pe[s-104],Per=d2[s-104]) }
Serie.data <- setNames(lapply(105:172 , function(s) Data.function(s)),paste0("d_", 105:172))  


###  53:104 2010
###  1:52  2009
###  1:104 2009-2010
###  105:172 2011-2012
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


Data.modif=Data.new[!Data.new$CsK <= 0,]
Data.N1=Data.modif[!Data.modif$C <= 3.5/8,]

BAD=c(37,66,90,95,96,121,127,150,186,215,220,249,278,313,336,377,386,402,417,469,474,505,527,555,580,590,606,704,756,806,831,838,921,948,978,1257,1277,1307,1473,1533,1561,1567,1620,1625,1652,1729,1755,1788,1842,1874)


Data.N2=Data.N1[-BAD,]   #2011-2012



##########################################
### Code to extracte the base_SJ   #######
##########################################
setwd("C:/Users/e0g411k03dt/Desktop/New Simulation Juillet/Dataset chorro/Dataset") 
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
Serie.dataBSJ <- setNames(lapply(5132:5591, function(s) Data.transf(s)),paste0("d_", 5132:5591))  

## 4391:4884  dans le cas 2009 de 07/01/2008 a 31/12/2009
## 4638:5132  dans le cas 2010 de 31/12/2008 a 31/12/2010
## 4391:5132  dans le cas 2009-2010 de 07/01/2008 a 31/12/2010
## 4391:5132  dans le cas 2009-2010 de 07/01/2008 a 31/12/2010
## 5132:5591  dans le cas 2011-2012 de 04/01/2011 a 31/12/2010


Data.ret <- data.frame()
for(i in 1:length(Serie.dataBSJ)) {
  Data.ret <- rbind(Data.ret, cbind(Serie.dataBSJ[[i]]))
}

##########################################
###  Data returns for returs and VIX   ###
##########################################
Serie.dataBSJ.ret <- setNames(lapply(2415:5591, function(s) Data.transf(s)),paste0("d_", 2415:5591))  

## 2172:4884  dans le cas 2009 de 1999-01-07 a 31/12/2009
## 2415:5132  dans le cas 2010 de 2000-01-04 a 31/12/2010
## 2172:5132  dans le cas 2009-2010 de 1999-01-07  a 31/12/2010

## 2415:5591  dans le cas 2009-2010 de 2000-01-04 a 31/12/2012

Data.returns <- data.frame()
for(i in 1:length(Serie.dataBSJ.ret)) {
  Data.returns <- rbind(Data.returns, cbind(Serie.dataBSJ.ret[[i]]))
}
#####################################################
###    Save both data.N and data.ret          #######
#####################################################
##(Data.ret,Data.N1,Data.N2,Data.N3,Data.N4,Data.N5,Data.returns, file="DataPrice2009.Rdata")

save(Data.ret,Data.N1,Data.N2,Data.returns, file="DataPrice20112012.Rdata")




