
###########################################
### Code to transforme the data set #######
###########################################
setwd("C:/Users/fanir/Desktop/Simulation_juin2018/Data/Dataset_chorro") 

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
  Maturity=c()
  
  index=1
  for (i in 1:n){
    strike[[index]]=dataset[[i]]$strike
    prix[[index]]=dataset[[i]]$prix
    ttm[[index]]=dataset[[i]]$ttm
    SJ[[index]]=dataset[[i]]$SJ
    tsr[[index]]=dataset[[i]]$tsr
    div[[index]]=dataset[[i]]$div
    Maturity[[index]]=dataset[[i]]$Maturity    
    index=index+1
  }
  return(list(prix=prix,strike=strike,ttm=ttm,SJ=SJ,tsr=tsr,div=div,Maturity=Maturity))
}
data=exportation(dataset)

price_C <- data$prix    #### Prix du call: data$prix .... Call_market price
Strike_k <-data$strike  #### Prix d'exercice: data$strike
T<-data$ttm             #### Time to maturity: data$ttm
S<- data$SJ             #### Prix du sous-jacent: data$SJ
r<- data$tsr            #### Taux d'interet sans risque: data$tsr
div_d<-data$div         #### dividende: data$div
Maturity=data$Maturity
###############################################
###  Transforming the data to data frame ######
###############################################
Pe=c(0,5,4,5,5,5,4,5,5,5,5,5,5,5,3,5,5,4,5,5,4,5,5,5,5,5,4,5,5,5,5,5,5,5,4,4,5,5,5,5,5,5,5,5,5,5,5,4,5,5,5,3) #2009

d2=c()
d2[1]=250
for(i in 2:length(Pe)) {
  d2[i]<- d2[i-1]+Pe[i]
}

d2lol=c()
d2lol[length(Pe)]=0
for(i in 1:length(Pe)-1) {
  d2lol[i]<- d2[i+1]
}
Data.function <- function(s) {data.frame(C=data$prix[[s]],K=data$strike[[s]],S =data$SJ[[s]], T =data$ttm[[s]], r=data$tsr[[s]], d=data$div[[s]],Pe=Pe[s],Pelol=d2lol[s],Per=d2[s],Mat=data$Maturity[[s]])}
Serie.data <- setNames(lapply(1:52, function(s) Data.function(s)),paste0("d_", 1:52))  

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
K=Data.contract$K
Pelol=Data.contract$Pelol
Per=Data.contract$Per
Data.contract$Mat<- as.Date(Data.contract$Mat)


NexP=rep(0, length(T)) 
for(i in 1:length(T)) {
  B1=subset(subset(Data.contract, K == Data.contract$K[i]), Per==Data.contract$Pelol[i])
  C1=B1[ which(B1$Mat==Data.contract$Mat[i]),]
  NexP[i]=C1$C[1]
}

NexP[is.na(NexP)] <- 0

Data.contract<-data.frame(C=Data.contract$C,K=Data.contract$K,S=Data.contract$S,T=Data.contract$T,r=Data.contract$r,d=Data.contract$d,Pe=Data.contract$Pe,Pelol=Data.contract$Pelol,Per=Data.contract$Per,Mod,Mat=Data.contract$Mat,NexP)




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

Data.new<-data.frame(C=Data.contract$C,K=Data.contract$K,S=Data.contract$S,T=Data.contract$T,r=Data.contract$r,d=Data.contract$d,Pe=Data.contract$Pe,Pelol=Data.contract$Pelol,Per=Data.contract$Per,Mod,CsK,Mat=Data.contract$Mat,NexP)

Data.modif=Data.new[!Data.new$CsK <= 0,]
Data.N1=Data.modif[!Data.modif$C <= 3.5/8,]
Data.N=Data.N1[-c(198,489,521,726,805,832,983,1135,1182,1187),]

##########################################
### Code to extracte the base_SJ   #######
##########################################
setwd("C:/Users/fanir/Desktop/Simulation_juin2018/Data/Dataset_chorro/Dataset") 


x=read.table("Base_SJ.csv",header=FALSE,sep=";")

date=x[,1]
prix_SJ=x[,2]               #### Prix du SJ:
tsr=x[,3]/100               #### Taux d'interet sans risque:
VIX_t=x[,4]                 #### VIX
ret <- diff(log(prix_SJ))   #### returns
vixret <- diff(log(VIX_t))   #### VIX returns



date=date[-1]
prix_SJ=prix_SJ[-1]
tsr=tsr[-1]      
VIX_t=VIX_t[-1]  

Data.BSJ<-data.frame(date=date,St=prix_SJ,rt=tsr,VIX =VIX_t,ret=ret,vixret=vixret)

##########################################
###    Data returns for option     #######
##########################################
##Data.transf <- function(s) {data.frame(date=Data.BSJ$date[[s]],St=Data.BSJ$St[[s]],rt=Data.BSJ$rt[[s]],VIX=Data.BSJ$VIX[[s]], ret =Data.BSJ$ret[[s]], vixret =Data.BSJ$vixret[[s]] ) }
##Serie.dataBSJ <- setNames(lapply(4391:4884, function(s) Data.transf(s)),paste0("d_", 4391:4884))  

Data.transf <- function(s) {data.frame(date=Data.BSJ$date[[s]],St=Data.BSJ$St[[s]],rt=Data.BSJ$rt[[s]],VIX=Data.BSJ$VIX[[s]], ret =Data.BSJ$ret[[s]], vixret =Data.BSJ$vixret[[s]] ) }
Serie.dataBSJ <- setNames(lapply(4638:5132, function(s) Data.transf(s)),paste0("d_", 4638:5132)) 

## 4391:4884  dans le cas 2009 de 07/01/2008 a 31/12/2009
## 4638:5132  dans le cas 2010 de 31/12/2008 a 31/12/2010

Data.ret <- data.frame()
for(i in 1:length(Serie.dataBSJ)) {
  Data.ret <- rbind(Data.ret, cbind(Serie.dataBSJ[[i]]))
}

##########################################
###  Data returns for returs and VIX   ###
##########################################
##Serie.dataBSJ.ret <- setNames(lapply(2172:4884, function(s) Data.transf(s)),paste0("d_", 2172:4884))  

Serie.dataBSJ.ret <- setNames(lapply(2415:5132, function(s) Data.transf(s)),paste0("d_", 2415:5132))  

## 2172:4884  dans le cas 2009 de 1999-01-07 a 31/12/2009
## 2415:5132  dans le cas 2010 de 2000-01-04 a 31/12/2010

Data.returns <- data.frame()
for(i in 1:length(Serie.dataBSJ.ret)) {
  Data.returns <- rbind(Data.returns, cbind(Serie.dataBSJ.ret[[i]]))
}
#####################################################
###    Save both data.N and data.ret          #######
#####################################################
save(Data.ret,Data.N,Data.returns, file="DataPrice2009test.Rdata")


