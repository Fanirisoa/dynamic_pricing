weRMSE <- function(para_h){ 
error <- rep(0)

###############################################
###  fonction to generate a data contract   ###
###############################################

Data.week.contract <- function(l) {
  
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

w=105+l-1  
  
Data.function <- function(s) {data.frame(C=data$prix[[s]],K=data$strike[[s]],S =data$SJ[[s]], T =data$ttm[[s]], r=data$tsr[[s]], d=data$div[[s]],Pe=Pe[s-104],Per=d2[s-104]) }
Serie.data <- setNames(lapply(w:w , function(s) Data.function(s)),paste0("d_", w:w))    


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


Data.N2=Data.N1[-BAD,]


  return(list(Data.N1=Data.N1,Data.N2=Data.N2)) 

}

Num_sem = 60
###############################################
###  fonction to generate a data returns    ###
###############################################


Data.week.returns <- function(l) {
  
  w = l*7
  setwd("C:/Users/fanir/Desktop/Simulation_juin2018/Data/Dataset_chorro/Dataset") 
  
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
  l1=5132+w
  Serie.dataBSJ <- setNames(lapply(5132:l1 , function(s) Data.transf(s)),paste0("d_", 5132:l1))  
  
  ## 5132:5591  dans le cas 2011-2012 de 04/01/2011 a 31/12/2012
  
  
  Data.ret <- data.frame()
  for(i in 1:length(Serie.dataBSJ)) {
    Data.ret <- rbind(Data.ret, cbind(Serie.dataBSJ[[i]]))
  }
  
  ##########################################
  ###  Data returns for returs and VIX   ###
  ##########################################
  l3=5132+w
  Serie.dataBSJ.ret <- setNames(lapply(2415:l3, function(s) Data.transf(s)),paste0("d_", 2415:l3))  
  
  ## 2172:4884  dans le cas 2009 de 1999-01-07 a 31/12/2009
  ## 2415:5132  dans le cas 2010 de 2000-01-04 a 31/12/2010
  ## 2172:5132  dans le cas 2009-2010 de 1999-01-07  a 31/12/2010
  
  ## 2415:5591  dans le cas 2009-2010 de 2000-01-04 a 31/12/2012
  
  Data.returns <- data.frame()
  for(i in 1:length(Serie.dataBSJ.ret)) {
    Data.returns <- rbind(Data.returns, cbind(Serie.dataBSJ.ret[[i]]))
  }
  
  return(list(Data.ret=Data.ret,Data.returns=Data.returns)) 
  
}

 




for(i in 1:Num_sem) {
A=Data.week.returns(i)
B=Data.week.contract(i)
Data.returns= A$Data.returns
  
Sol=optim(para_h, Heston_likelihood_ret, Data.returns=Data.returns, method="Nelder-Mead",control = list(maxit = 5000))

para_h1<-Sol$par

Data.N = B$Data.N2
Data.ret = A$Data.ret
RMSEwe=RMSE(para_h1,Data.ret,Data.N)
x = RMSEwe$error
error = append(error, x)
para_h = para_h1

}

rmse<-sqrt((mean(error)))

return(list(rmse=rmse,error=error)) 
}







