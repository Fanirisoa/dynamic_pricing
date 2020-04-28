rm(list=ls())
gc()
library(zoo)

###########################################
### Code to transforme the data set #######
###########################################
setwd("C:/Users/Leacalin/Desktop/Simulation 2015 chorro/Dataset chorro") 

path_data=paste(getwd(),"/Dataset/",sep="")
dates_ini=as.Date("07/01/2009","%d/%m/%Y")
dates_fini=as.Date("18/04/2012","%d/%m/%Y")
dates_courantes=seq(dates_ini,dates_fini,7)
n_dates=length(dates_courantes)
dataset=list() # Liste des donnèes
index_list=1
for (i in 1:n_dates){
  nom=paste(path_data,dates_courantes[i],".csv",sep="")
  x=read.delim(nom,header=FALSE,sep=";")
  # Traitement des donnÈes
  strike=x[,3]
  Maturity=as.matrix(x[,4])
  SJ=x[,5]
  tsr=x[,6]/100
  prix=x[,7]  
  div=x[,8]
  ttm=x[,9] 
  
  #Récupèration des prix
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
# Export des donnèes au bon format pour le pricer 
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

Mat <- list()
T_Mat <- list()
for (i in 1:172){
  Mat[[i]] <- dataset[[i]]$Maturity
  T_Mat[[i]] <- as.Date(Mat[[i]])-as.Date(dates_courantes[i])
}
T_Mat                       # Diference between T-t

price_C <- data$prix     #### Prix du call: data$prix .... Call_market price
Strike_k <-data$strike   #### Prix d'exercice: data$strike
T<-data$ttm      #### Time to maturity: data$ttm
S<- data$SJ       #### Prix du sous-jacent: data$SJ
r<- data$tsr      #### Taux d'interet sans risque: data$tsr
div_d<-data$div      #### dividende: data$div


###############################################
###  Transforming the data to data frame ######
###############################################
Data.function <- function(s) {data.frame(C=data$prix[[s]],K=data$strike[[s]],S =data$SJ[[s]], T =data$ttm[[s]], r=data$tsr[[s]], d=data$div[[s]]) }
Serie.data <- setNames(lapply(1:2, function(s) Data.function(s)),paste0("d_", 1:2))  

Data.contract <- data.frame()
for(i in 1:length(Serie.data)) {
  Data.contract <- rbind(Data.contract, cbind(Serie.data[[i]]))
}

Data.contract


##########################################
### Code to transforme the base_SJ #######
##########################################

setwd("C:/Users/Leacalin/Desktop/Simulation 2015 chorro/Dataset chorro/Dataset") 

x=read.table("Base_SJ.csv",header=FALSE,sep=";")

date=x[,1]
prix_SJ=x[,2]               #### Prix du SJ:
tsr=x[,3]/100               #### Taux d'interet sans risque:
VIX_t=x[,4]                 #### VIX
ret <- diff(log(prix_SJ))   #### returns

prix_SJ=prix_SJ[-1]
tsr=tsr[-1]      
VIX_t=VIX_t[-1]  


Data.ret<-data.frame(St=prix_SJ,rt=tsr,VIX =VIX_t,ret=ret)
Data.ret

####################################################
###### Constant of the model                      ##
####################################################
h1<- sd(Data.ret$ret)       # The first value for h 
h1_star<- sd(Data.ret$ret)  # The first value for h_star 


####################################################
######  Step 1 : The volatility updating rule     ##
####################################################
###### A- Under the historical probability        ##
####################################################

h<-function(para_h,Data.ret){
  ret<-Data.ret$ret   #  returns, 
  r<-Data.ret$rt      #  Taux d'interet sans risque
  
  h = c()     #  A vector containing h from the model,
  h[1]=h1     #  The first value for h,
  
  #para_h<-c() set up the parameters of the model 
  w=para_h[1]
  b=para_h[2]
  c=para_h[3]
  neta= para_h[4]
  nu=para_h[5]
  a=para_h[6]
  
  for (i in 2:length(ret)){
    h[i]=w+ (b*h[i-1])+ ((c/neta)*(ret[i-1] - r[i-1] - (nu*h[i-1])))+((a*neta)*(h[i-1])^2)/(ret[i-1] - r[i-1]- (nu*h[i-1]))
  }
  
  return(h)
}


####################################################
###### B- Under the risk neutral probability      ##
####################################################

h_star<-function(para_h,Data.ret){
  ret<-Data.ret$ret   #  returns, 
  r<-Data.ret$rt      #  Taux d'interet sans risque
  
  
  h_star = c()      #  A vector containing h_star from the model 
  h_star[1]=h1_star # The first value for h_star 
  #para_h<-c() set up the parameters of the model 
  w=para_h[1]
  b=para_h[2]
  c=para_h[3]
  neta= para_h[4]
  nu=para_h[5]
  a=para_h[6]
  
  #para_h_star<-c()  set up the parameters of the model 
  Pi=para_h[7]
  neta_star=para_h[8]
  
  for (i in 2:length(ret)){
    h_star[i]=w*Pi+ (b*h_star[i-1])+ ((c*Pi/neta)*(ret[i-1] - r[i-1] - ((nu/Pi)*h_star[i-1])))+((a*neta/Pi)*(h_star[i-1])^2)/(ret[i-1] - r[i-1]- ((nu/Pi)*h_star[i-1]))
  }
  return(h_star)
}

para_h<-c(1,0.1,-0.5,0.4,-0.5,-0.5,-0.1,0.2)
h(para_h,Data.ret)
h_star(para_h,Data.ret)
