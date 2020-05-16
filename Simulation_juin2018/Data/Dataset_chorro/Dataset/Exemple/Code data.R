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
  index=1
  for (i in 1:n){
    strike[[index]]=dataset[[i]]$strike
    prix[[index]]=dataset[[i]]$prix
    ttm[[index]]=dataset[[i]]$ttm
    SJ[[index]]=dataset[[i]]$SJ[1]
    tsr[[index]]=dataset[[i]]$tsr[1]
    div[[index]]=dataset[[i]]$div[1]
    index=index+1
  }
  return(list(prix=prix,strike=strike,ttm=ttm,SJ=SJ,tsr=tsr,div=div))
}
data=exportation(dataset)


price_C <- data$prix     #### Prix du call: data$prix .... Call_market price
Strike_k <-data$strike   #### Prix d'exercice: data$strike
T<-data$ttm      #### Time to maturity: data$ttm
S<- data$SJ       #### Prix du sous-jacent: data$SJ
r<- data$tsr      #### Taux d'interet sans risque: data$tsr
div_d<-data$div      #### dividende: data$div

