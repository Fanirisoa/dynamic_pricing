  path_data=paste(getwd(),"/Data/",sep="")
  dates_courantes=read.delim(paste(path_data,devise,"_0107_call.csv",sep=""),header=TRUE,sep=";")
  dates_courantes=as.matrix(dates_courantes[,1])
  dates_courantes=as.Date(dates_courantes,"%m/%d/%Y")
  n_dates=length(dates_courantes)
  dataset=list() # Liste des donnÈes
  index_list=1
  for (annee in c("07","08")){
    for (mois in c("01","02","03","04","05","06","07","08","09","10","11","12")){
      for (type in c("call")){#,"put")){
        dates=paste(mois,annee,sep="")
        nom=paste(path_data,devise,"_",dates,"_",type,".csv",sep="")
        x=read.delim(nom,header=FALSE,sep=";")
        # Traitement des donnÈes
        dates_temp=as.matrix(x[,1])
        n=length(dates_temp)
        dates_temp=dates_temp[2:n]
        dates_temp=as.Date(dates_temp,"%m/%d/%Y")
        prix=x[2:n,2:(ncol(x)-2)]  # Les deux derniËre colonnes contiennent l'index et le TSR.
        strike=x[1,2:(ncol(x)-2)]
        SJ=x[2:n,(ncol(x)-1)]
        tsr=x[2:n,ncol(x)]
        mat_contrat=paste("28-",mois,"-",annee,sep="")
        mat_contrat=as.Date(mat_contrat,"%d-%m-%y")
        ttm=as.numeric(mat_contrat-today)
        # Test pour l'existence du contrat
        test=try(which(dates_temp==today))
        if (is.character(test)==FALSE && ttm>2 && length(test)>0){
        prix=as.numeric(prix[test,])
        strike=as.numeric(strike)
        SJ=as.numeric(SJ[test])
        tsr=as.numeric(tsr[test])
          # RÈcupÈration des prix
        if (length(prix)>0){
          if (type=="call"){
          dataset[[index_list]]=""
          dataset[[index_list]]$call=""          
          dataset[[index_list]]$call$SJ=SJ
          dataset[[index_list]]$call$tsr=tsr
          dataset[[index_list]]$call$prix=prix
          dataset[[index_list]]$call$strike=strike
          dataset[[index_list]]$call$ttm=ttm
          }else{
          dataset[[index_list]]=""
          dataset[[index_list]]$put=""                    
          dataset[[index_list]]$put$SJ=SJ
          dataset[[index_list]]$put$tsr=tsr
          dataset[[index_list]]$put$prix=prix
          dataset[[index_list]]$put$strike=strike
          dataset[[index_list]]$put$ttm=ttm
          }
          index_list=index_list+1
        }
         
#        }else{
#          if (type=="call"){
#          dataset[[index_list]]=""
#          dataset[[index_list]]$call="Empty"
#          }else{
#          dataset[[index_list]]$put="Empty"
#          }
#        }
#      }# Fin de la boucle type
      }
    }# Fin de la boucle sur les mois
  }# Fin de la boucle annÈe
  }
# Export des donnÈes au bon format pour le pricer 
exportation<-function(dataset){
# Export des caractÈristiques des contrats actifs
n=length(dataset)
strike_call=list()
strike_put=list()
prix_call=list()
prix_put=list()
ttm=c()
index_call=1
index_put=1
for (i in 1:n){
  flag_call=0
  flag_put=0
  if (dataset[[i]]$call=="Empty"){flag_call=1} else{
  if (length(dataset[[i]]$call$strike)==0){flag_call=1}}
  if (dataset[[i]]$put=="Empty"){flag_put=1} else{
  if (length(dataset[[i]]$put$strike)==0){flag_put=1}}
  
  if (flag_call==0){
  strike_call[[index_call]]=dataset[[i]]$call$strike
  prix_call[[index_call]]=dataset[[i]]$call$prix
  ttm=rbind(ttm,dataset[[i]]$call$ttm)
  if (index_call==1){SJ=dataset[[i]]$call$SJ;tsr=dataset[[i]]$call$tsr}
  index_call=index_call+1
  }
  
  if (flag_put==0){
  strike_put[[index_put]]=dataset[[i]]$put$strike
  prix_put[[index_put]]=dataset[[i]]$put$prix
  index_put=index_put+1
  }
}
return(list(prix_put=prix_put,prix_call=prix_call,strike_call=strike_call,strike_put=strike_put,tsr=tsr,ttm=ttm,SJ=SJ))
}
data=exportation(dataset)

