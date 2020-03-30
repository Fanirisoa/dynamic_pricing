#####################################################
###              Load Data source             #######
#####################################################

getwd()

source("C:/Users/e0g411k03dt/Desktop/Sim_nov_2017_paper_2/New data/Data20092010.R") 
source("C:/Users/e0g411k03dt/Desktop/Sim_nov_2017_paper_2/New data/Data20112012.R") 

#####################################################
###             Clean the repertoir           #######
#####################################################
rm(list=ls())
gc()
library(compiler)
enableJIT(1)
enableJIT(3)
library("fBasics")

#####################################################
###    Load both data.contract and data.ret   #######
#####################################################
load("DataPrice20092010.Rdata")

########################################################################
###                 Data function :                              #######
########################################################################
Data.function<- function(Serie.data,i)
{
  
  Data.contract <- data.frame()
  Data.contract <- rbind(Data.contract, cbind(Serie.data[[i]]))
    
  
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
  
  Val.S=Data.contract$S
  
  resultat=Val.S[1]
    
  return(resultat)
}



########################################################################
###             Data Modification :                              #######
########################################################################
Data.Modification<- function(Serie.data,Data.N,i)
{

  data.S=c()    
  for(i in 1:104) {
    data.S[i] =  Data.function(Serie.data,i)
    
  }
  
a= data.S[i]


Data.modif=Data.N[Data.N$S == a ,]

return(Data.modif)
}


Data.Modification(Serie.data,Data.N2,4)

Data.ret[1:10,]
  