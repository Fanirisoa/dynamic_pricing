############################################################
####                  RMSE filter                         ##
############################################################

RMSEfiltre <- function(Data.N)
{  
  error = Data.N$Er
  rmse<-sqrt((mean(error)))
  return(list(rmse=rmse)) 
}

RMSEfiltre2 <- function(Data.N)
{  
  error = Data.N$Er2
  rmse<-sqrt((mean(error)))
  return(list(rmse=rmse)) 
}



RMSE2$rmse
Pr=RMSE2$P
Er=RMSE2$error



Data.new<-data.frame(C=Data.N$C,K=Data.N$K,S=Data.N$S,T=Data.N$T,r=Data.N$r,d=Data.N$d,Pe=Data.N$Pe,Per=Data.N$Per,Mod=Data.N$Mod,CsK=Data.N$CsK,Pr,Er)

Niv=1.00e-02

# 2009
Data.N5=Data.new[-c(85,115,146,198,203,356,376,404,440,462,488,519,568,618,665,701,756,780,861,1049,1056,1073,1102,1153,1177,1297),]

# 2010
Data.N5=Data.new[-c(7,32,66,92,144,150,178,221,230,252,260,277,284,285,304,336,359,366,406,473,479,505,511,535,542,572,579,690,731,758,765,766,784,846,853,896,923,928,946,950,972,1005,1044,1075,1102,1110,1129,1136,1158,1381,1445),]


Data.N6=Data.new[!Data.new$Er<= Niv,]
Data.N7=Data.new[!Data.new$Er>= Niv,]

RMSEfiltre(Data.N5) 
RMSEfiltre(Data.N6)
RMSEfiltre(Data.N7)   #  0.0146842

Data.N8=Data.N5[!Data.N5$Er<= Niv,]
Data.N9=Data.N5[!Data.N5$Er>= Niv,]


RMSEfiltre(Data.N8)
RMSEfiltre(Data.N9)   #  0.0146842

save(Data.ret,Data.N,Data.N6,Data.N7,Data.returns,Data.new, file="DataPrice2009.Rdata")

############################################################
####              Resulte with calibraition               ##
############################################################



RMSE2$rmse
Pr2=RMSE2$P
Er2=RMSE2$error

Data.new2<-data.frame(C=Data.new$C,K=Data.new$K,S=Data.new$S,T=Data.new$T,r=Data.new$r,d=Data.new$d,Pe=Data.new$Pe,Per=Data.new$Per,Mod=Data.new$Mod,CsK=Data.new$CsK,Pr=Data.new$Pr,Er=Data.new$Er,Pr2,Er2)

Data.new2[1:10,]

Data.N8=Data.new2[!Data.new2$Er2<= Niv,]
Data.N9=Data.new2[!Data.new2$Er2>= Niv,]

RMSEfiltre2(Data.N8)
RMSEfiltre2(Data.N9)   #  0.0146842


Data.N10=Data.new2[!Data.new2$Er>= Niv,]

RMSEfiltre2(Data.N10)

