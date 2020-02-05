######  Step 1 : The volatility updating rule
###### The data set :
source("./Code data.R.R")
source("./Finale NLS.R")
source("./codeBase_SJ.R")

###### Constant of the model :
r<-
h1<-            # The first value for h 
h1_star<-       # The first value for h_star 
  

###### 1- Under the historical probability:
h<-function(para_h){
                
    h = c()     #  A vector containing h from the model 
    h[1]=h1     # The first value for h 
    
    #para_h<-c() set up the parameters of the model para_h[1]=w, para_h[2]=b, para_h[3]=c, para_h[4]=neta, para_h[5]=nu, para_h[6]=a
    
    for (i in 2:length(X)){
      h[i]=para_h[1]+ (para_h[2]*h[i-1])+ ((para_h[3]/para_h[4])*(X[i-1] - r - (para_h[5]*h[i-1])))+((para_h[6]*para_h[4])*(h[i-1])^2)/(X[i-1] - r- (para_h[5]*h[i-1]))
    }
    
    return(h)
  }
  
  
###### 2- Under the risk neutral probability:
  
h_star<-function(para_h,para_h_star){
    h_star = c() #  A vector containing h_star from the model 
    
    h_star[1]=h1_star # The first value for h_star 
    
    #para_h_star<-c()  set up the parameters of the model para_h_star[1]=Pi, para_h_star[2]=neta_star
    
    
    for (i in 2:length(X)){
      h_star[i]=para_h[1]*para_h_star[1]+ (para_h[2]*h_star[i-1])+ ((para_h[3]*para_h_star[1]/para_h[4])*(X[i-1] - r- ((para_h[5]/para_h_star[1])*h_star[i-1])))+((para_h[6]*para_h[4]/para_h_star[1])*(h_star[i-1])^2)/(X[i-1] - r- ((para_h[5]/para_h_star[1])*h_star[i-1]))
    }
    return(h_star)
  }
  
  
  