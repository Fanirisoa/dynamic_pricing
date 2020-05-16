MPE_Vix = c(0.01108,0.01038,-0.0091,-0.0095,-0.0026,-0.0019,-0.0015,-0.0057,-0.0046,-0.0032,-0.0013,-0.00072,0.00091,0.00082,-0.0030,-0.00067,-0.00059,-0.00063,-0.00036,-0.00141,-0.00083,-0.00054,-0.00031,-0.00042) 
MAE_Vix = c(0.01164,0.11319,0.00942,0.00921,0.00465,0.00471,0.00451, 0.00625,0.00506,0.00410,0.00422,0.00388,0.00390,0.00376,0.00398,0.00368,0.00287,0.00294,0.00272,0.00309,0.00348,0.00277,0.00275,0.00273)
RMSE_Vix= c(0.27743,0.26959,0.24002,0.17945,0.17693,0.16653,0.16729, 0.18092,0.16834,0.16285,0.14339,0.12331,0.10552,0.12408,0.13700,0.11737,0.10445,0.10281,0.09911,0.12890,0.12390,0.09865,0.09357,0.09158)   

in_RMSE = c( 0.05939,0.05747,0.057183,0.05574,0.05801,0.05483,0.05570,0.05739,0.05502,0.05670,0.05199,0.05217,0.05124,0.04633,
             0.05110,0.05137,0.05124,0.05168,0.05016,0.054358,0.046160,0.043546,0.046483,0.043872)
out_RMSE= c( 0.07770,0.07733,0.076618,0.07339,0.07351,0.06500,0.07299,0.07004,0.06894,0.069001,0.06397,0.06488,0.05956,0.05929,
             0.06275,0.06331,0.06289,0.06240,0.06012,0.067427,0.061058,0.056641,0.061834,0.057568)
We_RMSE=  c( 0.06647,0.06511,0.065260,0.06101,0.06145,0.05921,0.05928,0.05950,0.05925,0.05869,0.05103,0.05143,0.05042,0.05044,
             0.05147,0.05152,0.05108,0.05005,0.049671,0.05147,0.05005,0.04801,0.051011,  0.04809)

Model_Garch= c("G_HN_Ret_ess","G_GJR_Ret_ess","G_NGARCH_Ret_ess","G_HN_Op_Ret_ess","G_HN_Ret_Vix_ess","G_GJR_Ret_Vix_ess","G_NGARCH_Ret_Vix_ess",
               "NIG_HN_Ret_ess","NIG_GJR_Ret_ess","NIG_NGARCH_Ret_ess","NIG_HN_Op_Ret_ess","NIG_HN_Ret_Vix_ess","NIG_GJR_Ret_Vix_ess","NIG_NGARCH_Ret_Vix_ess",
               "G_HN_Op_Ret_Qua" ,"G_HN_Ret_Vix_Qua","G_GJR_Ret_Vix_Qua","G_NGARCH_Ret_Vix_Qua","NIG_NGARCH_Ret_Vix_EGP","IG_Ret_ess","IG_Opt_Ret_ess","IG_Opt_Ret_Ushp","IG_Ret_Vix_ess","IG_Ret_Vix_Ushp")


#########################
### Generate Table   ####
#########################

A= data.frame(Model_Garch=Model_Garch,in_RMSE =in_RMSE ,out_RMSE=out_RMSE,We_RMSE=We_RMSE,RMSE_Vix=RMSE_Vix,MAE_Vix = MAE_Vix,MPE_Vix =MPE_Vix)

################################
###  Rank the Performance   ####
###############################

B=data.frame(Model_Garch=Model_Garch)
B$Rank_in_RMSE[order(A$in_RMSE ,decreasing=FALSE)]=1:nrow(A)
B$Rank_out_RMSE [order(A$out_RMSE ,decreasing=FALSE)]=1:nrow(A)
B$Rank_We_RMSE [order(A$We_RMSE ,decreasing=FALSE)]=1:nrow(A)
B$Rank_RMSE_Vix[order(A$RMSE_Vix ,decreasing=FALSE)]=1:nrow(A)
B$Rank_MAE_Vix[order(A$MAE_Vix ,decreasing=FALSE)]=1:nrow(A)
B$Rank_MPE_Vix[order(abs(A$MPE_Vix),decreasing=FALSE)]=1:nrow(A)



A
B



& $0.05739$  & $0.05502$  & $0.05199$& $0.05217$& $0.05124$\\ 
& $0.07004$  & $0.06894$  & $0.06397$& $0.06488$& $0.05956$\\ 



0.05574,0.05801,0.05483)     Gaussian
0.05199,0.05217,0.05124)      NIG
##########################
### MAtrice individuel####
##########################
OUT = c(0.05574,0.05801,0.05483)
WE =  c(0.05199,0.05217,0.05124)
k = length(OUT)

MatA = matrix(0, nrow = k, ncol = k)

for(i in 1:k){
  for(j in 1:k){
    if(i<j) {
      MatA[i,j] <- 100*(OUT[i]- OUT[j])/OUT[i]
    } else {
      MatA[i,j] <- 100*(WE[j]- WE[i])/WE[j]
    }
  }
}


OUT = c(0.07339,0.07351,0.06500)
WE =  c(0.06397,0.06488,0.05956) 
MatB = matrix(0, nrow = k, ncol = k)

for(i in 1:k){
  for(j in 1:k){
    MatB[i,j] <- 100*(OUT[j]- WE[i])/OUT[j]
    }
}

MatB


100*(0.06500-0.06488)/0.06500

a= 0.063051
b= 0.061277   
100*(a-b)/a


100*(0.07339-0.05801)/v




