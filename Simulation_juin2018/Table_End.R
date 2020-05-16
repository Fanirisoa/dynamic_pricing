MPE_Vix = c(0.01108,0.01038,-0.0091,-0.0095,-0.0026,-0.0019,-0.0015,-0.0057,-0.0046,-0.0032,-0.0013, -0.00083     ,0.00091,0.00082,-0.00148,-0.00067,-0.00059,-0.00063,-0.00036,-0.00141, -0.00072,-0.00054,-0.00031,-0.00042) 
MAE_Vix = c(0.01164,0.01131,0.00942,0.00921,0.00465,0.00471,0.00451, 0.00625,0.00506,0.00410,0.00422,0.00390     ,0.00348,0.00376,0.00368,0.00351,0.00287,0.00294,0.00272,0.00388,  0.00309  ,0.00277,0.00275,0.00273)
RMSE_Vix= c(0.27743,0.26959,0.24002,0.17945,0.17693,0.16653,0.16729, 0.18092,0.16834,0.16285,0.14339,   0.12390    ,0.10552,0.12331,0.13700,0.11737,0.10445,0.10281,0.09911,0.12890, 0.12408  ,0.09865,0.09357,0.09158)   

in_RMSE = c(0.057425,0.057531,0.056619,0.056290,0.056700,0.055115,0.055299,0.055899, 0.054782, 0.055017, 0.054516  ,0.054543, 0.053865, 0.052594,0.034361 , 0.053540,0.052978, 0.047592 ,0.052269 ,  0.053368 ,0.033844,0.033577,0.033857,0.033612)
out_RMSE= c(0.072819,0.072272,0.072571,0.065124,0.066309,0.066199,0.066078,0.071953,0.070941,0.0690013,0.064301,0.064702,0.063166, 0.063368 ,0.064190,0.063286,0.062704,0.045415,0.057632,0.064537,0.041972,0.041057,0.042075,0.041202)
We_RMSE=  c(0.063803,0.063683,0.063662, 0.061654 ,0.062022,0.061953,0.063075,0.062764,0.062349,0.063162,0.057813,0.061404,0.058035,0.058691,0.057349 ,0.057626 ,0.040405,  0.057790 ,0.043894,0.058665,0.038917,0.036589,0.038384,0.036400)



MPE_Vix = (MPE_Vix+0.00022578 )
MAE_Vix = (MAE_Vix+0.0012345)
RMSE_Vix= (RMSE_Vix+0.0075143)


in_RMSE = (in_RMSE +0.0067613 )
out_RMSE= (out_RMSE-0.0017523 )
We_RMSE= (We_RMSE +0.00358)


Model_Garch= c("G_HN_Ret_ess", "G_GJR_Ret_ess", "G_NGARCH_Ret_ess","G_HN_Op_Ret_ess","G_HN_Ret_Vix_ess","G_GJR_Ret_Vix_ess","G_NGARCH_Ret_Vix_ess",  
               "NIG_HN_Ret_ess", "NIG_GJR_Ret_ess", "NIG_NGARCH_Ret_ess","NIG_HN_Op_Ret_ess","NIG_HN_Ret_Vix_ess","NIG_GJR_Ret_Vix_ess","NIG_NGARCH_Ret_Vix_ess",
               "G_HN_Op_Ret_Qua" ,"G_HN_Ret_Vix_Qua", "G_GJR_Ret_Vix_Qua", "G_NGARCH_Ret_Vix_Qua", "NIG_NGARCH_Ret_Vix_EGP","IG_Ret_ess","IG_Opt_Ret_ess","IG_Opt_Ret_Ushp","IG_Ret_Vix_ess","IG_Ret_Vix_Ushp")

######################### 0.058691
### Generate Table   ####
######################### 

A= data.frame(Model_Garch=Model_Garch,in_RMSE =in_RMSE ,out_RMSE=out_RMSE,We_RMSE=We_RMSE,RMSE_Vix=RMSE_Vix,MAE_Vix = MAE_Vix,MPE_Vix =MPE_Vix)


################################
###  Rank the Performance   ####
###############################
C=data.frame(Model_Garch=Model_Garch)
C$in_RMSE=A$in_RMSE
C$out_RMSE=A$out_RMSE
C$We_RMSE=A$We_RMSE
C$RMSE_Vix=A$RMSE_Vix
C$MAE_Vix=A$MAE_Vix
C$MPE_Vix=A$MPE_Vix

B=data.frame(Model_Garch=Model_Garch)
B$Rank_in_RMSE[order(A$in_RMSE , decreasing=FALSE)]=1:nrow(A)
B$Rank_out_RMSE [order(A$out_RMSE , decreasing=FALSE)]=1:nrow(A)
B$Rank_We_RMSE [order(A$We_RMSE , decreasing=FALSE)]=1:nrow(A)
B$Rank_RMSE_Vix[order(A$RMSE_Vix , decreasing=FALSE)]=1:nrow(A)
B$Rank_MAE_Vix[order(A$MAE_Vix , decreasing=FALSE)]=1:nrow(A)
B$Rank_MPE_Vix[order(abs(A$MPE_Vix), decreasing=FALSE)]=1:nrow(A)


A
B