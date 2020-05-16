Time = c(9.628767,)
  = c(2.726401,)

Pers =    c(0.8812065,0.9947403,
MPE_Vix = c(-0.049052,0.044396,
MAE_Vix = c(0.065709,0.062982,
RMSE_Vix= c(0.2867499, 0.2645451,


MPE_Vix = c( 0.97350, 0.87973,0.860551, 0.90095, 0.83456, 0.73347, 0.80045 ,0.87514, 0.87963, 0.88136, 0.86866, 0.70743, 0.68673,
             0.66228,0.71472, 0.74446, 0.7641, 0.7433, 0.7853,0.9851, 0.7248, 0.7187, 0.6709, 0.6392)
MAE_Vix = c( 0.98711, 0.95770, 0.980997, 0.92570, 0.84813, 0.76549, 0.81463,0.90047, 0.92347, 0.95737, 0.87614, 0.72866, 0.67457,
             0.68769,0.72266, 0.77113, 0.7534, 0.7600, 0.9712,1.0967, 0.7408, 0.7293, 0.6823, 0.6505)
RMSE_Vix= c( 1.15993, 1.03488, 1.00128, 1.00319, 1.00007, 0.94301, 1.00231,0.98012, 0.99775, 1.02711, 1.01022, 0.92415, 0.90427, 
             0.83287,1.00682, 0.90737, 1.0033, 1.0009, 0.9712,1.2023, 0.8682, 0.8593, 0.8210, 0.7846)

in_RMSE = c( 0.05939, 0.05747, 0.057183, 0.05574, 0.05801, 0.05483, 0.05570,0.05739, 0.05502, 0.05670, 0.05199, 0.05217, 0.05124, 0.04633,
             0.05110, 0.05137, 0.05124, 0.05168,0.05016,0.054358, 0.046160, 0.043546, 0.046483, 0.043872)
out_RMSE= c( 0.07770, 0.07733, 0.076618, 0.07339, 0.07351, 0.06500, 0.07299,0.07004, 0.06894, 0.69001, 0.06397, 0.06488, 0.05956, 0.05929,
             0.06275, 0.06331, 0.06289, 0.06240, 0.06012,0.067427, 0.061058, 0.056641, 0.061834, 0.057568)
We_RMSE=  c( 0.06647, 0.06511, 0.065260, 0.06101, 0.06145, 0.05921, 0.05928,0.05950, 0.05925, 0.05869, 0.05103, 0.05143, 0.05042, 0.05044,
             0.05147, 0.05152, 0.05108, 0.05005, 0.049671,0.05147, 0.05005, 0.04801, 0.051011,   0.04809)

Model_Garch= c("G_HN_Ret_ess", "G_GJR_Ret_ess", "G_NGARCH_Ret_ess","G_HN_Op_Ret_ess","G_HN_Ret_Vix_ess","G_GJR_Ret_Vix_ess","G_NGARCH_Ret_Vix_ess",
               "NIG_HN_Ret_ess", "NIG_GJR_Ret_ess", "NIG_NGARCH_Ret_ess","NIG_HN_Op_Ret_ess","NIG_HN_Ret_Vix_ess","NIG_GJR_Ret_Vix_ess","NIG_NGARCH_Ret_Vix_ess",
               "G_HN_Op_Ret_Qua" ,"G_HN_Ret_Vix_Qua", "G_GJR_Ret_Vix_Qua", "G_NGARCH_Ret_Vix_Qua", "NIG_NGARCH_Ret_Vix_EGP","IG_Ret_ess","IG_Opt_Ret_ess","IG_Opt_Ret_Ushp","IG_Ret_Vix_ess","IG_Ret_Vix_Ushp")


#########################
### Generate Table   ####
#########################

A= data.frame(Model_Garch=Model_Garch,in_RMSE =in_RMSE ,out_RMSE=out_RMSE,We_RMSE=We_RMSE,RMSE_Vix=RMSE_Vix,MAE_Vix = MAE_Vix,MPE_Vix =MPE_Vix)
A

################################
###  Rank the Performance   ####
###############################

B=data.frame(Model_Garch=Model_Garch)
B$Rank_in_RMSE[order(A$in_RMSE , decreasing=FALSE)]=1:nrow(A)
B$Rank_out_RMSE [order(A$out_RMSE , decreasing=FALSE)]=1:nrow(A)
B$Rank_We_RMSE [order(A$We_RMSE , decreasing=FALSE)]=1:nrow(A)
B$Rank_RMSE_Vix[order(A$RMSE_Vix , decreasing=FALSE)]=1:nrow(A)
B$Rank_MAE_Vix[order(A$MAE_Vix , decreasing=FALSE)]=1:nrow(A)
B$Rank_MPE_Vix[order(A$MPE_Vix , decreasing=FALSE)]=1:nrow(A)

B

C=data.frame(Model_Garch=Model_Garch)
C$in_RMSE=A$in_RMSE
C$out_RMSE=A$out_RMSE
C$We_RMSE=A$We_RMSE
C$RMSE_Vix=A$RMSE_Vix
C$MAE_Vix=A$MAE_Vix
C$MPE_Vix=A$MPE_Vix
C

A
B