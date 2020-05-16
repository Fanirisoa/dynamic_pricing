#############################
###    NIG-GARCH-HN   #######
#############################
###     Option-Ret    #######
para_h_1<-c( 5.881028e-07,  1.506407e-06,  4.550518e+02,  6.500114e-01,  8.596182e+00)

para_distribution_1<-c(5.113353e+01, -6.937019e+00,  9.422875e-03,  9.754587e-05)


X_1 = E_errorHN(para_h_1,Data.returns)

###       VIX-Ret    #######
para_h_2<-c(9.484479e-09, 1.522912e-06,  4.628164e+02,  6.620771e-01,  1.869111e+00, 9.646981e-01)


para_distribution_2<-c(2.237095e+01, -1.047143e+01,  7.135328e-03,  8.048087e-04)
para_distribution_2<-c(23.254077016, -14.397610234,   0.007012446,  -0.286886696)

X_2 = E_errorHN(para_h_2,Data.returns)
#############################
###    NIG-GARCH-GJR  #######
#############################
###       VIX-Ret    #######

para_h_3<-c( 5.987174e-06,  1.240911e-01,  2.314265e-02,  8.504269e-01,  3.784983e-02,  9.757894e-01)

para_distribution_3<-c(50.9649933889, -4.2144660894,  0.0095102381 , 0.0001842042)

X_3 = E_errorGJR(para_h_3,Data.returns)



##############################################################################
#     Fit the datareturns with NIG distribution based on GH distribution     # 
##############################################################################

fgu_1 <- fitdist(X_1, "s", start=para_distribution_1)
fgu_2 <- fitdist(X_2, "s", start=para_distribution_2)
fgu_3 <- fitdist(X_3, "s", start=para_distribution_3)


summary(fgu_1)
summary(fgu_2)
summary(fgu_3)
##########################
#       Density plot     # 
##########################
x  <- seq(-0.05, 0.05, 0.000005)
y1 <- ds(x,fgu_1$estimate[1], fgu_1$estimate[2], fgu_1$estimate[3],  fgu_1$estimate[4])
y2 <- ds(x,fgu_2$estimate[1], fgu_2$estimate[2], fgu_2$estimate[3],  fgu_2$estimate[4])
y3 <- ds(x,fgu_3$estimate[1], fgu_3$estimate[2], fgu_3$estimate[3],  fgu_3$estimate[4])

plot( x, y1,ylim = c(0,50),xlab="NIG Distribution ",ylab="Density function", type="l", col="red", main = "NIG Garch Model")
lines(x,y2,col="blue")
lines(x,y3,col="3")
legend(0.02,48,c("NIG-HN-VIX","NIG-HN-OpRet","NIG-GJR-VIX"), lty=c(1,1,1), lwd=c(2.5,2.5,2.5),col=c("blue","red","3"),cex=0.8)

##########################
#    q-q plot, p-p plot  # 
##########################
windows(width=1, height=1) 

cdfcomp(fgu_1, addlegend=FALSE)
cdfcomp(fgu_2, addlegend=FALSE)
cdfcomp(fgu_3, addlegend=FALSE)

denscomp(fgu_1, addlegend=FALSE)
denscomp(fgu_2, addlegend=FALSE,fitcol="blue")
denscomp(fgu_3, addlegend=FALSE,fitcol="3")

ppcomp(fgu_1, addlegend=FALSE)
ppcomp(fgu_2, addlegend=FALSE)
ppcomp(fgu_3, addlegend=FALSE)

qqcomp(fgu_1, addlegend=FALSE,fitcol="red", main = "NIG-GARCH-HN Option-Returns")
qqcomp(fgu_2, addlegend=FALSE,fitcol="blue", main = "NIG-GARCH-HN VIX-Returns")
qqcomp(fgu_3, addlegend=FALSE,fitcol="3", main = "NIG-GARCH-GJR VIX-Returns")


