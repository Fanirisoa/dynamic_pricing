###################################################################################
###               Clean the repertoir and laod all the library used         #######
###################################################################################
rm(list=ls())
gc()
library(compiler)
enableJIT(1)
enableJIT(3)
library("fBasics")
#library("pracma")
library("numDeriv")
library("nlme")
library("Matrix")
library(xts)

##################################################################################################
###              Load : Data source,    Parameters of the model,  function to use          #######
##################################################################################################
setwd("/Users/leafanirisoa/Documents/projetGit/dynamic_pricing/data_used")  
path = "/Users/leafanirisoa/Documents/projetGit/dynamic_pricing/estimationJob/NIG_GARCH_Last/two_step_estimation/NGARCH_ret_vix"

source(paste(path,"/T2_parameters_set.R",sep=""))
source(paste(path,"/T2_Loglik_Ret_NGARCH.R",sep=""))
source(paste(path,"/T2_Loglik_VIX_NGARCH.R",sep=""))
source(paste(path,"/T2_Loglik_Mix_VIX_GJR.R",sep=""))
source(paste(path,"/T2_QMLNIG_VIX_GJR.R",sep=""))

