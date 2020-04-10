############# Written by JMA
############# 23/10/2019
rm(list=ls(all=TRUE))

library(data.table)
library(reshape2)

# Loading data
load("Data/HMD_Data.RData")

no.lifetables <- length(HMDL[Age == 0]$ex)

# source functions
source('R/Functions_1.R')

# Choose values for epsilon
epsilon <- c(.5,1,2)
#x <- 0

# ge the H_index, efficiency and inequaliy for different values of epsilon
HLI_Data <- do.call(rbind,lapply(epsilon, function(x, LT){
  HLI <- LT[,H_index(mx = mx,epsilon = x,age = 0:110 ,sex = Sex[1]), by = list(PopName,Sex,Year)]
  HLI$epsilon <- x
  HLI
},LT = HMDL))

# ge the decomposition for two consecutive years
HLI_Decomp <- do.call(rbind,lapply(epsilon, function(x, LT){
  HLI.decomp.country <- LT[,inner.decomp.function(DT = .SD,epsilon = x,age = 0:110,sex = Sex[1]), 
                           by = list(PopName,Sex)]
  HLI.decomp.country$epsilon <- x
  HLI.decomp.country
},LT = HMDL))

save(HLI_Data,HLI_Decomp,file = 'Data/Results_HLI.RData')
