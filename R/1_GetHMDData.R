####### Program for getting most recent data from HMD
############# Written by JMA
############# 23/10/2019
rm(list=ls(all=TRUE))
library(HMDHFDplus)
library(data.table)

# Set working directory

# get all countries in HMD
XYZ <- getHMDcountries()
# set your username for HMD
us <- "...@colmex.mx"
# set your password
pw <- "...ov"

# get all the lifetables available from HMD
HMDL <- do.call(rbind,lapply(XYZ, function(x, us, pw){
  cat(x,"\n")
  Males        <- readHMDweb(x,"mltper_1x1",username=us,password=pw)
  Females      <- readHMDweb(x,"fltper_1x1",username=us,password=pw)
  Males$Sex    <- "m"
  Females$Sex  <- "f"
  CTRY         <- rbind(Females, Males)
  CTRY$PopName <- x
  CTRY    
}, us = us, pw = pw))

# convert to data.table
HMDL <- data.table(HMDL)

# save the data
save(HMDL,file="Data/HMD_Data.RData")

#number of populations in the study
length(unique(HMDL$PopName))

#number of lifetbales
length(HMDL[Age== 0,]$ex)

#period spanning 
range(HMDL$Year)
