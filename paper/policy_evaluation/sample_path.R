
library(hmdpPricePigIT)
library(data.table)

#Estimate the RRM parameters
source("../paremeters_estimations/rrmParam.R", chdir = TRUE)

# define the param 
param<-setParameters(tMax=15,
                     pigs=15,
                     tStartMarketing=9,
                     meanWeights = weightMean,
                     sdWeights = weightSd,
                     modPolicy = TRUE, # we set to TRUE, if HMPD should be solved based on a given policy.  
                     centerPointsTP= round( seq(9.2,12.2,length=16), 2 ), #, round( seq(9.2,11.2,length=2), 2 ), #, 
                     centerPointsTF= round( seq(1.5,2.2,length=15), 2 ), # , round( seq(1.5,1.7,length=2), 2 ), #
                     centerPointsSP= round( seq(-0.4,0.4,length=5), 2 ), #must include 0
                     centerPointsSPi= round( seq(3.5,3.7,length=5), 2 ), #round( seq(3.5,3.8,length=3), 2 ),
                     centerPointsSF=round( seq(-0.1,0.1,length=5), 2), #round( seq(-0.1,0.1,length=9), 2)  # #must include 0
                     iMTP = idTP, # index of prior for pork price (when modPolicy = T)
                     iMTF = idTF, # index of prior for feed price (when modPolicy = T)
)

porkPrice = 9.4
feedPrice = 1.6  
#find the index id of pork and feed prices: 
idTP = 2; #index of pork price (predefined values)
idTF = 2; # index of feed price (predefined values) 
if(givenPolicy){
  idTP = findIndex(porkPrice,param$IntervalsTP) # index of prior for pork price based on price setting (when modPolicy = T)
  idTF = findIndex(feedPrice,param$IntervalsTF) # index of prior for feed price based on price setting (when modPolicy = T)
}

# generate the seed number 
set.seed(243565745)
seed_numbers <- runif(100, 1232424, 33435322)

time_h <- (param$tMax-param$tStartMarketing +2)
price_sample_paths <- purrr::map(seed_numbers, .f = function(x){
  set.seed(x)
  iMTF <- sample(0:(length(param$centerPointsTF)-1), 1)
  iMTP <- c()
  iMSP <- c()
  iMSF <- c()
  iMSPi <- c()
  set.seed(x)
  for(s in 1:time_h){ #TODO: find the max stage (8)
    iMTP[s] <- sample(0:(length(param$centerPointsTP)-1), 1) 
    iMSP[s] <- sample(0:(length(param$centerPointsSP)-1), 1)
    iMSF[s] <- sample(0:(length(param$centerPointsSF)-1), 1)
    iMSPi[s] <- sample(0:(length(param$centerPointsSPi)-1), 1)
  }  
  data.frame(iMTF = rep(iMTF,time_h), iMTP = iMTP, iMSP = iMSP, iMSF = iMSF, iMSPi = iMSPi)  
}) %>% setNames(seed_numbers)

param$sample_path <- price_sample_paths[[1]]

#Estimate the SSMs parameters
source("../paremeters_estimations/ssmParam.R", chdir = TRUE)

#Estimate the HMDP reward parameters
source("../paremeters_estimations/hmdpRewardParam.R", chdir = TRUE)

#Build the HMDP
prefix<-"main_"
load("paramPolicy")  #if param$modPolicy is set to FALSE, a predefind paramPolicy is loaded but it is not used in the model
BuildHMDP2(prefix, param, paramDLMP, paramDLMPi,  paramDLMF, paramPolicy)  

#solve the HMDP:
rm(mdp)
wLbl<-"Reward"
durLbl<-"Time"
mdp<-loadMDP(prefix, check = T,verbose = T)
g<-policyIteAve(mdp,wLbl,durLbl, maxIte = 5)     
policy<-getPolicy(mdp)              
if(param$modPolicy==T)
  do.call(file.remove,list(list.files(pattern = ".bin")))
