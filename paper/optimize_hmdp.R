#Note: To be able to run a model with a short time, change the values of
# centerPointsTP and centerPointsTF in "setParameters" function to:
# centerPointsTP = round( seq(9.2,11.2,length=2), 2 ),
# centerPointsTF =  round( seq(1.5,1.7,length=2), 2 ),

library(hmdpPricePigIT)


#Estimate the RRM parameters
source("paremeters_estimations/rrmParam.R", chdir = TRUE)

# full model (note long solution time)
param<-setParameters(tMax=15,
                     pigs=15,
                     tStartMarketing=9,
                     meanWeights = weightMean,
                     sdWeights = weightSd,
                     centerPointsTP =  round( seq(9.2,12.2,length=16), 2 ), # round( seq(9.2,11.2,length=2), 2 ), 
                     centerPointsTF = round( seq(1.5,2.2,length=15), 2 ), # round( seq(1.5,1.7,length=2), 2 ),
                     centerPointsSP = round( seq(-0.4,0.4,length=5), 2 ), #must include 0
                     centerPointsSPi = round( seq(3.5,3.7,length=5), 2 ), #round( seq(3.5,3.8,length=3), 2 ),
                     centerPointsSF = round( seq(-0.1,0.1,length=5), 2) #round( seq(-0.1,0.1,length=9), 2)  # #must include 0
)

# a smaller example (used as default - comment out if you want the full model)
param<-setParameters(tMax=15,
                     pigs=15,
                     tStartMarketing=9,
                     meanWeights = weightMean,
                     sdWeights = weightSd,
                     centerPointsTP = round( seq(9.2,11.2,length=2), 2 ),
                     centerPointsTF = round( seq(1.5,1.7,length=2), 2 ),
                     centerPointsSP = round( seq(-0.4,0.4,length=5), 2 ), #must include 0
                     centerPointsSPi = round( seq(3.5,3.7,length=5), 2 ), #round( seq(3.5,3.8,length=3), 2 ),
                     centerPointsSF = round( seq(-0.1,0.1,length=5), 2) #round( seq(-0.1,0.1,length=9), 2)  # #must include 0
)

#Estimate the SSMs parameters
source("paremeters_estimations/ssmParam.R", chdir = TRUE)

#Estimate the HMDP reward parameters
source("paremeters_estimations/hmdpRewardParam.R", chdir = TRUE)

#Build the HMDP
prefix<-"main_"
load("policy_evaluation/paramPolicy")  #if param$modPolicy is set to FALSE, a predefind paramPolicy is loaded but it is not used in the model
BuildHMDP2(prefix, param, paramDLMP, paramDLMPi,  paramDLMF, paramPolicy)  

#solve the HMDP:
rm(mdp)
wLbl<-"Reward"
durLbl<-"Time"
mdp<-loadMDP(prefix, check = T,verbose = T)
g<-policyIteAve(mdp,wLbl,durLbl)     
policy<-getPolicy(mdp)              
  if(param$modPolicy==T) do.call(file.remove,list(list.files(pattern = ".bin")))

