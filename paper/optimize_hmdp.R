
library(hmdpPricePigIT)

source("paremeters estimations/RRM parameters.R")
source("paremeters estimations/SSMs parameters.R")

param<-setParameters(tMax=15,
                     pigs=15,
                     meanWeights = weightMean,
                     sdWeights = weightSd,
                     modPolicy = F, # we set to TRUE, if HMPD should be solved based on a given policy.  
                     centerPointsTP=round( seq(9.2,12.2,length=16), 2 ),
                     centerPointsTF=round( seq(1.5,2.2,length=15), 2 ),
                     centerPointsSP=round( seq(-0.4,0.4,length=5), 2 ), #must include 0
                     centerPointsSPi=round( seq(3.5,3.7,length=5), 2 ), #round( seq(3.5,3.8,length=3), 2 ),
                     centerPointsSF=round( seq(-0.1,0.1,length=5), 2), #round( seq(-0.1,0.1,length=9), 2)  # #must include 0
                     iMTP=0, # index of prior for pork price (when modPolicy = T)
                     iMTF=0, # index of prior for feed price deviation (when modPolicy = T)
                     iMSP=2, # index of prior for pork price deviation (when modPolicy = T)
                     iMSF=2, # index of prior for feed price deviation (when modPolicy = T)
                     iMSPi=1 # index of prior for piglet price deviation (when modPolicy = T)
)

# Compute the reward values for the culled pigs and the feedintake data 
set.seed(123456)
rewParam<-SimulatePigs(pigs=param$pigs, samples=10000, weeks=param$tMax, prices = param$centerPointsTP, V = round(varianceRRM,5) , B = meanRRM,  R = errorRRM)
param$feedWeek = rewParam$feed7
param$feedCull = rewParam$feed3
for(i in 1:length(param$centerPointsTP) )
  param$rewCull[[i]] <- rewParam$rew[,,i] 

load("policy evaluation/paramPolicy")  #if param$modPolicy is set to FALSE, a predefind paramPolicy is loaded but it is not used in the model

prefix<-"main_"
BuildHMDP2(prefix, param, paramDLMP, paramDLMPi,  paramDLMF, paramPolicy)  

#solve:
rm(mdp)
wLbl<-"Reward"
durLbl<-"Time"
mdp<-loadMDP(prefix, check = T,verbose = T)
g<-policyIteAve(mdp,wLbl,durLbl, maxIte = 5)     
policy<-getPolicy(mdp)              

do.call(file.remove,list(list.files(pattern = ".bin")))

#-----------------------------------------------------------------------------------------------------------------
#Find the optimal policy of the related external process and modify it without considering price variations.
if(param$modPolicy==F){
  require(stringr)
  rm(mdpExternal,policyExternal)
  mPolicy<-list()
  for(j in 0:(dim(param$IntervalsTF)[1]-1) ){
    idFeed<-j
    g<-g
    IdExternal<-subset(policy, stateLabel==paste("(",0,",",idFeed,",",0,")",sep="") )$sId
    infoExternal<-infoMDP(mdp = mdp, sId = IdExternal)
    stateFirst<-as.character(unlist(infoExternal$state["stateStr"]))
    lastStage<-paste(stateFirst,"0","1",sep=",")
    termValues<-getPolicy(mdp, stageStr = lastStage)$weight
    pfx<-paste("exPro",idFeed,"_",sep = "")
    mdpExternal<-loadMDP(pfx,check = T,verbose = T)
    valueIte(mdp = mdpExternal, w = wLbl,dur = durLbl, termValues=termValues, g=g)
    policyExternal<-getPolicy(mdpExternal)
    policyExternalM<-policyExternal
    # state structure: getLabel(iTP,iSP,iFeed,iSF,iSPi,n,s);
    
    policyExternalM$actionLabelM<-policyExternal$actionLabel
    
    policyExternalM<-policyExternalM[,-c(1,3,5,6)]
    policyExternalM[policyExternalM=="term."]<--2
    policyExternalM[policyExternalM=="cont."]<--1
    policyExternalM[policyExternalM=="dummyTerm"]<--3
    policyExternalM$actionLabel<-as.numeric(policyExternalM$actionLabel)
    policyExternalM<-subset(policyExternalM,!is.na(actionLabel))
    
    aaa<-do.call(rbind, str_split(gsub( "\\(|\\)" , "" , unlist(policyExternalM$stateLabel) ),","))
    
    policyExternalM$TP<-as.integer(aaa[,1])
    policyExternalM$SP<-as.integer(aaa[,2])
    policyExternalM$TF<-as.integer(aaa[,3])
    policyExternalM$SF<-as.integer(aaa[,4])
    policyExternalM$SPi<-as.integer(aaa[,5])
    policyExternalM$n<-as.integer(aaa[,6])
    policyExternalM$s<-as.integer(aaa[,7])
    
    policyExternalM<-policyExternalM[,-1]
    policyExternalM<-as.matrix(policyExternalM)
    
    mPolicy[[j+1]]<-policyExternalM
    
  }
  
  paramPolicy<-mPolicy
  
  save(file = "paramPolicy",paramPolicy)
  
}
