#Note: To be able to run a model with a short time, change the values of
# centerPointsTP and centerPointsTF in "setParameters" function to:
# centerPointsTP = round( seq(9.2,11.2,length=2), 2 ),
# centerPointsTF =  round( seq(1.5,1.7,length=2), 2 ),

library(hmdpPricePigIT)
library(data.table)


#Estimate the RRM parameters
source("../paremeters_estimations/rrmParam.R", chdir = TRUE)

#find the index id of pork and feed prices: 
idTP = 2; #index of pork price (predefined values)
idTF = 2; # index of feed price (predefined values) 
if(givenPolicy){
  idTP = findIndex(porkPrice,param$IntervalsTP) # index of prior for pork price based on price setting (when modPolicy = T)
  idTF = findIndex(feedPrice,param$IntervalsTF) # index of prior for feed price based on price setting (when modPolicy = T)
}

# full model (note long solution time)
param<-setParameters(tMax=15,
                     pigs=15,
                     tStartMarketing=9,
                     meanWeights = weightMean,
                     sdWeights = weightSd,
                     modPolicy = givenPolicy, # we set to TRUE, if HMPD should be solved based on a given policy.  
                     centerPointsTP= round( seq(9.2,12.2,length=16), 2 ), #, round( seq(9.2,11.2,length=2), 2 ), #, 
                     centerPointsTF= round( seq(1.5,2.2,length=15), 2 ), # , round( seq(1.5,1.7,length=2), 2 ), #
                     centerPointsSP= round( seq(-0.4,0.4,length=5), 2 ), #must include 0
                     centerPointsSPi= round( seq(3.5,3.7,length=5), 2 ), #round( seq(3.5,3.8,length=3), 2 ),
                     centerPointsSF=round( seq(-0.1,0.1,length=5), 2), #round( seq(-0.1,0.1,length=9), 2)  # #must include 0
                     iMTP = idTP, # index of prior for pork price (when modPolicy = T)
                     iMTF = idTF # index of prior for feed price (when modPolicy = T)

)

# a smaller example (used as default - comment out if you want the full model)
param<-setParameters(tMax=15,
                     pigs=15,
                     tStartMarketing=9,
                     meanWeights = weightMean,
                     sdWeights = weightSd,
                     modPolicy = givenPolicy, # we set to TRUE, if HMPD should be solved based on a given policy.  
                     centerPointsTP = round( seq(9.2,11.2,length=2), 2 ),
                     centerPointsTF = round( seq(1.5,1.7,length=2), 2 ),
                     centerPointsSP = round( seq(-0.4,0.4,length=5), 2 ), #must include 0
                     centerPointsSPi = round( seq(3.5,3.7,length=5), 2 ), 
                     centerPointsSF = round( seq(-0.1,0.1,length=5), 2),   # #must include 0
                     iMTP = idTP, # index of prior for pork price (when modPolicy = T)
                     iMTF = idTF # index of prior for feed price (when modPolicy = T)
)

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

#-----------------------------------------------------------------------------------------------------------------
#Find the optimal policy of the related external processes  
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
  
  #paramPolicy <- data.frame(lapply(paramPolicy, as.numeric), stringsAsFactors=FALSE)
  #write.csv2(paramPolicy, file = "paramPolicy.csv",row.names=FALSE)
  save(file = "paramPolicy",paramPolicy)
  
  do.call(file.remove,list(list.files(pattern = ".bin")))
  
}

