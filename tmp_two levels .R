## Build the model (test)
library(mdpPricePigIT)
library(data.table)
require(Rcpp)   
require(RcppArmadillo)
require(dlm)  
source("Weight estimate.R")

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
#-----------------------------------------------------------------------------------------------------
# Estimate the DLMs parameters: 

# Read the price data form data base
#PriceData = read.table(file=file.choose(),header=T,sep=";")
PriceData = read.table(file="data/price data from 2005.csv",header=T,sep=";")
finisher<-ts(PriceData[,3],start=c(2006,1), end=c(2014,52),frequency=52)
feed<-ts(PriceData[,4], start=c(2006,1), end=c(2014,52),frequency=52)
piglet<-ts(PriceData[,2], start=c(2006,1), end=c(2014,52),frequency=52)

# Build the SSMs and estimate the variance components

#Pig price 


buildDLMPig <- function(x){
  
  GG1 = matrix (data = c(1,0,1,1), ncol = 2)
  FF1 = matrix (data = c(1,0), nrow = 1)
  M=matrix(c(0,0,0,exp(x[1])),ncol=2 )
  W1 = M #%*% t(M)
  
  V1 = 0
  
  m01 = c( mean(finisher), mean(diff(finisher)) )
  C01 = matrix (data = c(0,0,0,var(diff(finisher)) ), ncol = 2)
  
  dlm1<-dlm(GG=GG1, FF=FF1 , W=W1, V=V1, m0=m01, C0=C01)
  
  return(dlm1)
}

Param <- dlmMLE ( finisher  , parm=rep (1, 1), buildDLMPig)#, method="Nelder-Mead" )
exp(Param$par)

dlmPig <- buildDLMPig(Param$par)

#Feed price 

iniFeed<- mean(feed) # Initial feed-mix that we consider it as average feed price 

dataFeed<- feed - iniFeed 

buildDLMFeed <- function(x){
  
  GG1 = matrix (data = c(1), ncol = 1)
  FF1 = matrix (data = c(1), ncol = 1)
  W1 = diag(1)
  diag(W1)[1:1] <-exp(x[1])
  
  V1 = exp(x[2])
  
  m01 = c(mean(dataFeed))
  C01 = matrix (data = c(var(dataFeed)), ncol = 1)
  
  dlm1<-dlm(GG=GG1, FF=FF1 , W=W1, V=V1, m0=m01, C0=C01)
  
  return(dlm1)
}

Param <- dlmMLE ( dataFeed, parm=rep (1, 2), buildDLMFeed) #, method="Nelder-Mead" )
exp(Param$par)

dlmFeed <- buildDLMFeed(Param$par)

# Piglet price

dataPiglet<-log(piglet) - log(finisher)

buildDLMPiglet <- function(x){
  
  GG1 = matrix (data = c(1), ncol = 1)
  FF1 = matrix (data = c(1), ncol = 1)
  W1 = diag(1)
  diag(W1)[1:1] <-exp(x[1])
  
  V1 = exp(x[2])
  
  m01 = 3.55 #c(mean(dataPiglet))
  C01 = matrix (data = c(var(dataPiglet)), ncol = 1)
  
  dlm1<-dlm(GG=GG1, FF=FF1 , W=W1, V=V1, m0=m01, C0=C01)
  
  return(dlm1)
}

Param <- dlmMLE ( dataPiglet, parm=rep (1, 2), buildDLMPiglet) #, method="Nelder-Mead" )
exp(Param$par)

dlmPiglet <- buildDLMPiglet(Param$par)

#-----------------------------------------------------------------------------------------------------
# create DLMs
inidlms<-paramDLMs(dlmPig, dlmFeed,dlmPiglet)
yPig<-t( seq(8,13, length=inidlms$DLMP$dimObs) ) 
yFeed<-t( seq(1.1,2.2, length=inidlms$DLMF$dimObs) ) 
yPiglet<-t( seq(340,390, length=inidlms$DLMPi$dimObs) )
yPiglet<-log(yPiglet)-log(yPig)
yFeed <- yFeed -1.1

paramDLMP<-buildDLM(inidlms$DLMP,param,yPig)
paramDLMPi<-buildDLM(inidlms$DLMPi,param,yPiglet)
paramDLMF<-buildDLM(inidlms$DLMF,param,yFeed)


load("paramPolicy")  #if param$modPolicy is set to FALSE, a predefind paramPolicy is loaded but it is not used in the model

   
prefix<-"main_"
BuildHMDP2(prefix, param, paramDLMP, paramDLMPi,  paramDLMF, paramPolicy)  

#------------------------------
#solve:
rm(mdp)
wLbl<-"Reward"
durLbl<-"Time"
mdp<-loadMDP(prefix, check = T,verbose = T)
g<-policyIteAve(mdp,wLbl,durLbl, maxIte = 5)     
policy<-getPolicy(mdp)              
#do.call(file.remove,list(list.files(pattern = ".bin")))

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
    

#Note: To find the new g value for the modified policy (stored in the roos as "paramPolicy"), first we should
# set the bool variable modPolicy to TRUE and run all the codes in this scripts. 





