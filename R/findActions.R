
#' Find the optimal actions of the HMDP given price scenarios 
#' 
#' @param scenarioNum Scenario number related to the price data in a time period
#' @param param Prameter values of the HMDP. 
#' @param startTime Start time of the time period contained price data of the related scenario 
#' @param endTime End time of the time period contained price data of the related scenario
#' @param finisher Time series of pork price date
#' @param feed Time series of feed price date
#' @param piglet Time series of piglet price date
#' @param dlmPig Time series of piglet price date
#' @param dlmFeed Time series of piglet price date
#' @param dlmPiglet Time series of piglet price date
#' @param policy optimal policy of the HMDP
#' @param mdp The HMDP loaded using \code{loadMDP}
#' @param wLbl parameters related to bulding step of HMDP
#' @param durLbl parameters related to bulding step of HMDP
#' @param g averga reward pet time unit of the optimal policy
#' @param usePig A boolean variable for inversing the trend of pork price in the given time series    
#' @param useFeed A boolean variable for inversing the trend of feed price in the given time series
#' @param usePiglet A boolean variable for inversing the trend of piglet price in the given time series
#'       
#' @return A data table containing the optimal action and other information for polting the scenarios with optimal actions
#' @export 
#' @author Reza Pourmoayed \email{rpourmoayed@@econ.au.dk} and Lars Relund \email{lars@@relund.dk}


optimalSearch<-function(param, scenarioNum, startTime, endTime,  finisher, feed, piglet, 
                        dlmPig, dlmFeed, dlmPiglet, policy, mdp, wLbl, durLbl, g, usePig, useFeed, usePiglet){
  
  #read the data and estimate the posterior parameters:
  datPig<-window(finisher, start=startTime, end=endTime)
  if(usePig){
    invDiff<- -1*diff(datPig)
    for(h in 2:length(datPig))
      datPig[h]<- datPig[h-1] + invDiff[h-1]    
  }
  
  datFeed<-window(feed, start=startTime, end=endTime)  
  if(useFeed){
    invDiff<- -1*diff(datFeed)
    for(h in 2:length(datFeed))
      datFeed[h]<- datFeed[h-1] + invDiff[h-1]    
  }
  
  datPiglet<-window(piglet, start=startTime, end=endTime)   
  if(usePiglet){
    invDiff<- -1*diff(datPiglet)
    for(h in 2:length(datPiglet))
      datPiglet[h]<- datPiglet[h-1] + invDiff[h-1]    
  } 
  
  datPigObs<-datPig #window(finisher, start=startTime, end=endTime)
  datFeedObs<-datFeed - datFeed[1]
  datpigletObs<-log(datPiglet) - log(datPig)
  
  #require(dlm)
  dlmPig$m0<-c(datPigObs[1],0) # m0 should be adjasted and C0 from parameters ?
  posPig<-round(dlmFilter(datPigObs[-1], mod = dlmPig)$m,3)#[-param$tMax,]
  dlmFeed$m0<-datFeedObs[1] # m0 should be adjasted and C0 from parameters
  posFeed<-round(dlmFilter(datFeedObs[-1], mod = dlmFeed)$m,3)#[-param$tMax]
  dlmPiglet$m0<-datpigletObs[1] # m0 should be adjasted and C0 from parameters ?
  posPiglet<-round(dlmFilter(datpigletObs[-1], mod = dlmPiglet)$m,3)#[-param$tMax]
  
  # Find the state indexes:   
  idxTP<-c()
  for(t in 1:(param$tMax ) ){
    for(i in 1:dim(param$IntervalsTP)[1]){
      if( (param$IntervalsTP[i,2]<=posPig[t,1]) && (posPig[t,1]<param$IntervalsTP[i,3]) ) 
        idxTP[t]<-i-1 
    }
  }
  
  idxSP<-c()
  for(t in 1:(param$tMax ) ){
    for(i in 1:dim(param$IntervalsSP)[1]){
      if( (param$IntervalsSP[i,2]<=posPig[t,2]) && (posPig[t,2]<param$IntervalsSP[i,3]) )
        idxSP[t]<-i-1
    }
  }
  
  idxTF<-c()  # we use datFeed and observations of the related SSM
  for(t in 1:(param$tMax ) ){
    for(i in 1:dim(param$IntervalsTF)[1]){
      if( (param$IntervalsTF[i,2]<=datFeed[1]) && (datFeed[1]<param$IntervalsTF[i,3]) )  # Based on the structure of dlmF we need always to datFeed[1] in the second level! 
        idxTF[t]<-i-1
    }
  }
  
  idxSF<-c()
  for(t in 1:(param$tMax ) ){
    for(i in 1:dim(param$IntervalsSF)[1]){
      if( (param$IntervalsSF[i,2]<=posFeed[t]) && (posFeed[t]<param$IntervalsSF[i,3]) ) 
        idxSF[t]<-i-1
    }
  }
  
  idxSPi<-c()
  for(t in 1:(param$tMax ) ){
    for(i in 1:dim(param$IntervalsSPi)[1]){
      if( (param$IntervalsSPi[i,2]<=posPiglet[t]) && (posPiglet[t]<param$IntervalsSPi[i,3]) ) 
        idxSPi[t]<-i-1
    }
  }
  
  #Find the optimal policy of the related external process
  rm(mdpExternal,policyExternal)
  IdExternal<-subset(policy, stateLabel==paste("(",0,",",idxTF[1],",",0,")",sep="") )$sId
  infoExternal<-infoMDP(mdp = mdp, sId = IdExternal)
  stateFirst<-as.character(unlist(infoExternal$state["stateStr"]))
  lastStage<-paste(stateFirst,"0","1",sep=",")
  termValues<-getPolicy(mdp, stageStr = lastStage)$weight
  pfx<-paste("exPro",idxTF[1],"_",sep = "")
  mdpExternal<-loadMDP(pfx,check = T,verbose = T)
  valueIte(mdp = mdpExternal, w = wLbl,dur = durLbl, termValues=termValues, g=g)
  policyExternal<-getPolicy(mdpExternal)
  
  opt<-c()
  optMarket<-c()
  alive<-param$pigs
  remainPigs<-c()
  idState<-c()
  idAction<-c()
  NPV<-c()
  alivePigs<-c()
  
  for(s in 1:(param$tMax-param$tStartMarketing +2 ) ){
    
    if(s==1){
      t=1
    }else{
      t = (s + param$tStartMarketing -2 ) 
    }
    
    opt[s]<-subset(policyExternal, stateLabel==paste("(",idxTP[t],",",idxSP[t],",",idxTF[1],",",idxSF[t],",",idxSPi[t],",",alive,",",s,")",sep="") ) ["actionLabel"]  
    idState[s]<-subset(policyExternal, stateLabel==paste("(",idxTP[t],",",idxSP[t],",",idxTF[1],",",idxSF[t],",",idxSPi[t],",",alive,",",s,")",sep="") ) ["sId"]
    idAction[s]<-subset(policyExternal, stateLabel==paste("(",idxTP[t],",",idxSP[t],",",idxTF[1],",",idxSF[t],",",idxSPi[t],",",alive,",",s,")",sep="") ) ["aIdx"]
    NPV[s]<-subset(policyExternal, stateLabel==paste("(",idxTP[t],",",idxSP[t],",",idxTF[1],",",idxSF[t],",",idxSPi[t],",",alive,",",s,")",sep="") ) ["weight"]
    
    
    if( (opt[s]!="cont.") && (opt[s]!="term.") && (opt[s]!="dummyTerm")  ){
      optMarket[s]<-as.numeric(opt[s])
      remainPigs[s]<-alive
      alive<-alive - optMarket[s]
    }
    
    if(opt[s]=="term."){
      optMarket[s]<-alive
      remainPigs[s]<-alive
      alive<-0
    }
    
    if(opt[s]=="cont."){
      optMarket[s]<-0
      remainPigs[s]<-alive
    }
    
    alivePigs[s]<-alive
    
    if(alive==0) break
  }
  
  
  #Store information in a data.table
  dat<-data.table(t=1:(param$tMax))
#  dat$week<-1:param$tMax
  dat$pricePig<-datPig
  dat$priceFeed<-datFeed
  dat$pricePiglet<-datPiglet/100
  dat$TP<-posPig[,1]
  dat$SP<-posPig[,2]
  dat$TF<-datFeed[1]
  dat$SF<-posFeed
  dat$SPi<-posPiglet
#  dat$idxTP<-idxTP
#  dat$idxSP<-idxSP
#  dat$idxTF<-idxTF
#  dat$idxSF<-idxSF
#  dat$idxSPi<-idxSPi
  
  dat$meanWeight<-param$meanWeights[1:(param$tMax)]
  dat$sdWeight<-param$sdWeights[1:(param$tMax)]
  
  dat$stage<-dat$t
  dat$stage[1:(param$tStartMarketing)]<-1
  dat$stage[(param$tStartMarketing):(param$tMax)]<-2:(param$tMax-param$tStartMarketing+2)
  
#  dat$idState<-NA
#  dat$idState[1]<-idState[1]
#  dat$idState[(param$tStartMarketing):(param$tStartMarketing +length(optMarket) -2)]<-idState[-1]
  
#  dat$idAction<-NA
#  dat$idAction[1]<-idAction[1]
#  dat$idAction[(param$tStartMarketing):(param$tStartMarketing +length(optMarket) -2)]<-idAction[-1]
  
  dat$optimal<-NA
  #  dat$optimal[1:(param$tStartMarketing)]<-0
  dat$optimal[(param$tStartMarketing):(param$tStartMarketing +length(optMarket) -2)]<-optMarket[-1]
  
  dat$alive<-NA 
  dat$alive[1:(param$tStartMarketing)]<-param$pigs
  dat$alive[(param$tStartMarketing):(param$tStartMarketing +length(optMarket) -2)]<-remainPigs[-1]
  
  #  dat$weight<-NA
  #  dat$weight[t=1:(param$tStartMarketing-1)]<-as.numeric(NPV[1])
  #  dat$weight[t=(param$tStartMarketing-1):(param$tStartMarketing +length(optMarket) -2)]<-as.numeric(NPV)
  
  #Calculate payoff respect to action term.
#   sIdx<-unlist(dat$idState[(param$tStartMarketing) :(param$tMax) ])
#   sIdx<-sIdx[!is.na(sIdx)]
#   actionsIdx<-c()
#   infoRPO<-infoMDP(mdp = mdpExternal, sId = sIdx)
#   actionsIdx<-subset(infoRPO$actionDF[2:3],label=="term.")$aIdx
#   
#   rpo<-calcRPO(mdpExternal, w = wLbl, sId =sIdx, iA= actionsIdx , criterion="average", dur=durLbl, g=g) 
#   dat$RPOTerm<-NA
#   dat$RPOTerm[(param$tStartMarketing):(param$tStartMarketing +length(optMarket) -2)]<-rpo[,2]
#   
#   #Calculate payoff respect to action cont.
#   sIdx<-unlist(dat$idState[(param$tStartMarketing) :(param$tMax) ])
#   sIdx<-sIdx[!is.na(sIdx)]
#   actionsIdx<-c()
#   infoRPO<-infoMDP(mdp = mdpExternal, sId = sIdx)
#   actionsIdx<-subset(infoRPO$actionDF[2:3],label=="term.")$aIdx
#   rm(rpo)
#   rpo<-calcRPO(mdpExternal, w = wLbl, sId =sIdx, iA= rep(0,length(sIdx)) , criterion="average", dur=durLbl, g=g) 
#   dat$RPOCont<-NA
#   dat$RPOCont[(param$tStartMarketing):(param$tStartMarketing +length(optMarket) -2)]<-rpo[,2]
  
  
  # Calculate payoff respect to optimal actions
  #  sIdx<-unlist(dat$idState)
  #  sIdx<-sIdx[!is.na(sIdx)]
#   sIdx<-unlist(dat$idState[(param$tStartMarketing) :(param$tMax) ])
#   sIdx<-sIdx[!is.na(sIdx)]
#   actionIdx<-unlist(dat$idAction[(param$tStartMarketing) :(param$tMax) ])
#   actionIdx<-actionIdx[!is.na(actionIdx)]
#   rm(rpo)
#   rpo<-calcRPO(mdpExternal, w = wLbl, sId =sIdx, iA=actionIdx , criterion="average", dur=durLbl,g = g) 
#   
#   dat$RPOOpt<-NA
#   dat$RPOOpt[(param$tStartMarketing):(param$tStartMarketing +length(optMarket) -2)]<-rpo[,2]
  
  dat$scenario<-scenarioNum
  
  return(dat)  
}
