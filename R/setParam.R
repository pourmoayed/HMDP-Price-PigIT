#' Set the parameters used when build the HMDP model
#' 
#' @param tMax Maximum growth period in weeks.
#' @param tStartMarketing Week where start to consider marketing.
#' @param pigs Number of pigs in a pen at insertion.
#' @param minMarketingSize Desired minimum marketing group.
#' @param avgGRate Average growth rate in the herd. 
#' @param avgLeanP Average leanness percent in the herd.
#' @param avgInsWeight Avarage weight of the pigs at the insertion time into the pen.
#' @param avgInsSd Avarage of standard deviation of weight of the pigs at the insertion time into the pen.
#' @param convRateSd Sd of conversion rate between live weight and slaughter weight. 
#' @param cleaningPeriod Cleaninig period after terminating the pen.
#' @param marketingLength Daily length from marketing action to sending the pigs to slaughterhouse.
#' @param pigletLeadTime Lead time length of buying new piglets. Related decision is made in termination time.
#' @param meanWeights Avergae weight of the pigs in the RRM model.
#' @param sdWeights Standard deviation of weights during the growing period in the RRM model.
#' @param meanGrowth Average growth of the pigs in the RRM model.
#' @param sdGrowth Standard deviation of growth during the growing period in the RRM model.
#' @param modPolicy a boolean value showing the HMDP is solved for a given policy or not. If we set it to TRUE 
#' the average rewrad per time unit will be caculted for a given policy "paramPolicy" in functin \code{\link{BuildHMDP2}}. 
#' if it is set to FALSE, the optimal policy of the HMDP and the related average reward per time unit wil be found.
#' @param sample_path_given True if a sample path of prices is used for solving HMDP under given prices. Otherwise fixes pricesed will be used.            
#' @param rolling_horizon_model True if rolling horizom model is used where future prices are assumed the same as in the current epoch given a sample path.
#' @param rewCull A list containing the rewards of culling 1 pig according to the different pork prices.
#' @param feedWeek A matrix containing the feed intake of sorted pigs for one 1 week.
#' @param feedCull A matrix containing the feed intake of sorted pigs for marketingLength days.
#' @param iMSP id of pork price deviation in a given modified policy (without price deviation)
#' @param iMTP id of pork price in a given modified policy (without price deviation)
#' @param iMTF id of feed price in a given modified policy (without price deviation)
#' @param iMSF id of feed price deviation in a given modified policy (without price deviation)
#' @param iMSPi id of piglet price deviation in a given modified policy (without price deviation)
#' @param sample_path A matrix for sample path of price info used for modified policy. 
#' @param centerPointsTP Center points used in the discretization for the trend of pig price estimate.
#' @param centerPointsTF Center points used in the discretization for the trend of feed price estimate.
#' @param centerPointsSP Center points used in the discretization for the slope of pig price estimate.
#' @param centerPointsSF Center points used in the discretization for the slope of feed price estimate.
#' @param centerPointsSPi Center points used in the discretization for the slope of piglet price estimate.
#' @return A list containing all the parameters
#' @export 
#' @author Reza Pourmoayed \email{rpourmoayed@@econ.au.dk} and Lars Relund \email{lars@@relund.dk}
setParameters<-function(tMax=15,
                        tStartMarketing=9,
                        pigs=15,
                        minMarketingSize = 0, #for testing I have assumed this number is 0 
                        avgGRate=6,
                        avgLeanP=61,
                        avgInsWeight=30,
                        avgInsSd=2.2, # This value must be included in the related state vector!
                        convRateSd = 1.4,
                        cleaningPeriod=4,  # unit is day  
                        marketingLength=3, # unit is day
                        pigletLeadTime=1,  # unit is week
                        modPolicy=FALSE,
                        sample_path_given=FALSE,
                        rolling_horizon_model = FALSE,
                        rewCull = list(),
                        feedWeek = matrix(),
                        feedCull = matrix(),
                        iMTP=2,
                        iMTF=2,
                        iMSP=2,
                        iMSF=2,
                        iMSPi=1,  
                        meanWeights=seq(avgInsWeight,144,by=6), 
                        sdWeights=round( seq(avgInsSd,13,length=tMax), 2), 
                        meanGrowth=seq(5.5,9,by=0.2),  
                        sdGrowth=seq(1,2.5,by=0.1),                                                
                        centerPointsTP=round( seq(8,13,length=8), 2 ),
                        centerPointsTF=round( seq(1.1,2.2,length=6), 2 ),
                        centerPointsSP=round( seq(-0.4,0.4,length=5), 2 ),
                        centerPointsSPi=round( seq(3.45,3.65,length=4), 2 ),
                        centerPointsSF=round( seq(-0.2,0.2,length=5), 2),
                        sample_path = matrix()
                        
){
   model<-list(tMax=tMax)   
   model$tStartMarketing<-tStartMarketing  
   model$pigs<-pigs  
   model$minMarketingSize<-minMarketingSize
   model$avgGRate<-avgGRate
   model$avgLeanP<-avgLeanP
   model$avgInsWeight<-avgInsWeight
   model$avgInsSd<-avgInsSd
   model$convRateSd<-convRateSd
   model$cleaningPeriod<-cleaningPeriod
   model$marketingLength<-marketingLength
   model$pigletLeadTime<-pigletLeadTime
   model$meanWeights<-meanWeights            
   model$sdWeights<-sdWeights
   model$meanGrowth<-meanGrowth            
   model$sdGrowth<-sdGrowth   
   model$modPolicy<-modPolicy
   model$sample_path_given <-sample_path_given
   model$rolling_horizon_model <- rolling_horizon_model
   model$rewCull<-rewCull
   model$feedWeek<-feedWeek
   model$feedCull<-feedCull
   model$iMSP<-iMSP
   model$iMTP<-iMTP
   model$iMTF<-iMTF
   model$iMSF<-iMSF
   model$iMSPi<-iMSPi
      
   model$centerPointsTP<-centerPointsTP
   model$centerPointsTF<-centerPointsTF
   model$centerPointsSP<-centerPointsSP
   model$centerPointsSPi<-centerPointsSPi
   model$centerPointsSF<-centerPointsSF
      
   #require(discretizeGaussian)          # We start the discritization from here 
   obj<-Discretize()
   IntervalsTP<-matrix()
   IntervalsTF<-matrix()
   IntervalsSP<-matrix()
   IntervalsSPi<-matrix()
   IntervalsSF<-matrix()
   
   IntervalsTP<-as.matrix(obj$discretize1DVec(centerPointsTP, inf=100, asDF=F), ncol=3)
   IntervalsTF<-as.matrix(obj$discretize1DVec(centerPointsTF, inf=100, asDF=F), ncol=3)
   IntervalsSP<-as.matrix(obj$discretize1DVec(centerPointsSP, inf=100, asDF=F), ncol=3)
   IntervalsSPi<-as.matrix(obj$discretize1DVec(centerPointsSPi, inf=100, asDF=F), ncol=3)
   IntervalsSF<-as.matrix(obj$discretize1DVec(centerPointsSF, inf=100, asDF=F), ncol=3)
   
   model$IntervalsTP<-IntervalsTP
   model$IntervalsTF<-IntervalsTF
   model$IntervalsSP<-IntervalsSP
   model$IntervalsSPi<-IntervalsSPi
   model$IntervalsSF<-IntervalsSF
   
   model$sample_path <- sample_path
   
   return(model)
}

####################################################################

#' Initialize the parameters for the DLM used for the weight (DLMW), pig price (DLMP) and feed price (DLMF).
#' We assume the FF and GG matrices of these DLMs are time varying. 
#' 
#' @param dlmPig DLM related to the carcass price
#' @param dlmFeed DLM related to the feed price
#' @param dlmPiglet DLM related to the piglet price
#' 
#' @return A list containing all the parameters for three DLMs: DLMW, DLMP, DLMF
#' @export
#' @author Reza Pourmoayed \email{rpourmoayed@@econ.au.dk} 
paramDLMs<-function(dlmPig, dlmFeed, dlmPiglet){
 
   paramDLMPigPrice<-list()
   paramDLMPigletPrice<-list()
   paramDLMFeedPrice<-list()   
   pricePigFF<-list()
   pricePigGG<-list()
   pricePigletFF<-list()
   pricePigletGG<-list()
   priceFeedFF<-list()
   priceFeedGG<-list()
   
   pricePigDimObs<-20   # number of observations in the DLM
   pricePigletDimObs<-20
   priceFeedDimObs<-20
 
   for(i in 1:pricePigDimObs){
      pricePigFF[[i]]<- t(dlmPig$FF)
      pricePigGG[[i]]<-dlmPig$GG
   }
   
   for(i in 1:pricePigletDimObs){
     pricePigletFF[[i]]<-t(dlmPiglet$FF)
     pricePigletGG[[i]]<-dlmPiglet$GG
   }
   
   for(i in 1:priceFeedDimObs){
      priceFeedFF[[i]]<-t(dlmFeed$FF)
      priceFeedGG[[i]]<-dlmFeed$GG
   }
   
   
   paramDLMPigPrice$FF<-pricePigFF
   paramDLMPigPrice$GG<-pricePigGG
   paramDLMPigPrice$V<-dlmPig$V
   paramDLMPigPrice$W<-dlmPig$W
   paramDLMPigPrice$m0<-dlmPig$m0
   paramDLMPigPrice$C0<-dlmPig$C0
   paramDLMPigPrice$dimObs<-pricePigDimObs
   
   paramDLMPigletPrice$FF<-pricePigletFF
   paramDLMPigletPrice$GG<-pricePigletGG
   paramDLMPigletPrice$V<-dlmPiglet$V
   paramDLMPigletPrice$W<-dlmPiglet$W
   paramDLMPigletPrice$m0<-dlmPiglet$m0
   paramDLMPigletPrice$C0<-dlmPiglet$C0
   paramDLMPigletPrice$dimObs<-pricePigletDimObs
   
   
   paramDLMFeedPrice$FF<-priceFeedFF
   paramDLMFeedPrice$GG<-priceFeedGG
   paramDLMFeedPrice$V<-dlmFeed$V
   paramDLMFeedPrice$W<-dlmFeed$W
   paramDLMFeedPrice$m0<-dlmFeed$m0
   paramDLMFeedPrice$C0<-dlmFeed$C0
   paramDLMFeedPrice$dimObs<-priceFeedDimObs
   
   return(list(DLMP=paramDLMPigPrice, DLMPi=paramDLMPigletPrice, DLMF=paramDLMFeedPrice) )
}

####################################################################
#' A function to retun the required covariance matrices to compute the transition probabilities of the HMDP. 
#' 
#' @param iniDLM A list that contains the initial parameters of the related DLM.
#' @param param A list including the parameters of the HMDP model.
#' @param Y A list included the observations for the related DLM. 
#' 
#' @return A list containing covariance matrices used in file dlm.h to compute the transition probabilities.  
#' @export
#' @author Reza Pourmoayed \email{rpourmoayed@@econ.au.dk}  
buildDLM<-function(iniDLM,param,Y){
   
   m<-vector("list", iniDLM$dimObs)    # means of posterior - preallocate an empty list of length tMax
   C<-vector("list", iniDLM$dimObs)    # covariance matrices of posterior 
   L<-vector("list", iniDLM$dimObs)    # covariance matrices of transition probabilities for latent variables of DLMW, DLMP and DLMF for one ahead forecast. 
   Q<-vector("list", iniDLM$dimObs)    # covariance matrices of transition probabilities for observable variables of DLMW, DLMP and DLMF.
   L1<-matrix()                        # covariance matrices of transition probabilities for latent variables of DLMW, DLMP and DLMF for tStartMarketing ahead forecast (just for stage 1 of the second level of HMDP).
   Gk1<-matrix()                       # coeficiant matrix to compute the mean parameter of the related forecast distribution.   
   L2<-vector("list", param$tMax)      # covariance matrices of transition probabilities for latent variables of DLMW, DLMP and DLMF for max(pigletLeadTime,cleaningPeriod) ahead forecast.
   Gk2<-vector("list", param$tMax)     # coeficiant matrix to compute the mean parameter of the related forecast distribution.
   L3<-vector("list", param$tMax)      # covariance matrices of transition probabilities for observable variables of DLMW, DLMP and DLMF for max(pigletLeadTime,cleaningPeriod + marketingLength) ahead forecast.
   Gk3<-vector("list", param$tMax)     # coeficiant matrix to compute the mean parameter of the related forecast distribution.
   A<-vector("list", iniDLM$dimObs)    # a Matrix defined in the steps of DLM.
   
   
   for(t in 1:iniDLM$dimObs){
      #Prior
      if(t==1){
         a<-iniDLM$GG[[1]] %*% iniDLM$m0
         R<-iniDLM$GG[[1]] %*% iniDLM$C0 %*% t(iniDLM$GG[[1]]) + iniDLM$W
      }
      else{
         a<-iniDLM$GG[[t]] %*% m[[t-1]] 
         R <-iniDLM$GG[[t]] %*% C[[t-1]]  %*% t(iniDLM$GG[[t]]) + iniDLM$W
      }
      # One step forcast
      f<-t(iniDLM$FF[[t]]) %*% a
      Q[[t]]<-t(iniDLM$FF[[t]]) %*% R %*% iniDLM$FF[[t]] + iniDLM$V
      #Posterior (we see Y_t here)
      A[[t]]<-R %*% iniDLM$FF[[t]] %*% solve(Q[[t]])
      m[[t]] <-a + A[[t]] %*% (Y[,t]-f)   
      C[[t]] <-R  - A[[t]] %*% Q[[t]] %*% t(A[[t]]) 
      #Compute the covariance matrix for transition probabilities
      L[[t]] <-R-C[[t]] 
   }
       
   k1<-param$tStartMarketing-1 # (t^min -1) k1 ahead forecast of latent variables for the weight information    
   k2<-1 # k2 ahead forecast of latent variables the pig price information (sum of cleaning and preparation of delivery is 1 )
   
   # k1 ahead forecast parameters for latent veriables 
   R = iniDLM$C0   
   t=0 # since in the statndard SSM the initial posterior is defined at t=0
   G = diag( dim(R)[1] ) #iniDLM$GG[[t+1]]
      for(h in 1:k1 ){
          R = iniDLM$GG[[t+h]] %*% R %*% t(iniDLM$GG[[t+h]]) + iniDLM$W
          G = G %*% iniDLM$GG[[t+h]]
      }
      Qk = t(iniDLM$FF[[t+k1]]) %*% R %*% iniDLM$FF[[t+k1]] +  iniDLM$V
      Ak = R %*% iniDLM$FF[[t+k1]] %*% solve(Qk) 
      L1 = Ak %*% Qk %*% t(Ak)   
      Gk1 = G
  
   # k2 ahead forecast parameters for latent veriables
   for(t in 1:param$tMax){
      R = C[[t]]
      G = diag( dim(R)[1] ) #iniDLM$GG[[t+1]]
      for(h in 1:k2 ){
         R = iniDLM$GG[[t+h]] %*% R %*% t(iniDLM$GG[[t+h]]) + iniDLM$W
         G = G %*% iniDLM$GG[[t+h]]
      }
      Qk = t(iniDLM$FF[[t+k2]]) %*% R %*% iniDLM$FF[[t+k2]] +  iniDLM$V
      Ak = R %*% iniDLM$FF[[t+k2]] %*% solve(Qk)
      L2[[t]] = Ak %*% Qk %*% t(Ak)
      Gk2[[t]] = G
   }

   
   # k2 ahead forecast parameters for observable veriables
   for(t in 1:param$tMax){
      R = C[[t]]
      G = diag( dim(R)[1] ) #iniDLM$GG[[t+1]]
      for(h in 1:k2 ){
         R = iniDLM$GG[[t+h]] %*% R %*% t(iniDLM$GG[[t+h]]) + iniDLM$W
         G = G %*% iniDLM$GG[[t+h]]
      }
      L3[[t]] = t(iniDLM$FF[[t+k2]]) %*% R %*% iniDLM$FF[[t+k2]] +  iniDLM$V
      Gk3[[t]] = t(iniDLM$FF[[t+k2]]) %*% G
   }
         
   iniDLM$L<-L
   iniDLM$L1<-L1
   iniDLM$L2<-L2
   iniDLM$L3<-L3
   iniDLM$Gk1<-Gk1
   iniDLM$Gk2<-Gk2
   iniDLM$Gk3<-Gk3
   iniDLM$tMax<-param$tMax
   return(iniDLM)
}

####################################################################

#' A function to find the id of a state in the discretized intervals. 
#' 
#' @param st Given value of state.
#' @param matDis A matrix including the discretized intervals of the related stae. 
#' 
#' @return State id in matDis.   
#' @export
#' @author Reza Pourmoayed \email{rpourmoayed@@econ.au.dk}  

findIndex<-function(st,matDis){
  hh<--1
  for(i in 1:dim(matDis)[1]){
    if( ( st>=matDis[i,2] ) & ( st<matDis[i,3] )  )
      hh<-i-1;
  }
  return(hh);
}
   
   



