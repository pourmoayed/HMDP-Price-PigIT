# #read the weight data
#   library(RODBC)
#   au=odbcConnect("PigIt",uid = "AU_RP", pwd = "PigIt4AU_RP")
#   
#   pigWeighings = sqlQuery(au, "SELECT PigId, PigWeight, ObservationTime, PigPenName, InsertionDate
#                     FROM PigIt.ManualWeighings, PigIt.IndividualPigs, PigIt.PigPen
#                     WHERE PenId = PigIt.PigPen.Id AND PigId = PigIt.IndividualPigs.Id
#                     ORDER BY InsertionDate, PigId, ObservationTime")
#   
#   save(pigWeighings,file = "data/pigWeighings")
  
  load("data/pigWeighings")
  
   weights<-list()
   
   batches = unique(pigWeighings$InsertionDate)  
   pens = unique(pigWeighings$PigPenName) 
  
#    zz<-array(NA, dim=c(length(batches),length(pens)) )
#    
#    for( b in 1: length(batches)){
#      for(d in 1:length(pens)){
#        batchPen = subset(pigWeighings, pigWeighings$InsertionDate == batches[b] & pigWeighings$PigPenName==pens[d])
#        if (length(batchPen$PigId) > 0){
#          weights<-list()
#          numPigs = unique(batchPen$PigId)
#          for (p in 1:length(numPigs)) {
#            thisPig = batchPen[batchPen$PigId == numPigs[p], ]
#            weights[[p]]<-as.data.frame(thisPig)
#          }
#          di<-c()
#          for (i in 1:length(weights) ){
#            di[i] = length(weights[[i]]$ObservationTime) #di[i] =length(weights[[i]]$Tid)
#          }        
#          countDi<-c()
#          repDi<-c()
#          repDi<-unique(di)
#          for(w in 1:length(repDi))
#            countDi[w]<-sum(di == repDi[w])
#          zz[b,d]<-max(countDi[w])
#        }    
#      }
#    }
#     

  b= 7# 7 # 9  batch number 
  d= 28# 28 # 30 Pen number 
  weights<-list()
  
  batchPen = subset(pigWeighings, pigWeighings$InsertionDate == batches[b] & pigWeighings$PigPenName==pens[d])
  
  if (length(batchPen$PigId) > 0) {
    numPigs = unique(batchPen$PigId)
    for (p in 1:length(numPigs)) {
      thisPig = batchPen[batchPen$PigId == numPigs[p], ]
      weights[[p]]<-as.data.frame(thisPig)
    }
  }
  
#Load data from the old data base:
  
# load("data/individualWeighings")
# 
# individualWeighings$Pig = as.integer(individualWeighings$GrisID)
# individualWeighings = individualWeighings[,-1]
# 
# #extract a list of weight data
# 
# batches = unique(individualWeighings$OmsaetnDato)
# 
# disps = unique(individualWeighings$VentilNr)
# 
# weights<-list()
# 
# b=4  #batch number            #for (b in 1:length(batches)) 
#   thisBatch = individualWeighings[individualWeighings$OmsaetnDato == batches[b], ]
# d=3  #disps number            #for (d in 1:length(disps)) 
#     batchPen = thisBatch[thisBatch$VentilNr == disps[d], ]
#     if (length(batchPen$Pig) > 0) {
#           numPigs = unique(batchPen$Pig)
#           for (p in 1:length(numPigs)) {
#             thisPig = batchPen[batchPen$Pig == numPigs[p], ]
#                weights[[p]]<-as.data.frame(thisPig)
#        }
#     }

# batches: 20120926 20130115 20130508 20130814 20131120
#disps: 2  8 16 23

#Check the length of weight observations are same or not. 
di<-c()
for (i in 1:length(weights) ){
  di[i] = length(weights[[i]]$ObservationTime) #di[i] =length(weights[[i]]$Tid)
}

# countDi<-c()  
# repDi<-unique(di)
#   for(w in 1:length(repeatDi))
#     countDi[w]<-sum(di == repDi[w])
# max(countDi[w])
   

#weights[which(di!=11)] <- NULL

numPigs <- length(weights) 
numWeeks <- length(weights[[1]]$PigWeight)

weighValue<-c()
c=1
for(i in 1:numPigs){
  for(j in 1:numWeeks){
    weighValue[c]<-weights[[i]]$PigWeight[j] # weighValue[c]<-weights[[i]]$Vaegt[j]
    c=c+1
  }
}

givenX<-c()
c=1
for(i in 1:numPigs){
  for(j in 1:numWeeks){
    givenX[c]<-j
    c=c+1
  }
}



test.df <- data.frame( Pig = sort(rep(c(1:numPigs),numWeeks)), Week = rep(c(1:numWeeks),numPigs), x=givenX, y=weighValue)

# Fit a random quadratic regression model
library(lme4)
re.lm <- lmer(y ~ poly(x,degree = 2,raw = TRUE) + ( poly(x,degree = 2,raw = TRUE)|Pig), data = test.df) 
summary(re.lm)

#estimate the weight variances from week 1 to week 15 
  
  B_0 = summary(re.lm)$coefficients[1,1] 
  B_1 = summary(re.lm)$coefficients[2,1] 
  B_2 = summary(re.lm)$coefficients[3,1] 

  vercovInfo<-as.data.frame(VarCorr(re.lm))
    
  var_error = vercovInfo$vcov[7] 
  
  v_00 = vercovInfo$vcov[1] 
  v_11 = vercovInfo$vcov[2]  
  v_22 = vercovInfo$vcov[3]  
  
  cov_10 = cov_01 = vercovInfo$vcov[4] 
  
  cov_20 = cov_02 = vercovInfo$vcov[5] 
  
  cov_21 = cov_12 = vercovInfo$vcov[6] 

varianceRRM<-matrix(c(v_00,cov_10,cov_20,cov_01,v_11,cov_21,cov_02,cov_12,v_22),ncol = 3)
meanRRM<-matrix(c(B_0,B_1,B_2),ncol = 1)
errorRRM<-matrix(c(var_error),ncol = 1)  
  
weightVar<-c()
weightSd<-c()

for(t in 1:20){  
  Z<-matrix(c(1,t,t^2),nrow = 1)  
  weightVar[t]<-Z %*% varianceRRM %*% t(Z) + var_error
  weightSd[t]<-sqrt(weightVar[t])
}

#estimate the weight means from week 1 to week 15

weightMean<-c()
for(t in 1:20){
  weightMean[t]<- B_0 + B_1*t + B_2*t^2
}

#estimate the growth mean and sd from week 1 to week 15
  
growthMean<-c()
growthSd<-c()

for(t in 1:(20-1)){
  growthMean[t]<-weightMean[t+1] - weightMean[t]   
  }
  
for(t in 1:(20-1)){
  Zt<-matrix(c(1,t,t^2),nrow = 1)
  ZtPlus<-matrix(c(1,t+1,(t+1)^2),nrow = 1)
    growthSd[t]<-sqrt( weightVar[t] + weightVar[t+1] -2*( ZtPlus %*% varianceRRM %*% t(Zt) + var_error ) )   
  }
  
  
  # # make a plot of the mean and the variance estimations
# time<-c(1:15)
# 
# plot(time[1:15],weightSd[1:15])
# plot(time[1:15],weightMean[1:15])

#------------------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------------------




