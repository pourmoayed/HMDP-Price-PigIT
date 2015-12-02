# In this file the parameters of the RRM model are estimated using the RMLE method.
# In estimation the parameters, the real weight data of a pen is used. 
# The RMLE is implemented using package lme4 in R. 


 #read the weight data

  load("pigWeighings")
   weights<-list()
   batches = unique(pigWeighings$InsertionDate)  
   pens = unique(pigWeighings$PigPenName) 

  b= 7# 7 # 9  batch number in the database
  d= 28# 28 # 30 Pen number in the database
  weights<-list()
  
  batchPen = subset(pigWeighings, pigWeighings$InsertionDate == batches[b] & pigWeighings$PigPenName==pens[d])
  
  if (length(batchPen$PigId) > 0) {
    numPigs = unique(batchPen$PigId)
    for (p in 1:length(numPigs)) {
      thisPig = batchPen[batchPen$PigId == numPigs[p], ]
      weights[[p]]<-as.data.frame(thisPig)
    }
  }
  

#Check the length of weight observations are same or not. 
di<-c()
for (i in 1:length(weights) ){
  di[i] = length(weights[[i]]$ObservationTime) #di[i] =length(weights[[i]]$Tid)
}

# Define the data structure need to use lme4 package to estimate the parameters
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

# Fit a random quadratic regression model using lme4 package
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
  
#------------------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------------------




