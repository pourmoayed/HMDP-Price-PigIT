# Compute the reward values for the culled pigs and the feedintake for
# the remaining pigs in the pen.
set.seed(123456)
rewParam<-SimulatePigs(pigs=param$pigs, samples=10000, weeks=param$tMax, prices = param$centerPointsTP, V = round(varianceRRM,5) , B = meanRRM,  R = errorRRM)
param$feedWeek = rewParam$feed7
param$feedCull = rewParam$feed3
for(i in 1:length(param$centerPointsTP) )
  param$rewCull[[i]] <- rewParam$rew[,,i] 
