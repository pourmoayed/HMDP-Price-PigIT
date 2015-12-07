# Three scaenario are defined for time period  [startTime,endTime] (see run.R file) and the results are 
# stored in the data frames datS1, datS2, and datS3 
#
# Scenario 1 is based on the price date defined in time period  [startTime,endTime]
# In Scenario 2 the trend of feed price is inverted and the trends of pork and piglet prices are same with Scenario 1
# In Scenario 3 the trend of feed and pork prices are inverted and the trends of feed price is same with Scenario 1

datScenario1<-optimalSearch(param, scenarioNum=1, startTime=startTime, endTime=endTime,  finisher=finisher, feed=feed, piglet=piglet, 
                     dlmPig=dlmPig, dlmFeed=dlmFeed, dlmPiglet=dlmPiglet, policy=policy, mdp=mdp, wLbl=wLbl, durLbl=durLbl, g=g, usePig=F, useFeed=F, usePiglet=F)

datScenario2<-optimalSearch(param, scenarioNum=2, startTime=startTime, endTime=endTime,  finisher=finisher, feed=feed, piglet=piglet, 
                     dlmPig=dlmPig, dlmFeed=dlmFeed, dlmPiglet=dlmPiglet, policy=policy, mdp=mdp, wLbl=wLbl, durLbl=durLbl, g=g, usePig=F, useFeed=T, usePiglet=F)

datScenario3<-optimalSearch(param, scenarioNum=3, startTime=startTime, endTime=endTime,  finisher=finisher, feed=feed, piglet=piglet, 
                     dlmPig=dlmPig, dlmFeed=dlmFeed, dlmPiglet=dlmPiglet, policy=policy, mdp=mdp, wLbl=wLbl, durLbl=durLbl, g=g, usePig=T, useFeed=F, usePiglet=T)

if(param$modPolicy==F)
  do.call(file.remove,list(list.files(pattern = ".bin")))

# Store in the csv files

datScenario1 <- data.frame(lapply(datScenario1, as.numeric), stringsAsFactors=FALSE)
datScenario2 <- data.frame(lapply(datScenario2, as.numeric), stringsAsFactors=FALSE)
datScenario3 <- data.frame(lapply(datScenario3, as.numeric), stringsAsFactors=FALSE)

write.csv2(datScenario1, file = "datScenario1.csv",row.names=FALSE)
write.csv2(datScenario2, file = "datScenario2.csv",row.names=FALSE)
write.csv2(datScenario3, file = "datScenario3.csv",row.names=FALSE)