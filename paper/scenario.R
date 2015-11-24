
# Three scaenario are defined for time period  [startTime,endTime] (see run.R file) and the results are 
#stored in the data frames datS1, datS2, and datS3 
#
# Scenario 1 is based on the price date defined in time period  [startTime,endTime]
# In Scenario 2 the trend of feed price is inverted and the trends of pork and piglet prices are same with Scenario 1
# In Scenario 3 the trend of feed and pork prices are inverted and the trends of feed price is same with Scenario 1

datS1<-optimalSearch(scenarioNum=1, startTime=startTime, endTime=endTime,  finisher=finisher, feed=feed, piglet=piglet, 
                     dlmPig=dlmPig, dlmFeed=dlmFeed, dlmPiglet=dlmPiglet, policy=policy, mdp=mdp, wLbl=wLbl, durLbl=durLbl, g=g, usePig=F, useFeed=F, usePiglet=F)

datS2<-optimalSearch(scenarioNum=2, startTime=startTime, endTime=endTime,  finisher=finisher, feed=feed, piglet=piglet, 
                     dlmPig=dlmPig, dlmFeed=dlmFeed, dlmPiglet=dlmPiglet, policy=policy, mdp=mdp, wLbl=wLbl, durLbl=durLbl, g=g, usePig=F, useFeed=T, usePiglet=F)

datS3<-optimalSearch(scenarioNum=3, startTime=startTime, endTime=endTime,  finisher=finisher, feed=feed, piglet=piglet, 
                     dlmPig=dlmPig, dlmFeed=dlmFeed, dlmPiglet=dlmPiglet, policy=policy, mdp=mdp, wLbl=wLbl, durLbl=durLbl, g=g, usePig=T, useFeed=F, usePiglet=T)

if(param$modPolicy==F)
  do.call(file.remove,list(list.files(pattern = ".bin")))

#Store in the csv files

datS1 <- data.frame(lapply(datS1, as.numeric), stringsAsFactors=FALSE)
datS2 <- data.frame(lapply(datS2, as.numeric), stringsAsFactors=FALSE)
datS3 <- data.frame(lapply(datS3, as.numeric), stringsAsFactors=FALSE)

write.csv2(datS1, file = "datS1.csv",row.names=FALSE)
write.csv2(datS2, file = "datS2.csv",row.names=FALSE)
write.csv2(datS3, file = "datS3.csv",row.names=FALSE)