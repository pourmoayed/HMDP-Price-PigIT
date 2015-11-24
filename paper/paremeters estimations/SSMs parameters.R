
library(data.table)
require(dlm) 

# Estimate the DLMs parameters: 

# Read the price data form data base
#PriceData = read.table(file=file.choose(),header=T,sep=";")
PriceData = read.table(file="price data from 2005.csv",header=T,sep=";")
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
#exp(Param$par)

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
#exp(Param$par)

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
#exp(Param$par)

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
