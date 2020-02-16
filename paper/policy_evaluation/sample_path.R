
library(hmdpPricePigIT)
library(data.table)
library(dplyr)
library(ggplot2)
library(grid)
library(tikzDevice)


#Estimate the RRM parameters
source("../paremeters_estimations/rrmParam.R", chdir = TRUE)

givenPolicy <- TRUE
sample_path <- TRUE 
full_size <- TRUE

if(full_size){
  # full model 
  param<-setParameters(tMax=15,
                       pigs=15,
                       tStartMarketing=9,
                       meanWeights = weightMean,
                       sdWeights = weightSd,
                       modPolicy = givenPolicy, # we set to TRUE, if HMPD should be solved based on a given policy.  
                       sample_path_given = sample_path,
                       centerPointsTP= round( seq(9.2,12.2,length=16), 2 ), #, round( seq(9.2,11.2,length=2), 2 ), #, 
                       centerPointsTF= round( seq(1.5,2.2,length=15), 2 ), # , round( seq(1.5,1.7,length=2), 2 ), #
                       centerPointsSP= round( seq(-0.4,0.4,length=5), 2 ), #must include 0
                       centerPointsSPi= round( seq(3.5,3.7,length=5), 2 ), #round( seq(3.5,3.8,length=3), 2 ),
                       centerPointsSF=round( seq(-0.1,0.1,length=5), 2), #round( seq(-0.1,0.1,length=9), 2)  # #must include 0
                       # iMTP = idTP, # index of prior for pork price (when modPolicy = T)
                       # iMTF = idTF # index of prior for feed price (when modPolicy = T)
  )
}else{
  # medium size model
  param<-setParameters(tMax=15,
                       pigs=15,
                       tStartMarketing=9,
                       meanWeights = weightMean,
                       sdWeights = weightSd,
                       modPolicy = givenPolicy, # we set to TRUE, if HMPD should be solved based on a given policy. 
                       sample_path_given = sample_path,
                       centerPointsTP = round( seq(9.2,12.2,length=4), 2 ),
                       centerPointsTF = round( seq(1.5,2.1,length=4), 2 ),
                       centerPointsSP = round( seq(-2,2,length=5), 2 ), #must include 0
                       centerPointsSPi = round( seq(3.5,3.7,length=5), 2 ), 
                       centerPointsSF = round( seq(-0.4,0.4,length=5), 2),   # #must include 0
                       # iMTP = idTP, # index of prior for pork price (when modPolicy = T)
                       # iMTF = idTF # index of prior for feed price (when modPolicy = T)
  )
}

#initial pork and feed prices
porkPrice = 9.4
feedPrice = 1.6
#find the index id of pork and feed prices: 
if(full_size){
  idTP = 2; #index of pork price (predefined values)
  idTF = 2; # index of feed price (predefined values) 
}else{
  idTP = 0; #index of pork price (predefined values)
  idTF = 1; # index of feed price (predefined values) 
}

if(givenPolicy){
  idTP = findIndex(porkPrice,param$IntervalsTP) # index of prior for pork price based on price setting (when modPolicy = T)
  idTF = findIndex(feedPrice,param$IntervalsTF) # index of prior for feed price based on price setting (when modPolicy = T)
}
param$iMTP <- idTP
param$iMTF <- idTF

#Estimate the SSMs parameters
source("../paremeters_estimations/ssmParam.R", chdir = TRUE)

#Estimate the HMDP reward parameters
source("../paremeters_estimations/hmdpRewardParam.R", chdir = TRUE)

#Read the policy
if(full_size){
  load("paramPolicy_full_model")  
}else{
  load("paramPolicy_medium_model")  
}

# Genetate the price sample paths (from states of the policy)
# generate the seed number 
set.seed(243565745)
seed_numbers <- runif(1000, 1232424, 33435322)

time_h <- (param$tMax-param$tStartMarketing +2)
price_sample_paths <- purrr::map(seed_numbers, .f = function(x){
  set.seed(x)
  iMTF <- sample(0:(length(param$centerPointsTF)-1), 1)
  df_policy <- paramPolicy[[iMTF+1]] %>% data.frame() %>% dplyr::select(-actionLabel,-n) %>% dplyr::distinct()
  dat <- plyr::ddply(df_policy, .variables = c("s"), .fun = function(df){
    if(any(duplicated(df)))
      stop("duplicated rows in df")
    dplyr::sample_n(df,1)
  } ) %>% 
    dplyr::select(
      stage = s, 
      iMTP = TP, 
      iMSP = SP, 
      iMTF = TF, 
      iMSF = SF, 
      iMSPi = SPi
    )
}) %>% setNames(seed_numbers)

g <- c()
policy <- list()

for(ii in 1:length(price_sample_paths)){
  
  cat("iteration number: ", ii, "\n")
  
  param$sample_path <- as.matrix(price_sample_paths[[ii]])
  
  #Build the HMDP
  prefix<-"main_"
  BuildHMDP2Deterministic(prefix, param, paramDLMP, paramDLMPi,  paramDLMF, paramPolicy)  
  
  #solve the HMDP:
  rm(mdp)
  wLbl<-"Reward"
  durLbl<-"Time"
  mdp<-loadMDP(prefix, check = F,verbose = F)
  g[ii]<-policyIteAve(mdp,wLbl,durLbl, maxIte = 5)     
  policy[[ii]]<-getPolicy(mdp)              
  if(param$modPolicy==T)
    do.call(file.remove,list(list.files(pattern = ".bin")))
}
###################################################################################3
# plot the results 

#g value of HMDP with SSMs:
if(full_size){
  g_opt_ssm <- 129.99
}else{
  g_opt_ssm <- 104.0657 
}


# We remove one outlier from the results of sample path (one extreme negative value). 
g_sample_paths <- g

plot_data <- data.frame(x=1:length(g_sample_paths), y=g_sample_paths)

tikz("plot_sample_path.tex", width = 10, height = 7, standAlone=F)
ggplot2::ggplot(plot_data, aes(x=x, y=y)) +
  ggplot2::geom_point() +
  xlab("Sample paths") + ylab("Expected reward per time unit(DKK)") +
  ggplot2::geom_hline(yintercept = g_opt_ssm, linetype="twodash") +
  theme_bw() +  
 theme(axis.text=element_text(size=12),
       axis.title.x=element_text(size=16),
       axis.title.y=element_text(size=16))
dev.off()
  
VOI <- g_opt_ssm - g_sample_paths
summary(VOI)
  
