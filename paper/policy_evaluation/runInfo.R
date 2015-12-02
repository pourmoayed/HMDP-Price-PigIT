
## R script file to calculate value of price information given a specefic prrice setting . 
## Do the following
## 
## 1) Define the price setting 
## 2) Find the expected reward pr time unit of the optimal policy for the orginal model with price fluctuating  
## 3) Find the expected reward pr time unit of the modifed policy for the model defined for defined for the price setting of step 1.  
## 4) compute the VOI (value of price information)

# remember to set the working dir to ../paper/policy_evaluation
library(hmdpPricePigIT)

  message("Defind a price setting with specific values of pork and feed prices in variables porkPrice and feedPrice. Note that piglet price is automaticely calculated based on pork price.")
  porkPrice = 9.4
  feedPrice = 1.6  
 
  givenPolicy <- FALSE # Optimize without considering a given policy 
  source("optimizePolicy_hmdp.R")   # find optimal policy
  gOptimal<- g # expected rewrad per time unit of optimal policy 

  givenPolicy <- TRUE # Optimize based on a given policy 
  source("optimizePolicy_hmdp.R")
  
  gPolicy<- g # expected rewrad per time unit of a modifed policy based on price setting 
  
VOI = gOptimal - gPolicy;
VOI
