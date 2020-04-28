## R script file to calculate value of price information using 1000 sample paths of prices. The HMDP is compared with two models: 
## Model I:  Future prices are known. That is, given a price sample path the decision maker optimizes decisions based on full information about future prices. 
## Model II: A rolling horizon approach where future prices are assumed the same as in the current epoch.  

## Do the following
## 
## 1) Choose Model I or II to be solved based on 1000 sample paths
## 2) Choose a full or medium size model to be solved.
## 3) Choose the optimal policy of hmdp should be calculated or not
## 4) Find the expected reward pr time unit (g value) of 1000 simulated sample paths 
## 5) Find the optimal policy of hmdp 
## 6) Plot distribution of g values for Model I and Model II with g value of optimal HMDP. 

## remember to set the working dir to ../paper/policy_evaluation
########################################################################################################################################################

library(hmdpPricePigIT)

# Model used for solving
chosen_model <- "Model II"

# size of the model
model_size <- "full"

# hmdp_optimal
solve_hmdp <- FALSE

if(chosen_model == "Model II"){
  rolling_horizon_model <- TRUE
}else{
  rolling_horizon_model <- FALSE
}

if(model_size == "full"){
  full_size <- TRUE
}else{
  full_size <- FALSE
}
source("policy_sample_path.R")   # find optimal policy


# solve hmdp optimally
if(solve_hmdp == TRUE){
  source("policy_hmdp.R")
  #Read the policy
  if(full_size){
    paramPolicy_full_model <- paramPolicy   
  }else{
    paramPolicy_medium_model <- paramPolicy 
  }
  g_opt_ssm <- g
}

if(solve_hmdp == FALSE){
  if(full_size){
    g_opt_ssm <- 129.99
  }else{
    g_opt_ssm <- 104.0657 
  }
}

# plot g values for Model I and II
source("plot.R")
