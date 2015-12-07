## R script file for getting results used in the paper. 
## Do the following
## 
## 1) Find the optimal policy 
## 2) Create data for the 3 scenarios (stored in 3 csv files)
## 3) Plot the results (stored in pdf files)

# remember to set the working dir to this folder
setwd("./paper/")

library(hmdpPricePigIT)
useScenariosPaper <- FALSE   # use the scenarios used in the paper
if (!useScenariosPaper){
  message("Use new scenarios.")
  source("optimize_hmdp.R")   # find optimal policy
  message("Define a time period with length 15 weeks using vectors startTime and endTime such that year number is in the fisrt element and the week number is in the last element of the vectors")
  startTime <- c(2012,11) # specify the commencement time of the time period 
  endTime<- c(2012,25) # specify the end time of the time period
  source("scenario.R")        # simulate the 3 pens (use optimal policy to identify feed-mix and number of pigs)
}else{
  message("Use the scenarios and the optimal actions generated in the scenarios_paper folder.")
  datPaperS1 <- read.csv2("scenarios_paper/datPaperS1.csv")
  datPaperS2 <- read.csv2("scenarios_paper/datPaperS2.csv")
  datPaperS3 <- read.csv2("scenarios_paper/datPaperS3.csv")
}

source("plot.R", echo = TRUE)    # plot results as tex files

if (useScenariosPaper){
  tools::texi2pdf(file = "ScenariosPaper_plot.tex", clean = T)
}else{
  tools::texi2pdf(file = "Scenarios_plot.tex", clean = T)
}

