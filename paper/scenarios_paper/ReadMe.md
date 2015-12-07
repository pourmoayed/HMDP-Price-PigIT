# results of three scenarios used in the paper

This folder contains the data of three scenarios with different patterns of price fluctuation. The data and optimal actions for Scenarios 1,2 and 3 have been stored in the csv file "datPaperS1", "datPaperS2" and "datPaperS3", repectively.


##description of columns in the csv files

  t: week number from week 1 to week 15 in the related period.
  pricePig: observed pork price in the market at week t.
  priceFeed: observed feed price in the market at week t. 
  pricePiglet: observed piglet price in the market at week t.
  TP: trend of pork price (equal to pricePig).
  SP: pork price deviation from the price in the previous week obtained from
  the pork price SSM.
  TF: trend of feed price (equal to priceFeed)
  SF: Feed price deviation from the price in week 1 obtained from
  the feed price SSM.
  SPi: log ratio of piglet price deviation from the price in week 1 obtained
  from the piglet price SSM.
  meanWeight: average weight in week t obtained from the RRM.
  sdWeight: standard deviation of weight in week t obtained from the RRM.
  stage: stage number in the HMDP.
  optimal: Optimal markrting decision in week t.
  alive: number of pigs that has been remained in the pen at week t.
  scenario: scenario number.
  
