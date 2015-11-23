\name{setParameters}
\alias{setParameters}
\title{Set the parameters used when build the HMDP model}
\usage{
setParameters(tMax = 12, tStartMarketing = 9, phases = 4, pigs = 21,
  rations = 4, rationCost = c(1.2, 1.7, 2.5, 2.8), minRationT = c(3, 3, 3,
  3), thresholds = seq(84, 116, by = 2), minMarketingSize = 3,
  minMarketingPr = 0.5, convRate = 0.84, convRateSd = 1.4, avgGRate = 6,
  avgLeanP = 0.6, CovWG = 8.66, avgInsWeight = 30,
  rationGrowthIndex = c(1, 3, 4, 6), centerPointsWeight = c(seq(30, 80, by =
  20), seq(82, 130, by = 2)), centerPointsGrowth = seq(4, 8, by = 0.5),
  centerPointsSd = seq(1, 20, by = 1))
}
\arguments{
  \item{tMax}{Maximum growth period in weeks.}

  \item{tStartMarketing}{Week where start to consider
  marketing.}

  \item{phases}{Maximum number of phases (possible changes
  in the ration).}

  \item{pigs}{Number of pigs in a pen at insertion.}

  \item{rations}{Number of possible rations.}

  \item{rationCost}{A vector with ration costs (per feed
  unit) of length rations.}

  \item{minRationT}{A vector with minimum number of weeks
  the ration must be used.}

  \item{thresholds}{A vector of thresholds (in kg) used by
  the actions. It must be increasing!}

  \item{minMarketingSize}{Desired minimum marketing group.}

  \item{minMarketingPr}{If probability of marketing a group
  of pigs not higher than \code{minMarketingSize} is below
  \code{minMarketingPr} for a given action we do not
  include the action.}

  \item{convRate}{Conversion rate between live weight and
  carcass weight [added by Reza]}

  \item{convRateSd}{Standard deviation of the conversion
  rate of the live weight and the carcass weight [added by
  Reza]}

  \item{avgGRate}{Average growth rate in the herd [added by
  Reza]}

  \item{avgLeanP}{Average leanness percent in the herd
  [added by Reza]}

  \item{CovWG}{Covariance between live weight and growth
  rate (we assume it is a constant value) [added by Reza]}

  \item{avgInsWeight}{Avarage weight of the pigs at the
  insertion time into the pen [added by Reza]}

  \item{centerPointsWeight}{Center points used in the
  discretization for the mean weight estimate.}

  \item{centerPointsGrowth}{Center points used in the
  discretization for the mean growth estimate.}

  \item{centerPointsSd}{Center points used in the
  discretization for the weight standard deviation
  estimate.}

  \item{rationGrowthIndex}{A vector including the index of
  relevant growth rate (in vector centerPointsGrowth) for
  each ration}
}
\value{
A list containing all the parameters
}
\description{
Set the parameters used when build the HMDP model
}
\author{
Reza Pourmoayed \email{rpourmoayed@econ.au.dk} and Lars
Relund \email{lars@relund.dk}
}

