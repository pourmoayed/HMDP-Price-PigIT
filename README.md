# A model for slaughter pig marketing under price fluctuations

This repository contains the R package `hmdpPricePigIT` which builds a hierarchical Markov decision process modelling marketing decision of growing/finishing pigs under price fluctuations.

The package is used to find the optimal policy in the paper "Slaughter pig marketing under price fluctuations"

To load the `R` package do 

```{r}
library(devtools)
install_github("relund/mdp")
install_github("relund/discretizeNormal/discretizeGaussian")
install_github("pourmoayed/hmdpPricePigIT", args="--no-multiarch")
```

To reproduce the results in the paper have a look at the ReadMe file in the subfolder `paper`.
