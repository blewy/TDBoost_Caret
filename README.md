# TDBoost_Caret
Adapting TDBoost algorithm to be used with caret package.

In the Actuarial Field, specially on non-life insurance, the risk is usually model using a split aproach. 
Frequency and Average Cost are usually modelled separetely, for different reasons:

  - the factors that influence frequency are not necessarely the same for cost
  - the joint distribution of frequency*cost = burning_cost is not easily modeled
  
 hence some alternative aproaches are used, one of those is the tweady distribution, the package TDBoost uses a similar to GBM technique to 
 fit a model to the burning cost using the tweady distribution.
 
 On the doc folder I left a paper on the subject.
