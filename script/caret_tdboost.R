library(tidyverse)
library(caret)
library(data.table)
library(TDboost)
library(xgboost)


data(FHT)

#take a look at gbm parameter in Caret
gbmModelInfo <- getModelInfo(model = "C5.0", regex = FALSE)[[1]]

gbmModelInfo$grid

## Model components
names(gbmModelInfo)


## ------- Seting model components for caret ---------

#Type of Model
lpTDBoost <- list(type = "Regression",
              library = "TDboost",
              loop = NULL) 

## Parameters
prm <- data.frame(parameter = c("n.trees","shrinkage","interaction.depth","bag.fraction","n.minobsinnode","alpha","distribuition"),
                  class = c(rep("numeric", 6),"character"),
                  label = c("n.trees","shrinkage","interaction.depth","bag.fraction","n.minobsinnode","alpha","distribuition"))

lpTDBoost$parameters <- prm

## Grid
TDBoostGrid <- function(x, y, len = NULL, search = "grid") {
  library(TDboost)
  ## This produces low, middle and high values for sigma 
  ## (i.e. a vector with 3 elements). 
  ## To use grid search:
  if(search == "grid") {
    out <- expand.grid(n.trees= if(len == 1)  1 else  c(1, 10*((2:min(len, 11)) - 1)),
                      shrinkage= c(0.001,0.01,0.05,0.1),
                      interaction.depth=  c(seq(1,sqrt(ncol(x)),sqrt(ncol(x))/len)),
                      bag.fraction= 0.5,
                      n.minobsinnode= 10,
                      alpha= c(1.01,1.3,1.6,2),
                      distribuition="EDM")
  } else {
    ## For random search, define ranges for the parameters then
    ## generate random values for them
    ## 
    out <- data.frame(n.trees=  sample(1:300, replace = TRUE, size = len),
                       shrinkage=  runif(len, min=0.001, max=1),
                       interaction.depth=  sample(1:sqrt(ncol(x)), replace = TRUE, size = len),
                       bag.fraction= rep(0.5,len),
                       n.minobsinnode= rep(10,len),
                       alpha= rbeta(len, 2, 5, ncp = 0)+1.01,
                       distribuition=rep("EDM",len) )
  }
  out
}

lpTDBoost$grid <- TDBoostGrid

## Fit
lpTDBoostFit <- function(x, y, wts, param, lev, last, weights, classProbs, ...) { 
  TDboost.fit(x=x,y=y,
              offset = NULL,
              misc = NULL,
              distribution = list(name=param$distribuition,alpha=param$alpha),
              w = NULL,
              var.monotone = NULL,
              n.trees = param$n.trees,
              interaction.depth = param$interaction.depth,
              n.minobsinnode = param$n.minobsinnode,
              shrinkage = param$shrinkage,
              bag.fraction = param$bag.fraction,
              train.fraction = 0.5,
              keep.data = TRUE,
              verbose = FALSE,
              var.names = NULL,
              response.name = NULL, ...)
}

lpTDBoost$fit <- lpTDBoostFit

## Prediction
lpTDBoostPred <- function(modelFit, newdata, preProc = NULL, submodels = NULL)
  predict(modelFit, newdata,modelFit$best.iter)

lpTDBoost$predict <- lpTDBoostPred

## Probability- no no this models does nor output Probabilities
lpTDBoostProb <- function(modelFit, newdata, preProc = NULL, submodels = NULL)
  NULL  

lpTDBoost$prob <- lpTDBoostProb

## Parameters Sort
lpTDBoostSort <- function(x) x[order(x$n.trees,x$shrinkage,x$interaction.depth),]
lpTDBoost$sort <- lpTDBoostSort

## Levsl Sort
lpTDBoost$levels <- function(x) lev(x)


# test the model

fitControl <- trainControl(method = "repeatedcv",
                           ## 10-fold CV...
                           number = 10,
                           ## repeated ten times
                           repeats = 1)

TDBoostrid<- expand.grid(n.trees=3000,
            shrinkage= 0.005,
            interaction.depth=  3,
            bag.fraction= 0.5,
            n.minobsinnode= 10,
            alpha= 1.5,
            distribuition="EDM")

set.seed(825)
TDBoosfit <- train(y=data1$Y,         # formula
               x=data1 %>% select(-Y),
               method =lpTDBoost, 
               tuneGrid = TDBoostrid,
               #tuneLength = 10,
               trControl = fitControl
               )
TDBoosfit$modelInfo

# plot(TDBoosfit)

caret.predict<- predict(TDBoosfit,data2 %>% select(-Y))

set.seed(825)
TDboost1 <- TDboost.fit(y=data1$Y,         # formula
                        x=data1 %>% select(-Y),                # dataset
                   # var.monotone=c(0,0,0,0,0,0), # -1: monotone decrease,
                    # +1: monotone increase,
                    #  0: no monotone restrictions
                    distribution=list(name="EDM",alpha=1.5),
                    # specify Tweedie index parameter
                    n.trees=3000,                # number of trees
                    shrinkage=0.005,             # shrinkage or learning rate,
                    # 0.001 to 0.1 usually work
                    interaction.depth=3,         # 1: additive model, 2: two-way interactions, etc.
                    bag.fraction = 0.5,          # subsampling fraction, 0.5 is probably best
                    train.fraction = 0.1,        # fraction of data for training,
                    # first train.fraction*N used for training
                    n.minobsinnode = 10,         # minimum total weight needed in each node
                    keep.data=TRUE,              # keep a copy of the dataset with the object
                    verbose=FALSE)                # print out progress

TDboost1$n.trees  

# print out the optimal iteration number M
best.iter <- TDboost.perf(TDboost1,method="test")
print(best.iter)

# check performance using 5-fold cross-validation
best.iter <- TDboost.perf(TDboost1,method="cv")
print(best.iter)

# plot the performance
# plot variable influence
summary(TDboost1,n.trees=1)         # based on the first tree
summary(TDboost1,n.trees=best.iter) # based on the estimated best number of trees

# making prediction on data2
f.predict <- predict.TDboost(TDboost1,data2,best.iter)


gap<- cbind(caret.predict,f.predict)

head(gap)
