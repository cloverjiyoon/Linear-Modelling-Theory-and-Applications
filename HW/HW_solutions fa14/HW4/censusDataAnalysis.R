censusData <- read.table(file="HW_solutions/HW4/censusSmallTrain.txt",
                         header=TRUE)

# should probably drop first variable
# library(ggplot2)
# library(reshape2)
# 
# ggplot(data=melt(data=censusData[,c("logInc", catVars)], id.vars=c("logInc")),
#        aes(x=as.factor(value), y=logInc)) +
#   geom_boxplot() +
#   facet_wrap(facets=~variable, scales="free_x")


formatCensusData <- function(censusData) {
  
  censusData[, 1] <- log(censusData$inctot + 10000 + 1)
  names(censusData)[1] <- "logInc"
  
  # categorical variables:
  catVars <- c("sex", "hispan", "white", "black", "aian", "asian",
               "nhpi", "other", "race1", "marstat", "educ", "speak",
               "citizen", "abwork", "disable", "military", "milyrs",
               "esr", "lastwrk", "clwkr", "worklyr", "tenure",
               "hht", "fnf", "hhl", "empstat", "paoc", "parc",
               "p65", "wif")
  
  interactionVars <- c("sex", "educ")
  raceVars <- c("hispan", "white", "black", "aian", "asian")
  
  # probably want interactions between sex, race and educ.
  
  # remove disable and yr2us most likely...
  # numeric
  numVars <- c("logInc", "age", "pweight", "inctot", "persons",
               "p18", "npf", "noc", "nrc", "yr2us", "weeks")

  # unroll categorical variables:
  
  library(nnet)
  
  catData <- as.data.frame(sapply(X=catVars, function(varName) {
    class.ind(censusData[, varName])
  }))
  
  makeInteractionMat <- function(interactionVars, raceVars) {
    tmp <-lapply(interactionVars, function(interactionVar) {
      interDummyMat <- class.ind(censusData[, interactionVar])
      colnames(interDummyMat) <- paste0(interactionVar, ".", colnames(interDummyMat))
      lapply(raceVars, function(raceVar) {
        dummyMat <- class.ind(censusData[, raceVar])
        colnames(dummyMat) <- paste0(raceVar, ".", colnames(dummyMat))
        tmp <- lapply(colnames(interDummyMat), function(interLevel) {
          col1 <- interDummyMat[, interLevel]
          out <- data.frame(apply(dummyMat, 2, function(col2) col1 * col2))
          colnames(out) <- paste0(interLevel, ".x.", colnames(dummyMat))
          out
        })
        do.call(cbind, tmp)
      })
    })
    
    do.call(cbind, lapply(tmp, function(x) do.call(cbind, x)))
  }
  
  # create iteractions between sex, education, and race
  interactionMat <- makeInteractionMat(interactionVars, raceVars)
  interactionMat <- cbind(interactionMat,
                          makeInteractionMat("sex", "educ"))
  
  numData <- censusData[, numVars[-4]] #exclude inctot -- predict logInc instead
  
  cbind(numData, catData, interactionMat)
}

data <- formatCensusData(censusData)

# infer colinearity from lm
lmfit <- lm(logInc ~ ., data=data)
colinVars <- names(which(is.na(coefficients(lmfit))))

untangledData <- subset(data, select=!(colnames(data) %in% colinVars))

lmfit2 <- lm(logInc ~ ., data=untangledData)
summary(lmfit2) # no more NA's which is good

library(leaps)
backwardsStepwiseFit <- regsubsets(x=as.matrix(subset(untangledData, select=-logInc)),
                                  y=untangledData[, "logInc"],
                                  method=c("backward"),
                                  really.big = TRUE, nvmax=356)

stepwiseSummary <- summary(backwardsStepwiseFit)

stepwiseVars <- names(subset(untangledData, select=-logInc))[
                  stepwiseSummary$which[which.max(stepwiseSummary$adjr2),][-1]
                  ]

length(stepwiseVars) # 119
stepwiseFormula <- as.formula(paste0("logInc ~ ",
                                     paste(stepwiseVars, collapse="+")))

stepwiseModel <- lm(stepwiseFormula, untangledData)

summary(stepwiseModel)

# do lasso and ridge
library(glmnet)
library(doParallel)
registerDoParallel(cores=detectCores())

lassoFit <- cv.glmnet(x=as.matrix(subset(untangledData, select=-logInc)),
                      y=untangledData[, "logInc"],
                      alpha=1,
                      parallel=TRUE,
                      standardize=FALSE)

lassoVars <- rownames(coef(lassoFit, s=0))[
                  which(coef(lassoFit, s="lambda.min") != 0)][-1]

ridgeFit <- cv.glmnet(x=as.matrix(subset(untangledData, select=-logInc)),
                      y=untangledData[, "logInc"],
                      alpha=0,
                      parallel=TRUE,
                      standardize=FALSE)

ridgeVars <- rownames(coef(ridgeFit, s=0))[
               which(coef(ridgeFit, s="lambda.min") != 0)][-1]

# cross validate MSE of step-wise model:
source("HW_solutions/HW4/kFoldCV.R")

f <- function(data, newdata, k) {
  model <- lm(formula=stepwiseFormula, data=data)
  preds <- predict(model, newdata=newdata)
  (preds - newdata[,1])^2
}

stepwiseMSE <- kFoldCV(proc=f, k=10,
                       data=untangledData,
                       params=data.frame(k=1)) # 0.318

lassoMSE <- min(lassoFit$cvm) # 0.3518
ridgeMSE <- min(ridgeFit$cvm) # 0.3861

holdoutData <- read.table(file="HW_solutions/HW4/censusSmallTest.txt",
                          header=TRUE)

holdoutData <- formatCensusData(holdoutData)

# sanity check:
which(!(lassoVars %in% colnames(holdoutData) )) # 0

stepwiseCoefs <- coef(stepwiseModel)
stepwisePreds <- apply(subset(holdoutData, select=stepwiseVars), 1,
                       function(row) c(1,row) %*% stepwiseCoefs )

stepwiseMSE_holdout <- mean((holdoutData[, "logInc"] - stepwisePreds)^2) # 0.254

which(!(stepwiseVars %in% colnames(holdoutData) )) # 0

lassoCoefs <- coef(lassoFit,s="lambda.min")[c("(Intercept)",lassoVars),]
lassoPreds <- apply(subset(holdoutData, select=lassoVars), 1, function(row){
                c(1,row) %*% lassoCoefs
                })

lassoMSE_holdout <- mean((holdoutData[, "logInc"] - lassoPreds)^2) # 0.275

which((ridgeVars %in% colnames(holdoutData) )) # 15, 28, 78, 137!!

# relevant columns in holdoutData
ridgeCoefs <- coef(ridgeFit,s="lambda.min")[c(1,
                  which(c("Intercept",ridgeVars) %in% colnames(holdoutData))),] # 15, 28, 78, 137!!

ridgePreds <- apply(subset(holdoutData, select=names(ridgeCoefs[-1])), 1,
                    function(row) c(1,row) %*% ridgeCoefs )


ridgeMSE_holdout <- mean((holdoutData[, "logInc"] - ridgePreds)^2) # 0.308

results <- matrix(c(stepwiseMSE, stepwiseMSE_holdout,
                    lassoMSE, lassoMSE_holdout,
                    ridgeMSE, ridgeMSE_holdout),
                  byrow=TRUE, ncol=2)

colnames(results) <- c("CV MSE", "Holdout MSE")
rownames(results) <- c("stepwise", "lasso", "ridge")

print(results)