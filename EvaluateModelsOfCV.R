#! /usr/bin/Rscript --vanilla

library(MuMIn)
library(survival)
library("survminer")


source("DataSplitter.R")
source("Outcome.R")
source("MelanomeCSVParser.R")
source("FeatureReduction.R")
source("Model.R")
source("PredefinedFeatureReductionRuleSequences.R")
source("MelanomeSettings.R")
source("ModelTrainer.R")
source("ModelSelectionInnerLoop.R")
source("TrackVariables.R")


args <- commandArgs(trailingOnly = TRUE)



if (length(args) > 0) {
  path <- args[1]
} else {
  stop("Path to models must be supplied.n", call. = FALSE)
}

file <- file(file.path(path, "info.txt"), "w")
sink(file, type = "m")
sink(file, type = "o")
testAccuracies <- readRDS(file.path(path, "testAccuracy.rds"))
trainIdxs <- NULL
if (file.exists(file.path(path, "trainIdxs.rds"))) {
  trainIdxs <- readRDS(file.path(path, "trainIdxs.rds"))
}
testIdxs <- NULL
if (file.exists(file.path(path, "testIdxs.rds"))) {
  testIdxs <- readRDS(file.path(path, "testIdxs.rds"))
}
innerLoopTrainTestIdxs <- NULL
if (file.exists(file.path(path, "trainTestSetsInnerLoop.rds"))) {
  innerLoopTrainTestIdxs <- readRDS(file.path(path, "trainTestSetsInnerLoop.rds"))
}

preprocessors <- readRDS(file.path(path, "preprocessors.rds"))
bestModels <- readRDS(file.path(path, "bestModels.rds"))

csvParser <- MelanomeCSVParser$new()
settings <- MelanomeSettings$new(csvParser)
features <- c()
for (feature in settings$featureSets) {
  if (feature %in% names(preprocessors)) {
    features <- append(features, feature)
  }
}
settings$featureSets <- features


nuOfFeatureClasses<-length(settings$featureSets)
dataList <- settings$csvParser$readCSVFiles()

featureSets <- list()
for (featureSetName in settings$featureSets) {
  parser <- settings$featureSetParserMapping[[featureSetName]]
  featureSets[[featureSetName]] <- parser(dataList)
}

outcome <- settings$csvParser$getOutcome(dataList[[1]])
message(path)
message(paste("mean tesetaccuracy:", mean(testAccuracies), "+/-", sd(testAccuracies)))
message(toString(testAccuracies))
dataSplitter <- settings$getDataSplitter()
for (i in seq_len(length(bestModels))) {
  message("############################")
  message("############################")
  message("############################")
  model <- bestModels[[i]]
  preprocessor <- preprocessors[(nuOfFeatureClasses * (i - 1) + 1):((nuOfFeatureClasses * (i - 1) + 1) + nuOfFeatureClasses - 1)]

  if (!is.null(testIdxs)) {
    testIdx <- testIdxs[[i]]
    trainIdx <- trainIdxs[[i]]
    dataSplitter$setTestIdxs(testIdx)
    dataSplitter$setTrainIdxs(trainIdx)
    
    outcomeSubsets <- dataSplitter$getOutcomeSubsetWithCurrentIdx(outcome)
    
    message(paste("Fold", i, "eventRatio in train set", sum(outcomeSubsets$trainSet$status) / length(outcomeSubsets$trainSet$status)))
    message(paste("Fold", i, "eventRatio in test set", sum(outcomeSubsets$testSet$status) / length(outcomeSubsets$testSet$status)))
    idsInSubset <- outcomeSubsets$testSet$ids[outcomeSubsets$testSet$status == 1]
    message(paste("Ids with event in test set: ", toString(idsInSubset)))
    
    
    
    trainTestSetsSplits <- list()
    for (featureSetName in settings$featureSets) {
      trainTestSetsSplits[[featureSetName]] <- dataSplitter$getSubsetWithCurrentIdx(featureSets[[featureSetName]])
    }
    
    trainSets <- lapply(trainTestSetsSplits, `[[`, "trainSet")
    testSets <- lapply(trainTestSetsSplits, `[[`, "testSet")
    
    trainModelOnNewData(model, preprocessor, outcomeSubsets$trainSet, trainSets)
    trainAccuracy <- evaluateModelOnNewData(model, preprocessor, outcomeSubsets$trainSet, trainSets)
    testAccuracy <- evaluateModelOnNewData(model, preprocessor, outcomeSubsets$testSet, testSets)
    predictions <- modelPredictionOnNewData(model, preprocessor, outcomeSubsets$testSet, testSets)
  } else {
    trainAccuracy <- evaluateModelOnNewData(model, preprocessor, outcome, featureSets)
    testAccuracy <- 0.0
  }
  
  message(paste("Model Fold", i))
  model$printModelSummary()
  
  trainAccuraciesInnerLoops <- c()
  testAccuraciesInnerLoops <- c()
  if (!is.null(innerLoopTrainTestIdxs)) {
    numberOfInnerLoops = length(innerLoopTrainTestIdxs) / length(bestModels)
    for (innerLoopNr in 1:numberOfInnerLoops) {
      innerLoopData = innerLoopTrainTestIdxs[[((i-1)*numberOfInnerLoops) + innerLoopNr]]
      if (!is.null(innerLoopData)) {
        trainModelOnNewData(model, preprocessor, innerLoopData$outcomeTrain, innerLoopData$featuresTrain)
        trainAccuracyInnerLoop <- evaluateModelOnNewData(model, preprocessor, innerLoopData$outcomeTrain, innerLoopData$featuresTrain)
        trainAccuraciesInnerLoops <- append(trainAccuraciesInnerLoops, trainAccuracyInnerLoop)
        testAccuracyInnerLoop <- evaluateModelOnNewData(model, preprocessor, innerLoopData$outcomeTest, innerLoopData$featuresTest)
        testAccuraciesInnerLoops  <- append(testAccuraciesInnerLoops, testAccuracyInnerLoop)
      }
      
    }
  }


  message(paste("Train accuracy:", trainAccuracy))
  message(paste("Test accuracy:", testAccuracy))
  
  message(paste("Train accuracy inner loops:", toString(trainAccuraciesInnerLoops)))
  message(paste("Test accuracy inner loops:", toString(testAccuraciesInnerLoops)))

}
sink(file = NULL, type = "m")
sink(file = NULL, type = "o")
close(file)

