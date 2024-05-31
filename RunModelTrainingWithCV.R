#! /usr/bin/Rscript --vanilla

library(MuMIn)
library(survival)
library("survminer")

set.seed(7)

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

if (length(args) > 2) {
  path <- args[1]
  
  
  featuresToConsider <- NULL
  featuresToConsiderArg <- args[2]
  availableFeatures = c("radiomics", "clinic", "dose", "combined")
  if (featuresToConsiderArg %in% availableFeatures) {
    featuresToConsider <- featuresToConsiderArg
  }
  if (is.null(featuresToConsider)) {
    stop("Argment for features to use is not correct", call. = FALSE)
  }
  
  doNestedCV <- NULL
  doNestedCVArg = args[3]
  if (doNestedCVArg == "true") {
    doNestedCV <- TRUE
  } else if (doNestedCVArg == "false") {
    doNestedCV <- FALSE
  }
  if (is.null(doNestedCV)) {
    stop("Argment for nested CV is not correct", call. = FALSE)
  }
  
} else {
  stop("Wrong number of arguments.n", call. = FALSE)
}

melanomaNestedCrossValidation = function(settings) {
  
  dataList <- settings$csvParser$readCSVFiles()
  
  featureSets <- list()
  for (featureSetName in settings$featureSets) {
    parser <- settings$featureSetParserMapping[[featureSetName]]
    featureSets[[featureSetName]] <- parser(dataList)
  }
  
  outcome <- settings$csvParser$getOutcome(dataList[[1]])
  
  
  dataSplitter <- settings$getDataSplitter()
  
  variableTracker = TrackVariables$new(path)
  for (outerFoldNumber in seq_len(settings$nuOfOuterFolds)) {
    
    trainTestSetOutcome <- dataSplitter$sampleDataset(outcome)
    
    trainTestSetsSplits <- list()
    for (featureSetName in settings$featureSets) {
      trainTestSetsSplits[[featureSetName]] <- dataSplitter$getSubsetWithCurrentIdx(featureSets[[featureSetName]])
    }
    
    trainSets <- lapply(trainTestSetsSplits, `[[`, "trainSet")
    testSets <- lapply(trainTestSetsSplits, `[[`, "testSet")
    
    modelsPreprocessorsDatasets <- modelSelectionInnerLoop(trainTestSetOutcome$trainSet, settings, trainSets)
    selectedModel <- evaluateAllModelsOnAllValidationSets(modelsPreprocessorsDatasets$models, modelsPreprocessorsDatasets$featureReductioContainers, modelsPreprocessorsDatasets$testTrainSets)
    if (!is.null(selectedModel$model)) {
      trainModelOnNewData(selectedModel$model, selectedModel$preprocessor, trainTestSetOutcome$trainSet, trainSets)
      testAccuracy <- evaluateModelOnNewData(selectedModel$model, selectedModel$preprocessor, trainTestSetOutcome$testSet, testSets)
      
      
      variableTracker$trackVariable(testAccuracy)
      variableTracker$trackVariableWithName(selectedModel$model, "bestModels")
      variableTracker$trackVariableWithName(selectedModel$preprocessor, "preprocessors")
      variableTracker$trackVariableWithName(list(dataSplitter$getTestIdxs()), "testIdxs")
      variableTracker$trackVariableWithName(list(dataSplitter$getTrainIdxs()), "trainIdxs")
      variableTracker$trackVariableWithName(modelsPreprocessorsDatasets$testTrainSets, "trainTestSetsInnerLoop")
      
      variableTracker$saveTrackedVariablesToRds()
    }
    
  }  
}

melanomaCrossValidation = function(settings) {
  
  dataList <- settings$csvParser$readCSVFiles()
  
  featureSets <- list()
  for (featureSetName in settings$featureSets) {
    parser <- settings$featureSetParserMapping[[featureSetName]]
    featureSets[[featureSetName]] <- parser(dataList)
  }
  
  outcome <- settings$csvParser$getOutcome(dataList[[1]])
  
  modelsPreprocessorsDatasets <- modelSelectionInnerLoop(outcome, settings, featureSets)
  
  selectedModel <- evaluateAllModelsOnAllValidationSets(modelsPreprocessorsDatasets$models, modelsPreprocessorsDatasets$featureReductioContainers, modelsPreprocessorsDatasets$testTrainSets)
  trainModelOnNewData(selectedModel$model, selectedModel$preprocessor, outcome, featureSets)
  testAccuracy <- evaluateModelOnNewData(selectedModel$model, selectedModel$preprocessor, outcome, featureSets)
  variableTracker = TrackVariables$new(path)
  variableTracker$trackVariable(testAccuracy)
  variableTracker$trackVariableWithName(selectedModel$model, "bestModels")
  variableTracker$trackVariableWithName(selectedModel$preprocessor, "preprocessors")
  variableTracker$trackVariableWithName(modelsPreprocessorsDatasets$testTrainSets, "trainTestSetsInnerLoop")
  
  variableTracker$saveTrackedVariablesToRds()
  
}

melanomaTestOnExternalData = function(settings) {
  
  dataList <- settings$csvParser$readCSVFiles()
  
  featureSets <- list()
  for (featureSetName in settings$featureSets) {
    parser <- settings$featureSetParserMapping[[featureSetName]]
    featureSets[[featureSetName]] <- parser(dataList)
  }
  
  outcome <- settings$csvParser$getOutcome(dataList[[1]])
  
  
  dataSplitter <- settings$getDataSplitter()
  
  modelsPreprocessorsDatasets <- modelSelectionInnerLoop(outcome, settings, featureSets)
  selectedModel <- evaluateAllModelsOnAllValidationSets(modelsPreprocessorsDatasets$models, modelsPreprocessorsDatasets$featureReductioContainers, modelsPreprocessorsDatasets$testTrainSets)
  trainModelOnNewData(selectedModel$model, selectedModel$preprocessor, modelsPreprocessorsDatasets$testTrainSets[[1]]$outcomeTrain, modelsPreprocessorsDatasets$testTrainSets[[1]]$featuresTrain)
  testAccuracy <- evaluateModelOnNewData(selectedModel$model, selectedModel$preprocessor, modelsPreprocessorsDatasets$testTrainSets[[1]]$outcomeTest, modelsPreprocessorsDatasets$testTrainSets[[1]]$featuresTest)
  
  dataSplitter$sampleDataset(outcome)
  variableTracker = TrackVariables$new(path)
  variableTracker$trackVariable(testAccuracy)
  variableTracker$trackVariableWithName(selectedModel$model, "bestModels")
  variableTracker$trackVariableWithName(selectedModel$preprocessor, "preprocessors")
  variableTracker$trackVariableWithName(list(dataSplitter$getTestIdxs()), "testIdxs")
  variableTracker$trackVariableWithName(list(dataSplitter$getTrainIdxs()), "trainIdxs")
  
  variableTracker$saveTrackedVariablesToRds()
  
}

csvParser <- MelanomeCSVParser$new()
if (featuresToConsider == "radiomics") {
  settings <- MelanomeSettingsRadiomics$new(csvParser)
} else if (featuresToConsider == "clinic") {
  settings <- MelanomeSettingsClinical$new(csvParser)
} else if (featuresToConsider == "dose") {
  settings <- MelanomeSettingsDose$new(csvParser)
} else {
  settings <- MelanomeSettings$new(csvParser)
}

if (doNestedCV) {
  melanomaNestedCrossValidation(settings)
} else {
  melanomaCrossValidation(settings)
}

