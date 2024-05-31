
evaluateAllModelsOnAllValidationSets <- function(models, featureReductioContainers, testTrainSetList) {
  bestPerformance <- 0
  bestModel <- NULL
  featurePreprocessor <- NULL
  for (i in seq_len(length(models))) {#run again over all folds to get a performance value for each validation set and each model
    modelCIndices <- c()
    rfFeatContainers <- featureReductioContainers[[i]]
    model <- models[[i]]
    for (testTrainSet in testTrainSetList) {
      processedTrainData <- NULL
      processedTestData <- NULL
      for (featureName in names(testTrainSet$featuresTrain)) {
        currFeaturesToProcessTrain <- testTrainSet$featuresTrain[[featureName]]
        currFeaturesToProcessTest <- testTrainSet$featuresTest[[featureName]]
        rfFeatContainer <- rfFeatContainers[[featureName]]
        rfTrainSet <- rfFeatContainer$applyRulesToTestData(testTrainSet$outcomeTrain, currFeaturesToProcessTrain)
        rfTestSet <- rfFeatContainer$applyRulesToTestData(testTrainSet$outcomeTest, currFeaturesToProcessTest)
        if (is.null(processedTrainData)) {
          processedTrainData <- rfTrainSet
          processedTestData <- rfTestSet
        } else {
          processedTrainData <- cbind(processedTrainData, rfTrainSet)
          processedTestData <- cbind(processedTestData, rfTestSet)
        }
      }
      
      if (length(processedTrainData) > 0) {
        trainData <- model$bindData(testTrainSet$outcomeTrain, processedTrainData)
        testData <- model$bindData(testTrainSet$outcomeTest, processedTestData)
        model$trainModel(trainData)
        cIndex <- model$evaluate(testData)
        
        modelCIndices <- append(modelCIndices, cIndex)
      }
    }
    meanPerformance <- mean(modelCIndices)
    if (model$comparePerformanceValues(meanPerformance, bestPerformance)) {
      bestPerformance <- meanPerformance
      bestModel <- model
      featurePreprocessor <- rfFeatContainers
    }
  }
  list("model" = bestModel, "preprocessor" = featurePreprocessor)
}

prepareNewData <- function(preprocessors, outcome, data) {
  processedTrainData <- NULL
  for (featureName in names(data)) {
    currDataToProcess <- data[[featureName]]
    if (is.null(names(currDataToProcess))) {
      currDataToProcess <- currDataToProcess[[1]]
    }
    preprocessor <- preprocessors[[featureName]]
    processedData <- preprocessor$applyRulesToTestData(outcome, currDataToProcess)
    if (is.null(processedTrainData)) {
      processedTrainData <- processedData
    } else {
      processedTrainData <- cbind(processedTrainData, processedData)
    }
  }
  return(processedTrainData)
}

trainModelOnNewData <- function(model, preprocessors, outcome, data) {
  processedTrainData <- prepareNewData(preprocessors, outcome, data)
  trainData <- model$bindData(outcome, processedTrainData)
  model$trainModel(trainData)
}

getSpecifFeatValues <- function(featName, preprocessors, outcome, data) {
  processedTrainData <- prepareNewData(preprocessors, outcome, data)
  processedTrainData[[featName]]
}

getAveragePatient <- function(preprocessors, outcome, data) {
  processedTrainData <- prepareNewData(preprocessors, outcome, data)
  
  allFeaturesMean = processedTrainData%>%summarise(across(where(is.numeric),mean))#allFeaturesTest%>%mutate_if(is.numeric,mean)
  allFeaturesMin = processedTrainData%>%summarise(across(where(is.factor),~as.factor(0)))
  averagePatient = cbind(allFeaturesMean, allFeaturesMin)
  return(averagePatient)
}

evaluateModelOnNewData <- function(model, preprocessors, outcome, data) {
  processedData <- prepareNewData(preprocessors, outcome, data)
  testData <- model$bindData(outcome, processedData)
  testAccuracy <- model$evaluate(testData)
  return(testAccuracy)
}

modelPredictionOnNewData <- function(model, preprocessors, outcome, data, predictionType = "risk") {
  processedData <- prepareNewData(preprocessors, outcome, data)
  testData <- model$bindData(outcome, processedData)
  testAccuracy <- model$predict(testData, predictionType)
  return(testAccuracy)
}

#radiomicFeatures, clinicalFeatures, doseFeatures
modelSelectionInnerLoop <- function(outcome, settings, trainSets) {
  testTrainSets <- list()
  models <- list()
  featureReductioContainers <- list()
  dataSplitter <- settings$getDataSplitter()
  for (foldNumber in seq_len(settings$nuOfInnerFolds)) {
    
    trainTestSetOutcome <- dataSplitter$sampleDataset(outcome)

    featReduction <- c()
    originalTrainSets <- c()
    originalTestSets <- c()
    processedTrainData <- NULL
    processedTestData <- NULL
    for(featureName in names(trainSets)) {
      featureSet = trainSets[[featureName]]
      featReductionRules <- settings$featureSetsAndReductionRules[[featureName]]
      trainTestSetFeatures <- dataSplitter$getSubsetWithCurrentIdx(featureSet)
      rfFeatContainer <- featReductionRules(trainTestSetOutcome$trainSet, trainTestSetFeatures$trainSet)
      
      if (is.null(names(trainTestSetFeatures$trainSet))) {
        trainFeatures <- trainTestSetFeatures$trainSet[[1]]
        testFeatures <- trainTestSetFeatures$testSet[[1]]
      } else {
        trainFeatures <- trainTestSetFeatures$trainSet
        testFeatures <- trainTestSetFeatures$testSet
      }
      originalTrainSets[[featureName]] <- trainFeatures
      originalTestSets[[featureName]] <-  testFeatures
      
      featReduction[[featureName]] <- rfFeatContainer
      if (is.null(processedTrainData)) {
        processedTrainData <- rfFeatContainer$applyRules()
        processedTestData <- rfFeatContainer$applyRulesToTestData(trainTestSetOutcome$testSet, testFeatures)
      } else {
        processedTrainData <- cbind(processedTrainData, rfFeatContainer$applyRules())
        processedTestData <- cbind(processedTestData, rfFeatContainer$applyRulesToTestData(trainTestSetOutcome$testSet, testFeatures))
      }
      
    }

    if (length(processedTrainData) > 0) {
      model <- settings$baseModel$clone(deep = TRUE)
      trainer <- ModelTrainer$new(model, trainTestSetOutcome$trainSet, processedTrainData, trainTestSetOutcome$testSet, processedTestData, settings$maxFeaturesInmodel, settings$featureDeterminationMethod)
      trainer$findBestFeatureCombination()
      if (model$getNumberOfFields() > 0) {
        featureReductioContainers[[toString(foldNumber)]] <- featReduction
        models[[toString(foldNumber)]] <- model
        testTrainSets[[foldNumber]] <- list("outcomeTrain" = trainTestSetOutcome$trainSet, "featuresTrain" = originalTrainSets, "outcomeTest" = trainTestSetOutcome$testSet, "featuresTest" = originalTestSets)
      }
    }
    

  }
  
  list("models" = models, "featureReductioContainers" = featureReductioContainers, "testTrainSets" = testTrainSets)
}

