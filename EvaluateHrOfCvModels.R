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

file <- file(file.path(path, "featureInfo.txt"), "w")
sink(file, type = "m")
sink(file, type = "o")

bestModelFiles <- list.files(path, "bestModels.rds",  recursive=TRUE, full.names=TRUE, include.dirs=TRUE)
modelCoefficients <- c()
totalModels <- 0
for (bestModelFile in bestModelFiles) {
  bestModels <- readRDS(bestModelFile)
  message(bestModelFile)
  message(paste("Number of Models", length(bestModels)))
  totalModels <- totalModels + length(bestModels)
  for (i in seq_len(length(bestModels))) {
    model <- bestModels[[i]]
    coxModel <- model[[".__enclos_env__"]][["private"]][["model"]]
    coefNames <- names(coxModel$coefficients)
    hr <- exp(coxModel$coefficients)
    
    for (coefName in coefNames) {
      if (coefName %in% names(modelCoefficients)) {
        modelCoefficients[[coefName]] <- append(modelCoefficients[[coefName]],as.vector(hr[coefName]))
      } else {
        modelCoefficients[coefName] <- list(as.vector(hr[coefName]))
      }
    }
  }
}

message("############################")
message("############################")
message("############################")

for (coefName in names(modelCoefficients)) {
  hr <- modelCoefficients[[coefName]]
  message(paste(coefName, mean(hr), "+/-", sd(hr), "in", length(hr), "of", totalModels))
}

sink(file = NULL, type = "m")
sink(file = NULL, type = "o")
close(file)

