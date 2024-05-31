# TODO: Add comment
# 
# Author: fec
###############################################################################


library(R6)
source("Outcome.R")

MelanomeCSVParser <- R6Class("MelanomeCSVParser", 
		public = list(
				inputDataFrbgCSV = 'data/F_manualContours_anon.csv',
				inputDataUnetFrbgCSV = 'data/F_unetContours_anon.csv',
				inputDataAugmented1FrbgCSV = 'data/F_augmented1_anon.csv',
				inputDataAugmented2FrbgCSV = 'data/F_augmented2_anon.csv',
				inputDataKielCSV = 'data/K_manualContours_anon.csv',
				inputDataUnetKielCSV = 'data/K_unetContours_anon.csv',
				inputDataAugmented1KielCSV = 'data/K_augmented1_anon.csv',
				inputDataAugmented2KielCSV = 'data/K_augmented2_anon.csv',
				inputDataMingaCSV = 'data/M_manualContours_anon.csv',
				inputDataUnetMingaCSV = 'data/M_unetContours_anon.csv',
				inputDataAugmented1MingaCSV = 'data/M_augmented1_anon.csv',
				inputDataAugmented2MingaCSV = 'data/M_augmented2_anon.csv',
				

				
				getRadiomicFeatures = function(inputData) {
					featuresTumor <- inputData[,32:819]
					featuresMargin <- inputData[,825:1612]
					features <- cbind(featuresTumor,featuresMargin)
					return(features)
				},
        getRadiomcFeaturesAsList = function(inputData) {
          lapply(inputData, self$getRadiomicFeatures)
        },
        getDoseFeaturesForFirstDataSet = function(inputData) {
          return(self$getDoseFeatures(inputData[[1]]))
        },
				getDoseFeatures = function(inputData)
				{
					doseFeatureNames = c("Min.Gy.","Max.Gy.","Mean.Gy.","D50..Gy.","D98..Gy.","D2..Gy.")
					doseFeatures <- inputData[doseFeatureNames]
					return(doseFeatures)
				},
				getOutcome = function(inputData)
				{
					idsAll = private$getPatIds(inputData$patientName)
					timeAll <- inputData$mIfPD
					statusAll <- inputData$statusIfPD
          centerIds <- private$getCenter()
					
					outcome = Outcome$new(statusAll, idsAll, timeAll, centerIds)
				},
        getClinicalFeaturesForFirstDataSet = function(inputData) {
          return(self$getClinicalFeatures(inputData[[1]]))
        },
				getClinicalFeatures = function(inputData)
				{
					klinischeFeatures= c("ageClass","gender","kps","sysVor","sysWaehrend","sysDanach","molTarget", "gpams", "hmSumme","modalitaet")
					clinicalFeatures <- inputData[klinischeFeatures]
					
					clinicalFeatures$sysVor0 <- as.factor(ifelse(clinicalFeatures$sysVor == 0, 1, 0))
					clinicalFeatures$sysVor1 <- as.factor(ifelse(clinicalFeatures$sysVor == 1, 1, 0))
					clinicalFeatures$sysVor2 <- as.factor(ifelse(clinicalFeatures$sysVor >= 2, 1, 0))
					clinicalFeatures$sysVor <- NULL
					
					clinicalFeatures$sysWaehrend0 <- as.factor(ifelse(clinicalFeatures$sysWaehrend == 0, 1, 0))
					clinicalFeatures$sysWaehrend1 <- as.factor(ifelse(clinicalFeatures$sysWaehrend == 1, 1, 0))
					clinicalFeatures$sysWaehrend2 <- as.factor(ifelse(clinicalFeatures$sysWaehrend >= 2, 1, 0))
					clinicalFeatures$sysWaehrend <- NULL
					
					clinicalFeatures$sysDanach0 <- as.factor(ifelse(clinicalFeatures$sysDanach == 0, 1, 0))
					clinicalFeatures$sysDanach1 <- as.factor(ifelse(clinicalFeatures$sysDanach == 1, 1, 0))
					clinicalFeatures$sysDanach2 <- as.factor(ifelse(clinicalFeatures$sysDanach >= 2, 1, 0))
					clinicalFeatures$sysDanach <- NULL
					
					clinicalFeatures$modalitaet0 = as.factor(ifelse(clinicalFeatures$modalitaet == 0, 1, 0))
					clinicalFeatures$modalitaet1 = as.factor(ifelse(clinicalFeatures$modalitaet == 1, 1, 0))
					clinicalFeatures$modalitaet2 = as.factor(ifelse(clinicalFeatures$modalitaet >= 2, 1, 0))
					clinicalFeatures$modalitaet <- NULL
					
					clinicalFeatures$molTarget0 = as.factor(ifelse(clinicalFeatures$molTarget == 0, 1, 0))
					clinicalFeatures$molTarget1 = as.factor(ifelse(clinicalFeatures$molTarget == 1, 1, 0))
					clinicalFeatures$molTarget2 = as.factor(ifelse(clinicalFeatures$molTarget == 2, 1, 0))
					clinicalFeatures$molTarget <- NULL
					
					clinicalFeatures$gpams <- as.factor(ifelse(clinicalFeatures$gpams < 2.5, 0,1))
					
					clinicalFeatures$gender = as.factor(clinicalFeatures$gender)
					return(clinicalFeatures)
				},
				readCSVFiles = function()
				{
					inputDataFrbg <- read.csv(self$inputDataFrbgCSV)
					inputDataUnetFrbg <- read.csv(self$inputDataUnetFrbgCSV)
					inputDataAugmented1Frbg <- read.csv(self$inputDataAugmented1FrbgCSV)
					inputDataAugmented2Frbg <- read.csv(self$inputDataAugmented2FrbgCSV)
					
					inputDataKiel <- read.csv(self$inputDataKielCSV)
					inputDataUnetKiel <- read.csv(self$inputDataUnetKielCSV)
					inputDataAugmented1Kiel<- read.csv(self$inputDataAugmented1KielCSV)
					inputDataAugmented2Kiel <- read.csv(self$inputDataAugmented2KielCSV)
					
					inputDataMinga <- read.csv(self$inputDataMingaCSV)
					inputDataUnetMinga <- read.csv(self$inputDataUnetMingaCSV)
					inputDataAugmented1Minga <- read.csv(self$inputDataAugmented1MingaCSV)
					inputDataAugmented2Minga <- read.csv(self$inputDataAugmented2MingaCSV)
					
					inputData <- rbind(inputDataFrbg, inputDataKiel, inputDataMinga)
					inputDataUnet <- rbind(inputDataUnetFrbg, inputDataUnetKiel, inputDataUnetMinga)
					inputDataAugmented1 <- rbind(inputDataAugmented1Frbg, inputDataAugmented1Kiel, inputDataAugmented1Minga)
					inputDataAugmented2 <- rbind(inputDataAugmented2Frbg, inputDataAugmented2Kiel, inputDataAugmented2Minga)
					
          private$nFeaturesFrb <- nrow(inputDataFrbg)
          private$nFeaturesKiel <- nrow(inputDataKiel)
          private$nFeaturesMinga <- nrow(inputDataMinga)
          
					list(inputData, inputDataUnet, inputDataAugmented1, inputDataAugmented2 )
				}
		),
		private = list(
				getPatIds = function(patientNames)
				{
				patNames = patientNames
				patNames0 = patNames[1:private$nFeaturesFrb]
				patIds0 = unlist(patNames0)
				patNames1 = patNames[(private$nFeaturesFrb + 1):(private$nFeaturesFrb + private$nFeaturesKiel)]
				patIds1 = unlist(patNames1)
				patNames2 = patNames[(private$nFeaturesFrb + private$nFeaturesKiel + 1):(private$nFeaturesFrb + private$nFeaturesKiel + private$nFeaturesMinga)]
				patIds2 = unlist(lapply(patNames2,function(i) paste("M", strsplit(i,"_")[[1]][1],sep="")))
					ids = c(patIds0, patIds1, patIds2)
					return(ids)
				},
        getCenter = function()
        {
          centerIds <- c(rep(1,private$nFeaturesFrb), rep(2,private$nFeaturesKiel), rep(3,private$nFeaturesMinga))
          return(centerIds)
        },
        nFeaturesFrb = NULL,
        nFeaturesKiel = NULL,
        nFeaturesMinga = NULL
		)
)
