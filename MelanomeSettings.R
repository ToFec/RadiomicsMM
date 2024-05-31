# TODO: Add comment
# 
# Author: fec
###############################################################################


library(R6)

MelanomeSettings <- R6Class("MelanomeSettings", 
    public = list(
      outerStartFold = 1,
      outerEndFold = 30,
      innerStartFold = 1,
      innerEndFold = 30,
      maxFeaturesInmodel = 4,
      baseModel = NULL,
      featureSetsAndReductionRules = NULL,
      featureSetParserMapping = NULL,
      csvParser = NULL,
      featureSets = NULL,
      featureDeterminationMethod = "recursive",##"exhaustive",
      volumeColName = "original_shape_MeshVolume",
      featureReductionContainerProvider = NULL,
      initialize = function(csvParser) {
        self$baseModel <- CoxModel$new()
        self$csvParser = csvParser
        self$featureSets <- c("radiomics", "clinic", "dose")
        self$featureReductionContainerProvider <- FeatureReductionContainerProvider$new()
        self$featureReductionContainerProvider$volumeColName <- self$volumeColName
        self$featureSetsAndReductionRules = list("radiomics" = self$featureReductionContainerProvider$radiomicsFeatureEliminationRules, "clinic" =  self$featureReductionContainerProvider$clinicalFeatureEliminationRules, "dose" = self$featureReductionContainerProvider$clinicalFeatureEliminationRules)
        self$featureSetParserMapping = list("radiomics" = csvParser$getRadiomcFeaturesAsList, "clinic" = csvParser$getClinicalFeaturesForFirstDataSet, "dose" = csvParser$getDoseFeaturesForFirstDataSet)
      },
      getDataSplitter = function() {
        dataSplitter <- DataSplitter$new()
        dataSplitter$setSampleFunction(dataSplitter$getIndexForTrainTestSetConsideringPatIds)
        return(dataSplitter)
      }
    
    ),
    active = list(
        nuOfOuterFolds = function(value) {
          if (missing(value)) {
            self$outerEndFold - self$outerStartFold + 1
          } else {
            stop("`$nuOfFolds` is read only", call. = FALSE)
          }
        },
        nuOfInnerFolds = function(value) {
          if (missing(value)) {
            self$innerEndFold - self$innerStartFold + 1
          } else {
            stop("`$nuOfFolds` is read only", call. = FALSE)
          }
        }
    ),
    private = list(
    )
)

MelanomeSettingsRadiomicsClinicSplitByCenter <- R6Class("MelanomeSettingsRadiomicsClinicSplitByCenter",
    inherit = MelanomeSettings,
    public = list(
        outerStartFold = 1,
        outerEndFold = 1,
        innerStartFold = 1,
        innerEndFold = 1,
        initialize = function(csvParser) {
          super$initialize(csvParser)
        },
        getDataSplitter = function() {
          dataSplitter <- DataSplitter$new()
          dataSplitter$setSampleFunction(dataSplitter$splitByCenter)
          return(dataSplitter)
        }
    )
)

MelanomeSettingsRadiomicsClinic <- R6Class("MelanomeSettingsRadiomicsClinic",
    inherit = MelanomeSettings,
    public = list(
        initialize = function(csvParser) {
          super$initialize(csvParser)
          self$featureSets <- c("radiomics", "clinic")
        }
    )
)

MelanomeSettingsRadiomicsDose <- R6Class("MelanomeSettingsRadiomicsDose",
    inherit = MelanomeSettings,
    public = list(
        initialize = function(csvParser) {
          super$initialize(csvParser)
          self$featureSets <- c("radiomics", "dose")
        }
    )
)

MelanomeSettingsClinicDose <- R6Class("MelanomeSettingsClinicDose",
    inherit = MelanomeSettings,
    public = list(
        initialize = function(csvParser) {
          super$initialize(csvParser)
          self$featureSets <- c("clinic", "dose")
        }
    )
)

MelanomeSettingsRadiomics <- R6Class("MelanomeSettingsRadiomics",
    inherit = MelanomeSettings,
    public = list(
        initialize = function(csvParser) {
          super$initialize(csvParser)
          self$featureSets <- c("radiomics")
        }
    )
)

MelanomeSettingsClinical <- R6Class("MelanomeSettingsClinical",
    inherit = MelanomeSettings,
    public = list(
        initialize = function(csvParser) {
          super$initialize(csvParser)
          self$featureSets <- c("clinic")
        }
    )
)

MelanomeSettingsDose <- R6Class("MelanomeSettingsDose",
    inherit = MelanomeSettings,
    public = list(
        initialize = function(csvParser) {
          super$initialize(csvParser)
          self$featureSets <- c("dose")
        }
    )
)
