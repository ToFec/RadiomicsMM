# TODO: Add comment
# 
# Author: fec
###############################################################################

FeatureReductionContainerProvider <- R6Class("FeatureReductionContainerProvider",
    public = list(
        volumeColName = NULL,
        initialize = function() {
        },
        radiomicsFeatureEliminationRules = function(outcomeTrainSet, radiomicFeaturesTrainSets) {
          ## define Rules to apply
          centerAgnosticRule <- CenterAgnosticFeatures$new(0.001)
          
          keepFeaturesNoVolumeCorrelation <- RemoveFeaturesThatCorrelateWithVolume$new()
          keepFeaturesNoVolumeCorrelation$setVolumeColName(self$volumeColName)
          keepComparableFeatureRule <- KeepComparableFeatures$new(0.05, radiomicFeaturesTrainSets[-1])
          normalizer <- NormalizeFeaturesrule$new()
          univariateFeatureReduction <- UnivariateFeatureReductionRule$new()
          
          clusterAnalysis <- ClusterFeaturesWithPreComputedFeature$new()
          clusterAnalysis$setPrecomputedFeaturesProvider(univariateFeatureReduction$getCalculatedFeatures)
          
          featReductionContainer <- FeatureReductionContainer$new(outcomeTrainSet, radiomicFeaturesTrainSets[[1]])
          
          featReductionContainer$addRule(keepComparableFeatureRule)
          featReductionContainer$addRule(normalizer)
          
          featReductionContainer$addRule(univariateFeatureReduction)
          featReductionContainer$addRule(clusterAnalysis)
          
          return(featReductionContainer)
          
        },
        clinicalFeatureEliminationRules = function(outcomeTrainSet, clinicalTrainSet, clinicalTestSet) {
          normalizer <- NormalizeFeaturesrule$new()
          univariateFeatureReduction <- UnivariateFeatureReductionRule$new()
          
          featReductionContainer <- FeatureReductionContainer$new(outcomeTrainSet, clinicalTrainSet)
          featReductionContainer$addRule(normalizer)
          featReductionContainer$addRule(univariateFeatureReduction)
          return(featReductionContainer)
        }
    )
)



