# TODO: Add comment
# 
# Author: fec
###############################################################################


library(R6)

Model <- R6Class("Model",
    public = list(
        initialize = function() {
          private$formulaFields <- c()
          private$nuOfFeatures <- 0
          self$setFormulaBase()
        },
        trainModel = function(trainData) {
          private$model  <- glm(as.formula(self$getFormulaString()), family = binomial(), data = trainData)
        },
        evaluate = function(data) {
          prediction <- predict(private$model, data, type = "response")
          modelAccuracy <- confusionMatrix(factor(round(prediction), levels = 0:1), factor(data$status, levels = 0:1))
          modelACC <- modelAccuracy[["byClass"]][["Balanced Accuracy"]]
        },
        predict = function(data, predictionType = "response") {
          prediction <- predict(private$model, data, type = predictionType)
          return(prediction)
        },
        comparePerformanceValues = function(perfValue1, perfValue2) {#returns TRUE if first value is better, otherwise FALSE
          if (perfValue1 > perfValue2) {
            return(TRUE)
          }
          return(FALSE)
        },
        addFieldToFormula = function(fieldName) {
          private$formulaFields <- append(private$formulaFields, fieldName)
          length(private$formulaFields)
        },
        getNumberOfFields = function() {
          length(private$formulaFields)
        },
        popFieldFromFormula = function() {
          private$formulaFields <- private$formulaFields[1:length(private$formulaFields)-1]
          length(private$formulaFields)
        },
        setFormulaBase = function() {
          private$formulaBase <- "status"
        },
        bindData = function(outcome, features) {
          return(data.frame(cbind("status" = outcome$status, features)))
        },
        getFormulaString = function() {
          fields <- paste(private$formulaFields, collapse = "+")
          paste(private$formulaBase, "~", fields)
        },
        getPValuesOfFittedModel = function() {
          modelSummaryCoefs <- coef(summary(private$model))
          modelSummaryCoefs[2:nrow(modelSummaryCoefs), 4]
        },
        getModelParamNames = function() {
          paste(sort(private$formulaFields), collapse = "+")
        },
        printModelSummary = function() {
          print(summary(private$model))
        },
        messageModelSummary = function() {
          message(summary(private$model))
        }
    ),
    private = list(
        model = NULL,
        formulaBase = NULL,
        formulaFields = NULL,
        nuOfFeatures = NULL
    )
)

CoxModel <- R6Class("CoxModel",
    inherit = Model,
    public = list(
        evaluate = function(data) {
          cIndex <- concordance(private$model, newdata = data)
          modelACC <- cIndex$concordance
        },
        predict = function(data, predictionType = "risk") {
          prediction <- super$predict(data, predictionType)
          return(prediction)
        },
        trainModel = function(trainData) {
          private$model <- coxph(as.formula(self$getFormulaString()), data = trainData)
        },
        setFormulaBase = function() {
          private$formulaBase <- "Surv(time,status)"
        },
        bindData = function(outcome, features) {
          return(data.frame(cbind("time" = outcome$time, "status" = outcome$status, features)))
        },
        getPValuesOfFittedModel = function() {
          modelSummaryCoefs <- coef(summary(private$model))
          modelSummaryCoefs[1:nrow(modelSummaryCoefs), 5]
        },
        getSurvFit = function(data) {
          return(survfit(private$model, newdata=data))
        }
    ),

)
