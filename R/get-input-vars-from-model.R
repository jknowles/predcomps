GetInputVarsFromModel <- function(model) UseMethod("GetInputVarsFromModel")

GetInputVarsFromModel.lm <- function(model) {
  # This seems like a robust way to get the input names from an lm object, but I'm not really sure
  return(attr(model$terms, "term.labels"))
}

GetInputVarsFromModel.glm <- function(model) {
  # This seems like a robust way to get the input names from an glm object, but I'm not really sure
  return(attr(model$terms, "term.labels"))
}

GetInputVarsFromModel.randomForest <- function(model) {
  return(attr(model$terms, "term.labels"))
}

GetInputVarsFromModel.train <- function(model) {
  # This seems like a robust way to get the input names from an train object
  return(names(model$trainingData)[-grep(".outcome",names(model$trainingData))])
  
}

# GetInputVarsFromModel.caretEnsemble <- function(model) {
#   # This seems like a robust way to get the input names from an glm object, but I'm not really sure
#   return(attr(model$terms, "term.labels"))
# }


