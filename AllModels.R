########## Random Forest ##########
RunModel_RandomForest=function(trainData,targetVarName, treeSize)
{
  #install.packages('randomForest')
  library(randomForest)
  model = randomForest(trainData[,targetVarName] ~ ., 
                       data = trainData ,
                       method='class',
                       importance = TRUE,
                       mtry = 6,
                       ntree = treeSize)
  
  return(model)
 
}

Predict_RandomForest=function(model,dataset)
{
  result=predict(model,dataset,type = 'response')
  return(result)
}

########## Decision Tree ##########

RunModel_DecisionTree=function(trainData,targetVarName)
{
  library(rpart)
  model = rpart(trainData[,targetVarName] ~ .,
                data = trainData,
                method='class',
                control = rpart.control(minsplit = 1))
  return(model)
}


########## Model Evaluation ##########
ModelEvalMeasure_classification = function(confusionMatrix)
{
  TN=confusionMatrix[1]
  FN=confusionMatrix[2]
  FP=confusionMatrix[3]
  TP=confusionMatrix[4]
  
  accuracy=(TN+TP)/(TN+FN+FP+TP)
  sensitivity=TP/(TP+FN)
  specificity=TN/(FP+TN)
  
  measure=data.frame(accuracy,sensitivity,specificity,TN,FN,FP,TP)
  return(measure)
}


