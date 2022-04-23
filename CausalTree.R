
source("PropenUtil.R") 
library(causalTree)
library(dplyr)
library(rpart.utils)
library(rattle)

###############################
buildDisEmpHeteEffectforCausalDTModel<- function(trainingData){
  
  
  propsensityScores <- estimatePropensityforMOTIVATION(trainingData)
  tree1 <- causalTree(EMPLOYMENT~ . - ALLOWANCE
                      
                      , data = trainingData, treatment = trainingData$MOTIVATION,
                      split.Rule = "CT", 
                      cv.option = "CT", 
                      cv.Honest = T,
                      split.Honest = T, split.Bucket = T,
                      xval = 5, 
                      cp = 0, 
                      #minsize = 50, ##  minsize = 300,  for full datatse, 150 for training
                      minsize = 50,
                      #minsize = 220, ## for empployment
                      #weights = 1/propsensityScores,
                      propensity = propsensityScores,
                      cv.alpha = 1, 
                      split.alpha = 1)

	
  propsensityScores <- estimatePropensityforALLOWANCE(trainingData)	
  tree2 <- causalTree(EMPLOYMENT~ . - MOTIVATION
                      
                      , data = trainingData, treatment = trainingData$ALLOWANCE,
                      split.Rule = "CT", 
                      cv.option = "CT", 
                      cv.Honest = T,
                      split.Honest = T, split.Bucket = T,
                      xval = 5, 
                      cp = 0, 
                      #minsize = 50, ##  minsize = 300,  for full datatse, 150 for training
                      minsize = 50,
                      #minsize = 220, ## for empployment
                      #weights = 1/propsensityScores,
                      propensity = propsensityScores,
                      cv.alpha = 1, 
                      split.alpha = 1)
					  
					  
  
  

  results <- list()
  
  treeModel1<- list()
  treeModel1$model <- tree1
  treeModel1$factor <- 'MOTIVATION'
  
  treeModel2<- list()
  treeModel2$model <- tree2
  treeModel2$factor <- 'ALLOWANCE'
  
  
  results = c(results, list(treeModel1), list(treeModel2) )
  
  return (results)    

}

###############################
buildTurnOverHeteEffectforCausalDTModel<- function(trainingData){
  
  propsensityScores <- estimatePropensityforExtraversion(trainingData)
  
  tree1 <- causalTree(longTermCandidate~ . - greywage
                      
                      , data = trainingData, treatment = trainingData$extraversion,
                      split.Rule = "CT", 
                      cv.option = "CT", 
                      split.Honest = T, split.Bucket = F,
                      xval = 5, cp = 0, 
                      propensity = propsensityScores,
                      cv.alpha = 0.5, 
                      split.alpha = 1)
  
  propsensityScores <- estimatePropensityforGreywage(trainingData)
  
  tree2 <- causalTree(longTermCandidate~. - extraversion
                      
                      , data = trainingData, treatment = trainingData$greywage,
                      split.Rule = "CT", 
                      cv.option = "CT", 
                      split.Honest = T, 
                      xval = 5, cp = 0, 
                      cv.alpha = 0.5,
                      split.Bucket = T,
                      propensity = propsensityScores,
                      split.alpha = 1)
  
  
  
  results <- list()
  
  treeModel1<- list()
  treeModel1$model <- tree1
  treeModel1$factor <- 'extraversion'
  
  treeModel2<- list()
  treeModel2$model <- tree2
  treeModel2$factor <- 'greywage'
  
  
  results = c(results, list(treeModel1), list(treeModel2) )
  
  return (results)  
  
}


predictLargestCausalEffect <-function(models, recordForEstimate){
    
  prevLift = -9999
  treatmentName = 'NA'
  result = 0
  
  for(i in 1: length(models)){
    
    treeModel = models[[i]]
    result <- predict(treeModel$model, recordForEstimate)
    if(prevLift < result){
      prevLift <- result
      treatmentName <- treeModel$factor    
    }
    
  }
  
  return (list(prevLift, treatmentName))  
    
}

######################################



buildAdulttHeteEffectforCausalDTModel<- function(trainingData){
  
  
  dataDim <-dim (trainingData)

  
  
  propsensityScores <- estimatePropensityforEdu9(trainingData)
  
  tree1 <- causalTree(prof~ . - education_12 - self_emp - hours_30
                      
                      , data = trainingData, treatment = trainingData$education_9,
                      split.Rule = "CT", 
                      cv.option = "CT", 
                      split.Honest = T, split.Bucket = F, cv.Honest = T,
                      xval = 5, cp = 0, 
                     propensity = propsensityScores,
                      cv.alpha = 1, 
                      split.alpha = 1)
  
  
  
  propsensityScores <- estimatePropensityforEdu12(trainingData)
  
  tree2 <- causalTree(prof~ . - education_9 - self_emp - hours_30
                     
                     , data = trainingData, treatment = trainingData$education_12,
                     split.Rule = "CT", 
                     cv.option = "CT", 
                     split.Honest = T, split.Bucket = F, cv.Honest = T,
                     xval = 5, cp = 0, 
                     propensity = propsensityScores,
                     cv.alpha = 1, 
                     split.alpha = 1)
  

  tree3 <- causalTree(prof~ . - education_12 - education_9 - self_emp
                     
                     , data = trainingData, treatment = trainingData$hours_30,
                     split.Rule = "CT", 
                     cv.option = "CT", 
                     split.Honest = T, split.Bucket = F, cv.Honest = T,
                     xval = 5, cp = 0, 
                     propensity = propsensityScores,
                     cv.alpha = 1, 
                     split.alpha = 1)


  tree4 <- causalTree(prof~ . - education_12 - education_9 - hours_30
                     
                     , data = trainingData, treatment = trainingData$self_emp,
                     split.Rule = "CT", 
                     cv.option = "CT", 
                     split.Honest = T, split.Bucket = F, cv.Honest = T,
                     xval = 5, cp = 0, 
                     propensity = propsensityScores,
                     cv.alpha = 1, 
                     split.alpha = 1)



					 
  results <- list()
  
  
  treeModel1<- list()
  treeModel1$model <- tree1
  treeModel1$factor <- 'education_9'
  

  treeModel2<- list()
  treeModel2$model <- tree2
  treeModel2$factor <- 'education_12'    
  
  treeModel3<- list()
  treeModel3$model <- tree3
  treeModel3$factor <- 'hours_30'  


  treeModel4<- list()
  treeModel4$model <- tree4
  treeModel4$factor <- 'self_emp'  
  
  results = c(results, list(treeModel1), list(treeModel2), list(treeModel3), list(treeModel4) )
  
  
  
}
