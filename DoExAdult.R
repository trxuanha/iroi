source("Utility.R")

baseFolder = getwd()
inputName <- 'adult'
outCome <- 'prof'
manAttributes = c('education_9', 'education_12', 'self_emp', 'gov','Private', 'hours_50', 'hours_30')
  
for(timecn in 1: 5){
  
  trainingFile<- paste (baseFolder, '/input/' ,inputName, '/', inputName, '_train_' ,timecn,'.csv',sep='')
  
  
  trainingData <-read.csv(file = trainingFile)
  
  trainingData <- dplyr::select (trainingData, -c('salary'))
  

  CDTmodel <- buildAdulttHeteEffectforCausalDTModel(trainingData)
  
  
  recUBCFFModel <- buildRecommendationModel(trainingData, 'UBCF', manAttributes, outCome, 200) ## neighbor = sqrt(number of instance)
  reIBCFFModel <- buildRecommendationModel(trainingData, 'IBCF', manAttributes, outCome, 200) ## neighbor = sqrt(number of instance)
  
  recPOPULARFModel <- buildRecommendationModel(trainingData, 'POPULAR', manAttributes, outCome)

  
  testFile <- paste (baseFolder, '/input/' ,inputName, '/', inputName, '_test_' ,timecn,'.csv',sep='')

  method = 'IROI'
  outputFolder <- paste (baseFolder, '/output/' ,method, '/',inputName, sep='')
  estimateUpLiftScore(CDTmodel,outCome, 'IROI', testFile, outputFolder, exceptAttrbute = c('salary'))
 
  method = 'UBCF'
  outputFolder <- paste (baseFolder, '/output/' ,method, '/',inputName, sep='') 
  estimateUpLiftScore(recUBCFFModel,outCome, 'UBCF', testFile, outputFolder, manAttributes, exceptAttrbute = c('salary'))

  method = 'IBCF'
  outputFolder <- paste (baseFolder, '/output/' ,method, '/',inputName, sep='')   
  estimateUpLiftScore(reIBCFFModel,outCome, 'IBCF', testFile, outputFolder, manAttributes, exceptAttrbute = c('salary'))
  
  method = 'IPOP'
  outputFolder <- paste (baseFolder, '/output/' ,method, '/',inputName, sep='')  
  estimateUpLiftScore(recPOPULARFModel,outCome, 'IPOP', testFile, outputFolder, manAttributes, exceptAttrbute = c('salary'))  
  
  testFile <- paste (baseFolder, '/input/' ,inputName, '/', inputName, '_train_' ,timecn,'.csv',sep='')
  
  method = 'IROI'
  outputFolder <- paste (baseFolder, '/output/' ,method, '/',inputName, sep='')
  estimateUpLiftScore(CDTmodel,outCome, 'IROI', testFile, outputFolder, exceptAttrbute = c('salary'))
 
  method = 'UBCF'
  outputFolder <- paste (baseFolder, '/output/' ,method, '/',inputName, sep='') 
  estimateUpLiftScore(recUBCFFModel,outCome, 'UBCF', testFile, outputFolder, manAttributes, exceptAttrbute = c('salary'))

  method = 'IBCF'
  outputFolder <- paste (baseFolder, '/output/' ,method, '/',inputName, sep='')   
  estimateUpLiftScore(reIBCFFModel,outCome, 'IBCF', testFile, outputFolder, manAttributes, exceptAttrbute = c('salary'))
  
  method = 'IPOP'
  outputFolder <- paste (baseFolder, '/output/' ,method, '/',inputName, sep='')  
  estimateUpLiftScore(recPOPULARFModel,outCome, 'IPOP', testFile, outputFolder, manAttributes, exceptAttrbute = c('salary')) 
  
  
}
