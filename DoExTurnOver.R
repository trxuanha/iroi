source("Utility.R")

baseFolder = getwd()
inputName <- 'turnover'
outCome <- 'longTermCandidate'
manAttributes = c('coach', 'greywage', 'extraversion', 'independ','selfcontrol', 'anxiety', 'novator','way')
  
for(timecn in 1: 5){
  
  trainingFile<- paste (baseFolder, '/input/' ,inputName, '/', inputName, '_train_' ,timecn,'.csv',sep='')
  
  
  trainingData <-read.csv(file = trainingFile)
  
  trainingData <- subset(trainingData, (!( (event == 0) & (longTermCandidate == 0) )))
  
  
  trainingData <- dplyr::select (trainingData, -c('stag', 'event'))

  CDTmodel <- buildTurnOverHeteEffectforCausalDTModel(trainingData)
  
  
  recUBCFFModel <- buildRecommendationModel(trainingData, 'UBCF', manAttributes, outCome, 20) ## neighbor = sqrt(number of instance)
  reIBCFFModel <- buildRecommendationModel(trainingData, 'IBCF', manAttributes, outCome, 20)
  recPOPULARFModel <- buildRecommendationModel(trainingData, 'POPULAR', manAttributes, outCome)

  
  testFile <- paste (baseFolder, '/input/' ,inputName, '/', inputName, '_test_' ,timecn,'.csv',sep='')

  method = 'IROI'
  outputFolder <- paste (baseFolder, '/output/' ,method, '/',inputName, sep='')
  estimateUpLiftScore(CDTmodel,outCome, 'IROI', testFile, outputFolder, exceptAttrbute = c('stag', 'event'))
 
  method = 'UBCF'
  outputFolder <- paste (baseFolder, '/output/' ,method, '/',inputName, sep='') 
  estimateUpLiftScore(recUBCFFModel,outCome, 'UBCF', testFile, outputFolder, manAttributes, exceptAttrbute = c('stag', 'event'))

  method = 'IBCF'
  outputFolder <- paste (baseFolder, '/output/' ,method, '/',inputName, sep='')   
  estimateUpLiftScore(reIBCFFModel,outCome, 'IBCF', testFile, outputFolder, manAttributes, exceptAttrbute = c('stag', 'event'))
  
  method = 'IPOP'
  outputFolder <- paste (baseFolder, '/output/' ,method, '/',inputName, sep='')  
  estimateUpLiftScore(recPOPULARFModel,outCome, 'IPOP', testFile, outputFolder, manAttributes, exceptAttrbute = c('stag', 'event'))  
  
  testFile <- paste (baseFolder, '/input/' ,inputName, '/', inputName, '_train_' ,timecn,'.csv',sep='')
  
  method = 'IROI'
  outputFolder <- paste (baseFolder, '/output/' ,method, '/',inputName, sep='')
  estimateUpLiftScore(CDTmodel,outCome, 'IROI', testFile, outputFolder, exceptAttrbute = c('stag', 'event'))
 
  method = 'UBCF'
  outputFolder <- paste (baseFolder, '/output/' ,method, '/',inputName, sep='') 
  estimateUpLiftScore(recUBCFFModel,outCome, 'UBCF', testFile, outputFolder, manAttributes, exceptAttrbute = c('stag', 'event'))

  method = 'IBCF'
  outputFolder <- paste (baseFolder, '/output/' ,method, '/',inputName, sep='')   
  estimateUpLiftScore(reIBCFFModel,outCome, 'IBCF', testFile, outputFolder, manAttributes, exceptAttrbute = c('stag', 'event'))
  
  method = 'IPOP'
  outputFolder <- paste (baseFolder, '/output/' ,method, '/',inputName, sep='')  
  estimateUpLiftScore(recPOPULARFModel,outCome, 'IPOP', testFile, outputFolder, manAttributes, exceptAttrbute = c('stag', 'event')) 
  
  
}
