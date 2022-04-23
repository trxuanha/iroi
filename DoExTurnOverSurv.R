source("Utility.R")

baseFolder = getwd()
inputName <- 'turnover'
outCome <- 'longTermCandidate'
manAttributes = c('coach', 'greywage', 'extraversion', 'independ','selfcontrol', 'anxiety', 'novator','way')
  
  
trainingFile<- paste (baseFolder, '/input/' ,inputName, '/', inputName, '_full_1' ,'.csv',sep='')


trainingData <-read.csv(file = trainingFile)

trainingData <- subset(trainingData, (!( (event == 0) & (longTermCandidate == 0) )))


trainingData <- dplyr::select (trainingData, -c('stag', 'event'))

CDTmodel <- buildTurnOverHeteEffectforCausalDTModel(trainingData)


recUBCFFModel <- buildRecommendationModel(trainingData, 'UBCF', manAttributes, outCome)
reIBCFFModel <- buildRecommendationModel(trainingData, 'IBCF', manAttributes, outCome)
recPOPULARFModel <- buildRecommendationModel(trainingData, 'POPULAR', manAttributes, outCome)


testFile <- paste (baseFolder, '/input/' ,inputName, '/', inputName, '_full_1','.csv',sep='')

method = 'IROI'
outputFolder <- paste (baseFolder, '/output/one/' ,method, '/',inputName, sep='')
estimateUpLiftScore(CDTmodel,outCome, 'IROI', testFile, outputFolder, exceptAttrbute = c('stag', 'event'))

method = 'UBCF'
outputFolder <- paste (baseFolder, '/output/one/' ,method, '/',inputName, sep='') 
estimateUpLiftScore(recUBCFFModel,outCome, 'UBCF', testFile, outputFolder, manAttributes, exceptAttrbute = c('stag', 'event'))

method = 'IBCF'
outputFolder <- paste (baseFolder, '/output/one/' ,method, '/',inputName, sep='')   
estimateUpLiftScore(reIBCFFModel,outCome, 'IBCF', testFile, outputFolder, manAttributes, exceptAttrbute = c('stag', 'event'))

method = 'IPOP'
outputFolder <- paste (baseFolder, '/output/one/' ,method, '/',inputName, sep='')  
estimateUpLiftScore(recPOPULARFModel,outCome, 'IPOP', testFile, outputFolder, manAttributes, exceptAttrbute = c('stag', 'event'))  
  
