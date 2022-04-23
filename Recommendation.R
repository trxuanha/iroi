library(data.table)
library(knitr)
library(recommenderlab)


convertDatFrameToRecFormat<- function(raw_data, inoutcome){
  
  convertValueToRecFormat <- function(row, currentColum, outcome){
    val = 0
    if((row[currentColum] == 1)&&(row[outcome] == 1)){
      val = 1
    }
    return (val)
  }
  
  
  for (curcol in colnames(raw_data)){  
    if(curcol == inoutcome){
      next
    }
    raw_data[curcol] <- apply(raw_data, 1, convertValueToRecFormat, currentColum = curcol, outcome = inoutcome)
  }
  
  raw_data = dplyr::select (raw_data, -c(inoutcome))
  return(raw_data)
  
}



buildRecommendationModel<- function(training_data, modelName, manipulableAttribute, outcome, neighbor = 0){
  
  training_data =  training_data[ training_data[[ outcome  ]] == 1 , ]
  manipulableAttribute = c(manipulableAttribute, outcome)  
  training_data = dplyr::select (training_data, -c(outcome))
  matrix_training = as.matrix(training_data)
  
  bin_matrix_training <- as(matrix_training, "binaryRatingMatrix")
  
  if(neighbor > 0){
  
    if(modelName == 'UBCF')
	recModel <- Recommender(data = bin_matrix_training, method = modelName, param = list( nn = neighbor))


	if(modelName == 'IBCF')
		recModel <- Recommender(data = bin_matrix_training, method = modelName, param = list(k=neighbor))
		
	if(modelName == 'POPULAR')
		recModel <- Recommender(data = bin_matrix_training, method = modelName)			
  
  }else{
	recModel <- Recommender(data = bin_matrix_training, method = modelName)	
  }

	
	



	
  return (recModel)
  
  
}

estimateRecommendation <- function(recordForEstimate, recModel, manipulableAttribute){
  
  one_row = as.matrix(recordForEstimate)
  
  allColnames = colnames(one_row)
  maxcolName = 'NA'
  maxProb = 0 
  colNbr = ncol(recordForEstimate) 
  
  
  for(col in 1:colNbr){
    
    if(!is.element(allColnames[col], manipulableAttribute)){
      
      next
    }
    
    
    duplicate = t(matrix(one_row))
    duplicate[1, col] = 0
    
    bin_row = as( duplicate, "binaryRatingMatrix")
    
    recommendations <- predict(recModel, bin_row)
    recommendations <- as(recommendations, "matrix")
    
    
    if(is.na(recommendations[1, col] )){
      next
    }
    if(maxProb < recommendations[1, col] ){
      maxProb = recommendations[1, col]
      maxcolName = allColnames[col]
    }
    
  }
  
  if(is.na(maxcolName )){
    maxcolName <-'NA'
  }
  
  return  (list( maxProb , maxcolName  )) 
   
}