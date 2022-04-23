


estimatePropensityforMOTIVATION<- function(inputData){
  
  
  
  reg<-glm(MOTIVATION~ . - ALLOWANCE - MOTIVATION
           
           , family=binomial
           , data=inputData)
  
  
  return (reg$fitted)
  
}


estimatePropensityforALLOWANCE<- function(inputData){
  
  
  
  reg<-glm(ALLOWANCE~ . - ALLOWANCE - MOTIVATION
           
           , family=binomial
           , data=inputData)
  
  
  return (reg$fitted)
  
}



estimatePropensityforExtraversion<- function(inputData){
  
  reg<-glm(extraversion~ . - extraversion - greywage  
           , family=binomial
           , data=inputData)
  return (reg$fitted)
  
}

estimatePropensityforGreywage<- function(inputData){
  
  reg<-glm(greywage~ . - extraversion - greywage
           , family=binomial
           , data=inputData)
  return (reg$fitted)
}




estimatePropensityforEdu12<- function(inputData){
  
  
  reg<-glm(education_12~ . - prof - education_9 - education_12
           
           , family=binomial
           , data=inputData)
  
  
  return (reg$fitted)
  
}

estimatePropensityforEdu9<- function(inputData){
  
  
  reg<-glm(education_9~ . - prof - education_9 - education_12
           
           , family=binomial
           , data=inputData)
  
  
  return (reg$fitted)
  
}
