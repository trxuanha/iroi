source("CausalTree.R")
source("Recommendation.R")

library(stringr)
library(ini)
library(survival)
library(survminer)
library(patchwork)


estimateUpLiftScore<- function(model, outComeColName, estimationType, infileName, outputFileFolder, manipulableAttribute=NULL, exceptAttrbute = c()){
  
  
  data <-read.csv(file = infileName)
  
  data['LIFT_SCORE'] = 0
  data['TREATMENT_NAME'] = ''
  data['UPLIFT'] = 0
  data ['Y_TREATED'] = 0
  data ['N_TREATED'] = 0
  data ['Y_UNTREATED'] = 0
  data ['N_UNTREATED'] = 0
  data ['FOLLOW_REC'] = 0
  

 for(row in 1: nrow(data)){
   
   inputRow = dplyr::select (data[row, ], -c('LIFT_SCORE', 'TREATMENT_NAME','UPLIFT',
                                             'Y_TREATED','N_TREATED','Y_UNTREATED','N_UNTREATED', 'FOLLOW_REC', outComeColName, exceptAttrbute ))
   
   if(estimationType == 'IROI'){
     
     val <- predictLargestCausalEffect(model, inputRow )
     
   }
   
   if((estimationType == 'UBCF') || (estimationType == 'IBCF') || (estimationType == 'IPOP')|| (estimationType == 'AR')){
     
     val <- estimateRecommendation( inputRow, model, manipulableAttribute )
     
   }   
   
   val <- unlist(val)
   data[row,'LIFT_SCORE'] <- as.numeric(val[1])
   data[row,'TREATMENT_NAME'] <- val[2]
    
 }
  
  data <- data[order(-data$LIFT_SCORE),]
  
  y_treated <- 0
  n_treated <- 0
  y_untreated <- 0
  n_untreated <- 0
  
  
  for(row in 1: nrow(data)){
    
    
    TREATMENT_NAME = data[row,'TREATMENT_NAME']  
    TREATMENT_NAME <- toString(TREATMENT_NAME)
    
    data[row,'N_TREATED']<- n_treated
    data[row,'Y_TREATED']<- y_treated
    data[row,'N_UNTREATED']<- n_untreated
    data[row,'Y_UNTREATED']<- y_untreated
    
    if((TREATMENT_NAME != 'NA')&&(data[row,TREATMENT_NAME] == 1)){
      data [row, 'FOLLOW_REC'] = 1
      n_treated <- n_treated + 1
      data[row,'N_TREATED']<- n_treated
      if(data[row,outComeColName] == 1){
        y_treated <- y_treated + 1
        data[row,'Y_TREATED']<- y_treated
      }
      
    }else{
      
      n_untreated <- n_untreated + 1
      data[row,'N_UNTREATED']<- n_untreated
	  
      if(data[row,outComeColName] == 1){  
        y_untreated <- y_untreated + 1
        data[row,'Y_UNTREATED']<- y_untreated
      }
      
    }
    
    if(n_treated == 0) {
      data[row,'UPLIFT'] = 0
    }else if(n_untreated == 0){
      data[row,'UPLIFT'] = 0
    }else{
      liftestimate = ((y_treated/n_treated) - (y_untreated/n_untreated) )*(n_treated + n_untreated)
      qiniestimate = ((y_treated) - (y_untreated*(n_treated/n_untreated) ))
      data[row,'UPLIFT'] <- liftestimate
    }
    
  }
  
  ## update uplift by percentage
  
  totalIncrease <- ((y_treated/n_treated) - (y_untreated/n_untreated) )
  
  for(row in 1: nrow(data)){
    
    n_treated <- data[row,'N_TREATED']
    y_treated <- data[row,'Y_TREATED']
    n_untreated <- data[row,'N_UNTREATED']
    y_untreated <- data[row,'Y_UNTREATED']
    
    liftestimate <- (((y_treated/n_treated) - (y_untreated/n_untreated) ))
    liftestimateWithBase <- (((y_treated/n_treated) - (y_untreated/n_untreated) ))/totalIncrease
    data[row,'UPLIFT'] <- liftestimate
    
    
  }
  
 
   ##Output
  

  
	fileName = basename(infileName)
	fileNameParts <- strsplit(fileName ,'\\.')
	fileNameParts <- unlist(fileNameParts)

	secondFileNameParts <- strsplit(fileNameParts[1] ,'_')
	secondFileNameParts <- unlist(secondFileNameParts)

	newFileName <- paste(c(secondFileNameParts[1],'_', secondFileNameParts[2], '_', estimationType, '_', secondFileNameParts[3],
						   '.', fileNameParts[2]), collapse = "")
					   
  fullPath <- paste(c(outputFileFolder,'/',newFileName ), collapse = "")
  write.csv(data,fullPath, row.names = FALSE)
  
  
}

plotUpliftCurve<- function(upLiftFolder, pattern='*',  percentStep=0.1){
  
  inFileName1=NULL
  inFileName2=NULL
  inFileName3=NULL
  inFileName4=NULL
  results = list.files(upLiftFolder, pattern, full.names = TRUE)  
  legendText = c()
  color = c()
  linest = c()
  pch = c()
  
  if(length(results) >= 1){  
    inFileName1 = results[1]
  }

  if(length(results) >= 2){
    inFileName2 = results[2]
  }
  
  if(length(results) >= 3){
    inFileName3 = results[3]
  }
  
  if(length(results) >= 4){
    inFileName4 = results[4]
  }
  
  if(is.null(inFileName1)){
    return (0)
  }
  
  data <-read.csv(file = inFileName1)
  
  totalSize = nrow(data)
  
  step <- as.integer(totalSize*percentStep)
  mycount <- 1
  
  y_val = vector()
  
  currentSize <- step * mycount
  
  while (currentSize <= totalSize){
    
    val = data[currentSize,'UPLIFT']
    
    y_val<- c(y_val, val)
    
    mycount <- mycount + 1
    
    currentSize <- step * mycount
    
  }
  
  if(currentSize < totalSize){
    
    val = data[totalSize,'UPLIFT']
    
    y_val<- c(y_val, val)
    
  }
  
  
  # Add start point
  
  orPerpop <- (seq(length(y_val))/length(y_val))*100
  plot(orPerpop, y_val, type="l", col="blue", ljoin="bevel", lend=2, 
       cex.main= 1, font.main = 1, #ylim=c(-2,30), 
       ylim=c(-0.1,0.5), 
       lwd=3, xlab="Percentage of population targeted(%)",ylab="Cumulative incremental gains")

  color <- c(color, 'blue')
  
  inFileName1 <- basename(inFileName1)
    
  if(str_detect(inFileName1, 'IROI')){
    
    legendText <- c(legendText, 'IROI')  
  }
  
  if(str_detect(inFileName1, 'UBCF') ){
    
    legendText <- c(legendText, 'UBCF')
    
  }
  
  if(str_detect(inFileName1, 'IBCF')){
    
    legendText <- c(legendText, 'IBCF')
    
  }
  
  if(str_detect(inFileName1, 'IPOP')){
    
    legendText <- c(legendText, 'IPOP')
    
  }
  
  

  if(is.null(inFileName2)){
    return (0)
  }
  
  data <-read.csv(file = inFileName2)
  
  totalSize = nrow(data)
  
  step <- as.integer(totalSize*percentStep)
  mycount <- 1
  
  y_val = vector()
  
  currentSize <- step * mycount
  
  while (currentSize <= totalSize){
    
    val = data[currentSize,'UPLIFT']
    
    y_val<- c(y_val, val)
    
    mycount <- mycount + 1
    
    currentSize <- step * mycount
    
  }
  
  if(currentSize < totalSize){
    
    val = data[totalSize,'UPLIFT']
    
    y_val<- c(y_val, val)
    
  }
  
  
  # Add start point
  
  orPerpop <- (seq(length(y_val))/length(y_val))*100
  ### Add origin point
  orPerpop <- c(0, orPerpop)
  y_val<- c(0, y_val)
  
  lines(orPerpop, y_val, col="green",lty=1, type="l", ljoin="bevel", lend=2, lwd=3)
  
  color <- c(color, 'green')
  
  inFileName2 <- basename(inFileName2)
  
  if(str_detect(inFileName2, 'IROI')){
    
    legendText <- c(legendText, 'IROI')
    
  }
  
  if(str_detect(inFileName2, 'UBCF') ){
    
    
    
    legendText <- c(legendText, 'UBCF')
    
  }
  
  if(str_detect(inFileName2, 'IBCF')){
    
    legendText <- c(legendText, 'IBCF')
    
  }
  
  if(str_detect(inFileName2, 'IPOP')){
    
    legendText <- c(legendText, 'IPOP')
    
  }
  
  
  if(is.null(inFileName3)){
    return (0)
  }
  
  data <-read.csv(file = inFileName3)
  
  totalSize = nrow(data)
  
  step <- as.integer(totalSize*percentStep)
  mycount <- 1
  
  y_val = vector()
  
  currentSize <- step * mycount
  
  while (currentSize <= totalSize){
    
    val = data[currentSize,'UPLIFT']
    
    y_val<- c(y_val, val)
    
    mycount <- mycount + 1
    
    currentSize <- step * mycount
    
  }
  
  if(currentSize < totalSize){
    
    val = data[totalSize,'UPLIFT']
    
    y_val<- c(y_val, val)
    
  }
  
  
  # Add start point
  
  orPerpop <- (seq(length(y_val))/length(y_val))*100
  ### Add origin point
  orPerpop <- c(0, orPerpop)
  y_val<- c(0, y_val)
  
  lines(orPerpop, y_val, col="yellow",lty=1, type="l", ljoin="bevel", lend=2, lwd=3)
  color <- c(color, 'yellow')
  
  
  inFileName3 <- basename(inFileName3)
  
  
  if(str_detect(inFileName3, 'IROI')){
    
    legendText <- c(legendText, 'IROI')
    
  }
  
  if(str_detect(inFileName3, 'UBCF') ){
    
    
    
    legendText <- c(legendText, 'UBCF')
    
  }
  
  if(str_detect(inFileName3, 'IBCF')){
    
    legendText <- c(legendText, 'IBCF')
    
  }
  
  if(str_detect(inFileName3, 'IPOP')){
    
    legendText <- c(legendText, 'IPOP')
    
  }
  
  ##########################################
  
  
  if(is.null(inFileName4)){
    return (0)
  }
  
  
  data <-read.csv(file = inFileName4)
  
  totalSize = nrow(data)
  
  step <- as.integer(totalSize*percentStep)
  mycount <- 1
  
  y_val = vector()
  
  currentSize <- step * mycount
  
  while (currentSize <= totalSize){
    
    val = data[currentSize,'UPLIFT']
    
    y_val<- c(y_val, val)
    
    mycount <- mycount + 1
    
    currentSize <- step * mycount
    
  }
  
  if(currentSize < totalSize){
    
    val = data[totalSize,'UPLIFT']
    
    y_val<- c(y_val, val)
    
  }
  
  
  # Add start point
  
  orPerpop <- (seq(length(y_val))/length(y_val))*100
  ### Add origin point
  orPerpop <- c(0, orPerpop)
  y_val<- c(0, y_val)
  
  lines(orPerpop, y_val, col="purple",lty=1, type="l", ljoin="bevel", lend=2, lwd=3)
  color <- c(color, 'purple')
  
  inFileName4 <- basename(inFileName4)
  
  
  if(str_detect(inFileName4, 'IROI')){
    
    legendText <- c(legendText, 'IROI')
    
  }

  if(str_detect(inFileName4, 'UBCF') ){
    
    
    
    legendText <- c(legendText, 'UBCF')
    
  }
  
  if(str_detect(inFileName4, 'IBCF')){
    
    legendText <- c(legendText, 'IBCF')
    
  }
  
  if(str_detect(inFileName4, 'IPOP')){
    
    legendText <- c(legendText, 'IPOP')
    
  }
  

  legend("topleft", legend=legendText,
         col= color, lty = c(1), cex=1, pch = c(19))
  
  
  
}


estimateAverageUplift<-function(upLiftFolder, pattern='*',percentStep=0.2){
  

  results = list.files(upLiftFolder, pattern, full.names = TRUE)
  
  
  if(length(results) == 0){
    return(NULL)
  }
  
  data <-read.csv(file = results[1])
  
  upliftAggregation <- NULL
  
  for(timecn in 1: length(results)){
    
    
    data <-read.csv(file = results[timecn])
    
    # include valid data
    #data <- data[!(is.na(data$UPLIFT)),]
    
    data = subset(data, TREATMENT_NAME != 'NA' )
    totalSize = nrow(data)
    step <- as.integer(totalSize*percentStep)
    mycount <- 1
    
    y_val = vector()
    
    
    currentSize <- step * mycount
    
    while (currentSize <= totalSize){
      
      val = data[currentSize,'UPLIFT']
      
      y_val<- c(y_val, val)
      
      mycount <- mycount + 1
      
      currentSize <- step * mycount
      
    }
    
    if(currentSize < totalSize){
      
      val = data[totalSize,'UPLIFT']
      
      y_val<- c(y_val, val)
      
    }
    
    
    # Add start point
    
    orPerpop <- (seq(length(y_val))/length(y_val))*100
    ### Add origin point
    #orPerpop <- c(0, orPerpop)
    #y_val<- c(0, y_val)
    
    
    if(is.null(upliftAggregation)){
      
      upliftAggregation <- as.data.frame(t(y_val ))
      
    }else{
      
      upliftAggregation <- rbind(upliftAggregation, y_val)  
    }
    
    
  }

  
  return (colMeans(upliftAggregation))
  
}


plotAverageUplift<-function(upLiftFolder, isTesting=TRUE,percentStep=0.2, ymin=0, ymax=1){
  
  
  
  mode = 'test'
  
  if(isTesting == FALSE){
    mode = 'train'
  }
  
  
  legendText = c()
  color = c()
  
  
  pattern<- paste(c('.*(',mode,'.*IROI).*'), collapse = "")
  avgerageVec = estimateAverageUplift(upLiftFolder, pattern, percentStep)
  
  
 
  myplotData <- NULL
  
  variableList = c()
  if(!is.null(avgerageVec)){
    
    color = c(color,  'blue')
    legendText = c(legendText, 'IROI')
    
    popPercent <- (seq(length(avgerageVec))/length(avgerageVec))*100
    
    IROI <- avgerageVec
    
    print(avgerageVec)
    

    
    myplotData <- data.frame(IROI, popPercent)
    
    
  }
  


  pattern<- paste(c('.*(',mode,'.*UBCF).*'), collapse = "")
  
  
  avgerageVec = estimateAverageUplift(upLiftFolder, pattern, percentStep)
  

  
  if(!is.null(avgerageVec)){
    
    color = c(color,  'green')
    legendText = c(legendText, 'UBCF')
    
    popPercent <- (seq(length(avgerageVec))/length(avgerageVec))*100
    
    
    UBCF <- avgerageVec
    if(!is.null(myplotData)){
      
      
      myplotData <- cbind(myplotData, UBCF)  
      
    }else{
      
      myplotData <- data.frame(UBCF, popPercent)
    }
    
    
  }



  pattern<- paste(c('.*(',mode,'.*IBCF).*'), collapse = "")
  
  avgerageVec = estimateAverageUplift(upLiftFolder, pattern, percentStep)
  
  if(!is.null(avgerageVec)){
    
    color = c(color,  'yellow')
    legendText = c(legendText, 'IBCF')
    
    
    popPercent <- (seq(length(avgerageVec))/length(avgerageVec))*100
    
    IBCF <- avgerageVec
    if(!is.null(myplotData)){
      
      
      myplotData <- cbind(myplotData, IBCF)  
      
    }else{
      
      myplotData <- data.frame(IBCF, popPercent)
    }
    
    

    
    
  }

     

  pattern<- paste(c('.*(',mode,'.*IPOP).*'), collapse = "")
  
  avgerageVec = estimateAverageUplift(upLiftFolder, pattern, percentStep)
  
  if(!is.null(avgerageVec)){
    
    color = c(color,  'purple')
    legendText = c(legendText, 'IPOP')
    

    popPercent <- (seq(length(avgerageVec))/length(avgerageVec))*100
    
    IPOP <- avgerageVec
    if(!is.null(myplotData)){
      
      
      myplotData <- cbind(myplotData, IPOP)  
      
    }else{
      
      myplotData <- data.frame(IPOP, popPercent)
    }
    
    
    
  }
  
  ################
  
  
  pattern<- paste(c('.*(',mode,'.*CAE).*'), collapse = "")
  
  avgerageVec = estimateAverageUplift(upLiftFolder, pattern, percentStep)
  
  if(!is.null(avgerageVec)){
    
    color = c(color,  'black')
    legendText = c(legendText, 'CAE')
    
    
    popPercent <- (seq(length(avgerageVec))/length(avgerageVec))*100
    
    CAE <- avgerageVec
    if(!is.null(myplotData)){
      
      
      myplotData <- cbind(myplotData, CAE)  
      
    }else{
      
      myplotData <- data.frame(CAE, popPercent)
    }
    
    
    
  }
  
  
  ###########
  
  tempData = dplyr::select (myplotData, -c('popPercent'))
  print(tempData)
  print(tempData[nrow(tempData),])
  
  maxVal<- max(tempData[nrow(tempData),])
  
  print(maxVal)

  #myplotData <- myplotData/maxVal
  
  if(!is.null(myplotData)){
    

    
    myplotData = melt(myplotData, id = 'popPercent')
    
    
     myPlot <- ggplot(myplotData, aes(x = popPercent, y = value, colour = variable)) + 
      geom_line() + geom_bar(stat="identity")+
      ylab(label="Cumulative outcome improvement") + 
      xlab("Percentage of population targeted(%)") + 
      geom_point(size=2)  +
      geom_line(size=1, position = position_dodge(0.5)) + 
      scale_colour_manual(values=c("blue", "green", "red", "purple", "black")) +
      theme_Publication() +
      labs(colour = "") + ylim(ymin, ymax)
    
    
  }

  
  return(myPlot)
  
  
  }

##################################


plotAverageUpliftTrend<-function(upLiftFolder, isTesting=TRUE, ymin=0, ymax=1, method){
  
  
  cusColor = 'yellowgreen'
  
  methodEx = method
  
  if(method == 'IROI' )
	cusColor = 'yellowgreen'

  if(method == 'IPOP' ){
	cusColor = 'steelblue'
	methodEx = 'IPOP'
  
  }
	
  if(method == 'IBCF' )
	cusColor = 'darkred'

  if(method == 'UBCF' )
	cusColor = 'deeppink'

	
  percentStep = 0.25
  
  mode = 'test'
  
  if(isTesting == FALSE){
    mode = 'train'
  }
  
  
  legendText = c()
  color = c()
  
  
  pattern<- paste(c('.*(',mode,'.*', methodEx, ').*'), collapse = "")
  

  
  
  avgerageVec = estimateAverageUplift(upLiftFolder, pattern, percentStep)

  print('patternpatternpatternpattern')
  
  print(pattern)
  
  print(avgerageVec)
  
  
 
  myplotData <- NULL
  
  variableList = c()
  if(!is.null(avgerageVec)){
    
    color = c(color,  'blue')
    legendText = c(legendText, 'IROI')
    
    popPercent <- (seq(length(avgerageVec))/length(avgerageVec))*100
    
    IROI <- avgerageVec
    
    print(avgerageVec)
    

    
    myplotData <- data.frame(IROI, popPercent)
    
    
  }
  

  
  
  ###########
  
  tempData = dplyr::select (myplotData, -c('popPercent'))
  print(tempData)
  print(tempData[nrow(tempData),])
  
  maxVal<- max(tempData[nrow(tempData),])
  
  print(maxVal)

  #myplotData <- myplotData/maxVal
  
  if(!is.null(myplotData)){
    

    
    myplotData = melt(myplotData, id = 'popPercent')
    
    
	myPlot <- ggplot(myplotData) + 
			  geom_col(aes(x = popPercent, y = value), size = 1, fill = cusColor) +
	          geom_line(aes(x = popPercent + 11, y = value), size = 1.5, color="blue", group = 1)+
			  geom_point(aes(x = popPercent + 11 , y = value), size = 3, color = "blue")+
			  ylab(label="Cumulative outcome improvement") + 
			  xlab("Percentage of population targeted(%)") + 
			  theme_Publication() +
			  labs(title= method) + ylim(ymin, ymax)	  
					  
    
    
  }
  
  
  return(myPlot)
  
  
  }



#################################

theme_Publication <- function(base_size=25, base_family="Helvetica") {
  library(grid)
  library(ggthemes)
  (theme_foundation(base_size=base_size, base_family=base_family)
    + theme(plot.title = element_text(face = "bold",
                                      size = rel(1.2), hjust = 0.5),
            text = element_text(),
            panel.background = element_rect(colour = NA),
            plot.background = element_rect(colour = NA),
            panel.border = element_rect(colour = NA),
            axis.title = element_text(face = "bold",size = rel(1)),
            axis.title.y = element_text(angle=90,vjust =2),
            axis.title.x = element_text(vjust = -0.2),
            axis.text = element_text(),
            axis.line = element_line(colour="black"),
            axis.ticks = element_line(),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            legend.key = element_rect(colour = NA),
            legend.position = "bottom",
            legend.direction = "horizontal",
            legend.key.size= unit(0.6, "cm"),
            legend.margin = unit(0, "cm"),
            legend.title = element_text(face="italic"),
            plot.margin=unit(c(10,5,5,5),"mm"),
            strip.background=element_rect(colour="#f0f0f0",fill="#f0f0f0"),
            strip.text = element_text(face="bold")
    ))
  
}
scale_fill_Publication <- function(...){
  library(scales)
  discrete_scale("fill","Publication",manual_pal(values = c("#386cb0","#fdb462","#7fc97f","#ef3b2c","#662506","#a6cee3","#fb9a99","#984ea3","#ffff33")), ...)
  
}
scale_colour_Publication <- function(...){
  library(scales)
  discrete_scale("colour","Publication",
                 manual_pal(values = c("#1664d9","red","orange","green4","purple2","#1f9eb3",
                                       "#d93572","#f781bf","#e41a1c")), ...)
  
}
