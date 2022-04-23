library(survival)
library(survminer)
library(ggpubr)


baseFolder <- getwd()
topPercent <- 0.25
dataforPlot <- NULL

getSurvPlot <-function(inData, method, topPercent, inputName){
    
    liftSurvivalData <-data.frame(inData)
    liftSurvivalData <- liftSurvivalData[order(-liftSurvivalData$LIFT_SCORE),]
    topRow = as.integer(nrow(liftSurvivalData)*topPercent)
    liftSurvivalData <- liftSurvivalData[1:topRow, ]
    dataforPlot <<- liftSurvivalData
	
	fit2 <- survfit(Surv(time = dataforPlot$stag, event = dataforPlot$event)
                    ~ FOLLOW_REC , data = dataforPlot)
	surv_diff <- survdiff(Surv(time = dataforPlot$stag, event = dataforPlot$event)
							  ~ FOLLOW_REC, data = dataforPlot, rho = 0)

	
	
    options(scipen=999)

	SVC <- ggsurvplot(fit2, data = dataforPlot, pval = F, conf.int = T,
					  legend.title = '', 
					  legend.labs = c("Non-Follower", "Follower"),
					  legend = "bottom",
					  font.x = c(20, "bold"),
					  font.y = c(12, "bold"),
					  font.tickslab = c(20, "plain"),
					  font.legend = c(20, "bold", "black"),
					  xlim = c(0, 200),
					  break.x.by = 100,
					  break.y.by = 0.5,
					  ylab = c(""),
					  xlab = c("Weeks") 
	) + labs(title = method) + guides(colour = guide_legend(nrow = 1))	




    return (SVC$plot +  theme(plot.title = element_text(hjust = 0.5, vjust = 1.0, size=20)))
}

methods <- c('IPOP', 'UBCF', 'IBCF',  'IROI')
comGraph <- list()

inputName<- 'turnover'
dataforPlot <- NULL

for (method in methods){
    outputBase <- paste(baseFolder, '/output/one/', method,'/', inputName ,sep='')
    combinedData <- NULL
    inFilePath <- paste (outputBase, '/', inputName,'_full_',method,  '_1.csv' , sep='')
    combinedData <-read.csv(file = inFilePath)
	SVgp <- getSurvPlot(combinedData, method, topPercent, inputName)
	comGraph <- c(comGraph, list(SVgp))	
}


outputBase <- paste(baseFolder, '/output/PerformanceEval/' ,sep='')
outFileName <- paste('TurnoverSurv', '.png' ,sep='') 
ggsave(paste0(outputBase, outFileName), ggarrange(plotlist=comGraph, common.legend = TRUE, ncol=2, nrow =2, legend='bottom'),
       width=40, height=30,
       units='cm')
	   

