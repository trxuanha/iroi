library(pcalg)
library(graph)



baseFolder = getwd()
trainingFolder <- paste (baseFolder, '/input/' ,'turnover', sep='')
fullFilePath <- paste (trainingFolder,'/','turnover_full_1','.csv', sep='') 

dat <-read.csv(file = fullFilePath)

colnames(dat)[which(names(dat) == "longTermCandidate")] <- "long_emp_retention"

colnames(dat)[which(names(dat) == "way")] <- "transportation"



dat <- dplyr::select (dat, -c('stag', 'event') )
V <- colnames(dat)               
V

myFixedGaps <- matrix(1:169, nrow=13, ncol=13)
myFixedGaps[] <- FALSE 

myFixedGaps[2, 5] <- TRUE  
myFixedGaps[5, 2] <- TRUE 

myFixedGaps[3, 5] <- TRUE  
myFixedGaps[5, 3] <- TRUE  

myFixedGaps[1, 5] <- TRUE  
myFixedGaps[5, 1] <- TRUE 

myFixedGaps[1, 5] <- TRUE  
myFixedGaps[5, 1] <- TRUE 


myEdges <- matrix(1:169, nrow=13, ncol=13)
myEdges[] <- FALSE 

myEdges[2, 9] <- TRUE 
myEdges[9, 2] <- TRUE 

myEdges[6, 13] <- TRUE 
myEdges[13, 6] <- TRUE 

myEdges[4, 7] <- TRUE 
myEdges[7, 4] <- TRUE 


myEdges[1, 7] <- TRUE 
myEdges[7, 1] <- TRUE 

myEdges[5, 7] <- TRUE 
myEdges[7, 5] <- TRUE 

myEdges[7, 10] <- TRUE 
myEdges[10, 7] <- TRUE 

myEdges[1, 8] <- TRUE 
myEdges[8, 1] <- TRUE 

myEdges[2, 8] <- TRUE 
myEdges[8, 2] <- TRUE 

myEdges[13, 8] <- TRUE 
myEdges[8, 13] <- TRUE 


res <-pc(suffStat=list(dm=dat, adaptDF=FALSE), indepTest=binCItest, alpha=0.05, labels=colnames(dat), fixedGaps = myFixedGaps, fixedEdges = myEdges)

mygraph = attr(res, 'graph')
nodes(mygraph)

mygraph <-removeEdge('profession', 'gender', mygraph)
mygraph <-addEdge('gender', 'profession', mygraph, 1)

mygraph <-removeEdge('transportation', 'selfcontrol', mygraph)
mygraph <-addEdge('selfcontrol', 'transportation', mygraph, 1)


nA <- list()
nNodes <- length(nodes(mygraph))
nA$fillcolor <- c(long_emp_retention="yellow")
nA$shape <- c(long_emp_retention="box")
nattrs <- list(node=list(shape="ellipse", fixedsize=FALSE, fontsize=20))

outputGraphFolder <- paste(baseFolder, '/output/Graphs/', sep='')
outFileName <- paste(outputGraphFolder, 'TurnOverDAG','.jpg' ,sep='')

jpeg(outFileName, width = 350, height = 350)
plot(mygraph, attrs=nattrs, nodeAttrs=nA)
dev.off()