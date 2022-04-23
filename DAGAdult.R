library(pcalg)
library(graph)



baseFolder = getwd()
trainingFolder <- paste (baseFolder, '/input/' ,'adult', sep='')
fullFilePath <- paste (trainingFolder,'/','adult_full_1','.csv', sep='') 

dat <-read.csv(file = fullFilePath)

dat <- dplyr::select (dat, -c('salary'))

myFixedGaps <- matrix(1:196, nrow=14, ncol=14)
myFixedGaps[] <- FALSE 


myEdges <- matrix(1:196, nrow=14, ncol=14)
myEdges[] <- FALSE 

myEdges[3, 13] <- TRUE 
myEdges[13, 3] <- TRUE 

res <-pc(suffStat=list(dm=dat, adaptDF=FALSE), indepTest=binCItest, alpha=0.05, labels=colnames(dat), fixedGaps = myFixedGaps, fixedEdges = myEdges)



mygraph = attr(res, 'graph')
nodes(mygraph)

mygraph <-removeEdge('prof', 'education_9', mygraph)
mygraph <-addEdge('education_9', 'prof', mygraph, 1)

mygraph <-removeEdge('prof', 'education_12', mygraph)
mygraph <-addEdge('education_12', 'prof', mygraph, 1)

mygraph <-removeEdge('prof', 'self_emp', mygraph)
mygraph <-addEdge('self_emp', 'prof', mygraph, 1)


mygraph <-removeEdge('prof', 'hours_30', mygraph)
mygraph <-addEdge('hours_30', 'prof', mygraph, 1)

mygraph <-removeEdge('prof', 'age_30', mygraph)
mygraph <-addEdge('age_30', 'prof', mygraph, 1)





nA <- list()
nNodes <- length(nodes(mygraph))
nA$fillcolor <- c(prof="yellow")
nA$shape <- c(prof="box")
nattrs <- list(node=list(shape="ellipse", fixedsize=FALSE, fontsize=20))

outputGraphFolder <- paste(baseFolder, '/output/Graphs/', sep='')
outFileName <- paste(outputGraphFolder, 'AdultDAG','.jpg' ,sep='')

jpeg(outFileName, width = 1600, height = 1000)
plot(mygraph, attrs=nattrs, nodeAttrs=nA)
dev.off()