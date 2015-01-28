# Project: AXA Kaggle Trip Driver Predicition
## Fuction: hClustCurve
## Title: Hierarchical Clustering Exploratory Visualizer Function
## Creator: Christopher Risi
## Create Date: January, 16th, 2015
## Modifier:  
## Modified Date: 
###Version 0.1###

hClustCurve <-function(curveScores, nclusters)
  {
    hierarchicalCluster.single <- hclust(dist(curveScores), method="single")
    hierarchicalCluster.complete <- hclust(dist(curveScores), method="complete")
    hierarchicalCluster.average <- hclust(dist(curveScores), method="average")
    hierarchicalCluster.median <- hclust(dist(curveScores), method="median")
    hierarchicalCluster.centroid <- hclust(dist(curveScores), method="centroid")
    
    hierarchicalCluster.singleTreeCut <- cutree(hierarchicalCluster.single, k= nclusters)
    hierarchicalCluster.completeTreeCut <- cutree(hierarchicalCluster.complete, k= nclusters)
    hierarchicalCluster.averageTreeCut <- cutree(hierarchicalCluster.average, k= nclusters)
    hierarchicalCluster.medianTreeCut <- cutree(hierarchicalCluster.median, k= nclusters)
    hierarchicalCluster.centroidTreeCut <- cutree(hierarchicalCluster.centroid, k= nclusters)
    
    
    crvScoresCuts <- cbind(curveScores, hierarchicalCluster.singleTreeCut,hierarchicalCluster.completeTreeCut,hierarchicalCluster.averageTreeCut,hierarchicalCluster.medianTreeCut,hierarchicalCluster.centroidTreeCut)
  
    mainPlot <- ggplot(crvScoresCuts, aes(curveScore, radius))
    
    plotSingle <- mainPlot + geom_point(aes(colour = factor(hierarchicalCluster.singleTreeCut),shape = factor(hierarchicalCluster.singleTreeCut)), size = 3) + scale_shape(solid = FALSE)
    plotComplete <- mainPlot + geom_point(aes(colour = factor(hierarchicalCluster.completeTreeCut),shape = factor(hierarchicalCluster.completeTreeCut)), size = 3) + scale_shape(solid = FALSE)
    plotAverage <- mainPlot + geom_point(aes(colour = factor(hierarchicalCluster.averageTreeCut),shape = factor(hierarchicalCluster.averageTreeCut)), size = 3) + scale_shape(solid = FALSE)
    plotMedian <- mainPlot + geom_point(aes(colour = factor(hierarchicalCluster.medianTreeCut),shape = factor(hierarchicalCluster.medianTreeCut)), size = 3) + scale_shape(solid = FALSE)
    plotCentroid <- mainPlot + geom_point(aes(colour = factor(hierarchicalCluster.centroidTreeCut),shape = factor(hierarchicalCluster.centroidTreeCut)), size = 3) + scale_shape(solid = FALSE)
    
    grid.arrange(plotSingle, plotComplete, plotAverage, plotMedian, plotCentroid , ncol=3, nrow=2)  
  }