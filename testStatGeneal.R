load("data/statGeneal.rda")
statGeneal = statGeneal[1:500,]
getChild("Christian Robert",statGeneal)
ig <- dfToIG(statGeneal)
getYear("Nicolas Chopin",statGeneal)
getAncestors("Chana Lowenstein",statGeneal,3)
getParent("Shlomo Sawilowsky",statGeneal)
getDegree("Boris Shulkin", "R. Clifford Blair", ig, statGeneal)
getBasicStatistics(ig)
statGeneal[which(statGeneal$child=="Boris Shulkin"),]

pathCC = getPath("Carl Morris","Cindy Christiansen", ig, statGeneal, isDirected=FALSE)
# Have not dealt with cases where year is NA
# Possibly imputation?? (halfway between parent and child)
plotPath(pathCC)

# Probably don't need to write out full name (just points/dots)
# Important points emphasized (are all descended from a few people? )
# Standard network (remove year), but could see people who have lots of advisees (ggNet)

p = plotPathOnAllAn(pathCC, statGeneal, ig, binVector = 1:100)
p2 = plotPathOnAllAn2(pathCC, statGeneal, ig, binVector = 1:100)

library(animint)
animint2dir(list(plot = p, plot2 = p2), out.dir = "animintAll", open.browser = FALSE)



########################## Tweak plotPathOnAll ########################## 
plotPathOnAllAn = function(path, geneal, ig, binVector=sample(1:12, 12)){
  x <- y <- xend <- yend <- xstart <- ystart <- label <- NULL
  if(class(ig)!="igraph"){
    stop("ig must be an igraph object")
  }
  
  if(mode(path)=="character"){
    if(length(path)!=2){
      stop("path needs to contain two variety names")
    }
    varieties <- path
    path <- getPath(varieties[1], varieties[2], ig)
  } else if(sum(names(path)%in%c("pathVertices", "yearVertices"))!=2){
    stop("path does not appear to be a result of the getPath() function")
  } 
  
  # All node labels and their x, y position (label, x, y)
  pMPDF <- buildMinusPathDF(path, geneal, ig, binVector)
  # x, y, xend, yend of all edges
  eTDF <- buildEdgeTotalDF(geneal, ig, binVector)
  # label, xstart, ystart, xend, yend, x, y (of only path)
  pTDF <- buildPlotTotalDF(path, geneal, ig, binVector)
  
  textFrame = data.frame(x = pMPDF$x, y = pMPDF$y, label = pMPDF$label)
  textFrame = transform(textFrame,
                        w = strwidth(pMPDF$label, 'inches') + 0.25,
                        h = strheight(pMPDF$label, 'inches') + 0.25,
                        d = "."
  )
  
  # The plotTotalImage object creates two line segments (geom_segment), one to create grey
  # edges for non-path connections between pairs of nodes, the other to create light-green
  # edges for path connections between pairs of nodes; and two labels (geom_text), one to
  # create labels of size 2 for non-path connections between pairs of nodes, the other to
  # create labels of size 2.5 and boldfaced for path connections between pairs of nodes.
  plotTotalImage = ggplot2::ggplot(data = pMPDF, ggplot2::aes(x = x, y = y)) +
    ggplot2::geom_segment(data = eTDF, ggplot2::aes(x=x, y=y-.1, xend=xend, yend=yend+.1), colour = "gray84") +
    ggplot2::geom_segment(data = pTDF, ggplot2::aes(x=xstart, y=ystart, xend=xend, yend=yend), colour = "seagreen2", size = 1) +
    ggplot2::geom_text(data = textFrame,ggplot2::aes(x = x, y = y, label = d, clickSelects = d), size = 20) +
    ggplot2::geom_text(data = pTDF,ggplot2::aes(x = x, y = y, label = label), size = 2.5,  fontface="bold") +
    ggplot2::xlab("Year") +    
    # Erase the y-axis, and only include grids from the x-axis
    ggplot2::theme(axis.text.y=ggplot2::element_blank(),axis.ticks.y=ggplot2::element_blank(),
                   axis.title.y=ggplot2::element_blank(),legend.position="none",
                   panel.grid.major.y=ggplot2::element_blank(),
                   panel.grid.minor=ggplot2::element_blank())
  # Return the plotTotalImage
  plotTotalImage
}

plotPathOnAllAn2 = function(path, geneal, ig, binVector=sample(1:12, 12)){
  x <- y <- xend <- yend <- xstart <- ystart <- label <- NULL
  if(class(ig)!="igraph"){
    stop("ig must be an igraph object")
  }
  
  if(mode(path)=="character"){
    if(length(path)!=2){
      stop("path needs to contain two variety names")
    }
    varieties <- path
    path <- getPath(varieties[1], varieties[2], ig)
  } else if(sum(names(path)%in%c("pathVertices", "yearVertices"))!=2){
    stop("path does not appear to be a result of the getPath() function")
  } 
  
  # All node labels and their x, y position (label, x, y)
  pMPDF <- buildMinusPathDF(path, geneal, ig, binVector)
  # x, y, xend, yend of all edges
  eTDF <- buildEdgeTotalDF(geneal, ig, binVector)
  # label, xstart, ystart, xend, yend, x, y (of only path)
  pTDF <- buildPlotTotalDF(path, geneal, ig, binVector)
  
  textFrame = data.frame(x = pMPDF$x, y = pMPDF$y, label = pMPDF$label)
  textFrame = transform(textFrame,
                        w = strwidth(pMPDF$label, 'inches') + 0.25,
                        h = strheight(pMPDF$label, 'inches') + 0.25,
                        d = "."
  )
  
  # The plotTotalImage object creates two line segments (geom_segment), one to create grey
  # edges for non-path connections between pairs of nodes, the other to create light-green
  # edges for path connections between pairs of nodes; and two labels (geom_text), one to
  # create labels of size 2 for non-path connections between pairs of nodes, the other to
  # create labels of size 2.5 and boldfaced for path connections between pairs of nodes.
  plotTotalImage = ggplot2::ggplot(data = pMPDF, ggplot2::aes(x = x, y = y)) +
    ggplot2::geom_segment(data = eTDF, ggplot2::aes(x=x, y=y-.1, xend=xend, yend=yend+.1), colour = "gray84") +
    ggplot2::geom_segment(data = pTDF, ggplot2::aes(x=xstart, y=ystart, xend=xend, yend=yend), colour = "seagreen2", size = 1) +
    ggplot2::geom_text(data = textFrame,ggplot2::aes(x = x, y = y, showSelected = label), size = 20) +
    ggplot2::geom_text(data = pTDF,ggplot2::aes(x = x, y = y, label = label), size = 2.5,  fontface="bold") +
    ggplot2::xlab("Year") +    
    # Erase the y-axis, and only include grids from the x-axis
    ggplot2::theme(axis.text.y=ggplot2::element_blank(),axis.ticks.y=ggplot2::element_blank(),
                   axis.title.y=ggplot2::element_blank(),legend.position="none",
                   panel.grid.major.y=ggplot2::element_blank(),
                   panel.grid.minor=ggplot2::element_blank())
  # Return the plotTotalImage
  plotTotalImage
}

buildMinusPathDF = function(path, geneal, ig, binVector=1:12){
  
  if(class(ig)!="igraph"){
    stop("ig must be an igraph object")
  }
  
  if(sum(1:length(binVector)%in%binVector)!=length(binVector)){
    stop("binVector must contain all numbers 1:length(binVector)")
  }
  
  if(mode(path)=="character"){
    if(length(path)!=2){
      stop("path needs to contain two variety names")
    }
    varieties <- path
    path <- getPath(varieties[1], varieties[2], ig)
  } else if(sum(names(path)%in%c("pathVertices", "yearVertices"))!=2){
    stop("path does not appear to be a result of the getPath() function")
  } 
  
  tG <- buildSpreadTotalDF(geneal, ig, binVector)
  eG <- igraph::get.data.frame(ig, "edges")
  
  label=tG$name
  x=tG$year
  y=tG$y
  # If the label is part of the path, then we change the its value to NA
  for (i in 1:length(label)){
    if (label[i]%in%path$pathVertices){
      label[i]=NA
    }
  }
  plotMinusPathDF = data.frame(label,x,y)
  
  # Return the data frame object of the full genealogy
  plotMinusPathDF
}

buildSpreadTotalDF = function(geneal, ig, binVector=1:12){
  if(class(ig)!="igraph"){
    stop("ig must be an igraph object.")
  }
  
  if(sum(1:length(binVector)%in%binVector)!=length(binVector)){
    stop("binVector must contain all numbers 1:length(binVector)")
  }
  
  totalDF = igraph::get.data.frame(ig, "vertices")
  #totalDF = totalDF[!is.na(totalDF$name),]
  
  yearVector = c()
  for (i in 1:dim(totalDF)[1]){
    currYear = getYear(totalDF[i,],geneal)
    yearVector = c(yearVector, currYear)
  }
  
  totalDF2 = cbind(totalDF, yearVector)
  colnames(totalDF2)[2] = "year"
  totalDF = totalDF2
  
  totalDF = totalDF[order(totalDF$year, decreasing=FALSE), ]
  
  numrows <- ceiling(nrow(totalDF)/length(binVector))
  
  idx <- matrix(1:(numrows*length(binVector)), ncol=length(binVector), nrow=numrows, byrow=TRUE)
  idx <- idx[, binVector]
  idx <- as.numeric(t(idx))[1:nrow(totalDF)]
  
  spreadTotalDF <- totalDF
  spreadTotalDF$y <- jitter(rep(1:numrows, length.out=nrow(totalDF)), amount=.5)[idx]
  
  spreadTotalDF
}

buildEdgeTotalDF = function(geneal, ig, binVector=1:12){
  
  if(class(ig)!="igraph"){
    stop("ig must be an igraph object")
  }
  
  if(sum(1:length(binVector)%in%binVector)!=length(binVector)){
    stop("binVector must contain all numbers 1:length(binVector)")
  }
  
  tG <- buildSpreadTotalDF(geneal, ig, binVector)
  eG <- igraph::get.data.frame(ig, "edges")
  
  # edgeTotalDF used in function plotPathOnAll()
  numEdges = length(igraph::E(ig))
  x=as.numeric(rep("",numEdges))
  y=as.numeric(rep("",numEdges))
  xend=as.numeric(rep("",numEdges))
  yend=as.numeric(rep("",numEdges))
  # For each edge in the graph
  for (i in 1:numEdges){
    xname = as.character(eG[i,]$from)
    xendname = as.character(eG[i,]$to)
    x_i = getYear(xname, tG)
    xend_i = getYear(xendname, tG)
    if(!xname%in%tG$name) {
      stop(paste(xname, "cannot be found in ig vertices"))
    }
    if(!xendname%in%tG$name) {
      stop(paste(xendname, "cannot be found in ig vertices"))
    }
    y_i = tG$y[which(tG$name==xname)]
    yend_i = tG$y[which(tG$name==xendname)]
    x[i] = x_i
    xend[i] = xend_i
    y[i] = y_i
    yend[i] = yend_i
  }
  # Create a dataframe containing the start and end positions of the x and y axes
  edgeTotalDF = as.data.frame(cbind(x, y, xend, yend))
  
  edgeTotalDF
}

buildPlotTotalDF = function(path, geneal, ig, binVector=1:12){
  if(class(ig)!="igraph"){
    stop("ig must be an igraph object")
  }
  
  if(mode(path)=="character"){
    if(length(path)!=2){
      stop("path needs to contain two variety names")
    }
    varieties <- path
    path <- getPath(varieties[1], varieties[2], ig)
  } else if(sum(names(path)%in%c("pathVertices", "yearVertices"))!=2){
    stop("path does not appear to be a result of the getPath() function")
  } 
  
  
  if(sum(1:length(binVector)%in%binVector)!=length(binVector)){
    stop("binVector must contain all numbers 1:length(binVector)")
  }
  
  tG <- buildSpreadTotalDF(geneal, ig, binVector)
  
  label=path$pathVertices
  x=as.numeric(path$yearVertices)
  xstart=x
  xend=rep(0,length(label))
  ystart=rep(0,length(label))
  yend=rep(0,length(label))
  for (i in 2:length(label)){
    ystart[i-1] = tG$y[match(label[i-1], tG$name)]
    yend[i-1] = tG$y[match(label[i], tG$name)]
    xend[i-1] = xstart[i]
  }
  ystart[i] = yend[i-1]
  yend[i] = ystart[i]
  xend[i] = xstart[i]
  y = ystart
  plotTotalDF = data.frame(label,xstart,ystart,xend,yend,x,y)
  
  plotTotalDF
}
