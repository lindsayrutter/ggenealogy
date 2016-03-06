load("data/statGeneal.rda")
geneal = statGeneal
statGeneal = statGeneal[-which(is.na(statGeneal$year)),]
statGeneal = statGeneal[!duplicated(statGeneal$child),]
myNode = statGeneal[,-which(names(statGeneal) %in% c("parent"))]

rowRemove = c()
for (i in 1:nrow(statGeneal)){
  if (!statGeneal[i,]$parent %in% statGeneal$child){
    rowRemove = c(rowRemove, i)
  }
}
myEdge = statGeneal[-(rowRemove), which(names(statGeneal) %in% c("child","parent"))]

#statGeneal = statGeneal[1:500,]
getChild("Christian Robert",statGeneal)
ig <- dfToIG(statGeneal)
getYear("Nicolas Chopin",statGeneal)
getAncestors("Chana Lowenstein",statGeneal,3)
getParent("Shlomo Sawilowsky",statGeneal)
getDegree("Boris Shulkin", "R. Clifford Blair", ig, statGeneal)
getBasicStatistics(ig)
statGeneal[which(statGeneal$child=="Boris Shulkin"),]


pathCC = getPath("Scott Zeger","Kay See Tan", ig, statGeneal, isDirected=FALSE)
# Have not dealt with cases where year is NA
# Possibly imputation?? (halfway between parent and child)
plotPath(pathCC)

# see statistics professors with most statistics children 
allIndvl = unique(c(statGeneal$child,statGeneal$parent)) #3613
lengthVec=c()
for (i in 1:length(allIndvl)){
  lengthVec = c(lengthVec, nrow(getDescendants(allIndvl[i], statGeneal, gen = 8)))
}
childDat = data.frame(Name = allIndvl, NumChild = lengthVec)


library(dplyr)
#allIndvl = unique(c(statGeneal$child,statGeneal$parent)) #3613
descDat <- data.frame(Name = unique(c(statGeneal$child, statGeneal$parent)))
descDat <- transform(descDat, FUN = nrow(getDescendants(as.character(Name), statGeneal, gen = 8)))
#getNumDesc = function(a){
#}
descDat <- apply(descDat[,'Name'], 1, function(x) testFunc[x(1)])
descDat <- apply(descDat[,'Name'], 1, function(x) nrow(getDescendants(as.character(x), statGeneal, gen = 8)))


testFunc <- function(a) nrow(getDescendants(as.character(a), statGeneal, gen = 8))
lapply(as.character(descDat[,c('Name')]), function(x) testFunc(x[1]))
res = lapply(as.character(descDat[,c('Name')]), function(x) testFunc(x[1]))

#WORKS
#res = lapply(descDat[,c('Name')], function(x) testFunc(x[1]))

dat <- data.frame(x=c(1,2), y=c(3,4), z=c(5,6))
testFunc <- function(a, b) a + b
apply(dat[,c('x','z')], 1, function(x) testFunc(x[1],x[2]))

descDat <- unique(c(statGeneal$child, statGeneal$parent))
testFunc <- function(a) nrow(getDescendants(a, statGeneal, gen = 8))
res=sapply(descDat, testFunc)
table(res)
which(res==159)

library(stringi)
stri_rand_lipsum(1)

words <- data.frame(Word = )


lengthVec=c()
for (i in 1:length(allIndvl)){
  lengthVec = c(lengthVec, nrow(getDescendants(allIndvl[i], statGeneal, gen = 8)))
}


descDat = data.frame(Name = allIndvl, NumChild = nrow(getDescendants(allIndvl[1:3613], statGeneal, gen = 8)))

# dplyr arrange can be used to sort column length in DF male
alea <- arrange(male,length)
# sort descending maled <- arrange(male,desc(length))
ruffeLW <- mutate(ruffeLW,logL=log(length),logW=log(weight))
head(ruffeLW)

name1Vec = c()
name2Vec = c()
shortPathVec=c()
for (i in 1:(length(allName)-1)){
  for (j in (i+1):(length(allName))){
    name1Vec = c(name1Vec, allName[i])
    name2Vec = c(name2Vec, allName[j])
    shortPathVec = c(shortPathVec, length(getPath(allName[i], allName[j], ig, statGeneal, isDirected=FALSE))$pathVertices)
  }
}

allName=allNameOrig[-which(allName=="Mike West")]
shortPathVec=c()
vecName1 = c()
for (i in 1:length(allName)){
  vecName1 = c(vecName1, allName[i])
  shortPathVec = c(shortPathVec, length(getPath("Mike West",allName[i], ig, statGeneal, isDirected=FALSE)$pathVertices))
}

plotAncDes("David Cox", statGeneal, mAnc = 6, mDes = 6, vCol = "blue")
mypath=getPath("David Cox", "Kay See Tan", ig, statGeneal, isDirected=FALSE)


#plotPathOnAll(pathCC, statGeneal, ig, binVector = sample(1:12, 12))
# Probably don't need to write out full name (just points/dots)
# Important points emphasized (are all descended from a few people? )
# Standard network (remove year), but could see people who have lots of advisees (ggNet)

p = plotPathOnAllAn(pathCC, statGeneal, ig, binVector = 1:100)

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
  
  if (sum(which(is.na(geneal$year)))>0){
    geneal = geneal[-which(is.na(geneal$year)),] 
  }
  geneal = geneal[!duplicated(geneal$child),]
  myNode = geneal[,-which(names(geneal) %in% c("parent"))]
  
  rowRemove = c()
  for (i in 1:nrow(geneal)){
    if (!geneal[i,]$parent %in% geneal$child){
      rowRemove = c(rowRemove, i)
    }
  }
  myEdge = geneal[-(rowRemove), which(names(geneal) %in% c("child","parent"))]
  
  # All node labels and their x, y position (label, x, y)
  pMPDF <- buildMinusPathDF(path, geneal, ig, binVector)
  
  pMPDF2 = pMPDF[-which(is.na(pMPDF$label)),]
  
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
  
  tG <- buildSpreadTotalDF(myNode, binVector)
  eG <- igraph::get.data.frame(ig, "edges")
  
  label=tG$child
  x=tG$year
  y=tG$y
  # If the label is part of the path, then we change its value to NA
  for (i in 1:length(label)){
    if (label[i]%in%path$pathVertices){
      label[i]=NA
    }
  }
  plotMinusPathDF = data.frame(label,x,y)
  
  # Return the data frame object of the full genealogy
  plotMinusPathDF
}

buildSpreadTotalDF = function(myNode, binVector=1:12){
  if(class(ig)!="igraph"){
    stop("ig must be an igraph object.")
  }
  
  if(sum(1:length(binVector)%in%binVector)!=length(binVector)){
    stop("binVector must contain all numbers 1:length(binVector)")
  }
  
  #totalDF = igraph::get.data.frame(ig, "vertices")
  #totalDF = totalDF[!is.na(totalDF$name),]
  
  #geneal = geneal[-which(is.na(geneal$year)),]
  #geneal = geneal[!duplicated(geneal$child),]
  #myNode = geneal[,which(names(geneal) %in% c("child","year"))]
  
  #yearVector = c()
  #for (i in 1:dim(totalDF)[1]){
  #  currYear = getYear(totalDF[i,],geneal)
  #  yearVector = c(yearVector, currYear)
  #}
  
  #totalDF2 = cbind(totalDF, yearVector)
  #colnames(totalDF2)[2] = "year"
  #totalDF = myNode
  
  myNode = myNode[order(myNode$year, decreasing=FALSE), ]
  
  numrows <- ceiling(nrow(myNode)/length(binVector))
  
  idx <- matrix(1:(numrows*length(binVector)), ncol=length(binVector), nrow=numrows, byrow=TRUE)
  idx <- idx[, binVector]
  idx <- as.numeric(t(idx))[1:nrow(myNode)]
  
  spreadTotalDF <- myNode
  spreadTotalDF$y <- jitter(rep(1:numrows, length.out=nrow(myNode)), amount=.5)[idx]
  
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


######################## MJS_PLOT WORK ######################## 
library(metricsgraphics)
myDF = mtcars
myDF = cbind(Row.Names = rownames(myDF), myDF)
rownames(myDF) <- NULL
colnames(myDF)[1] = "carName"

mjs_plot(myDF, x=wt, y=mpg) %>%
  mjs_point(color_accessor=carb, size_accessor=carb) %>%
  #mjs_add_line(qsec) %>%
  mjs_labs(x="Weight of Car", y="Miles per Gallon")

p = getAllDF(pathCC, statGeneal, ig, binVector = 1:100)

###################### FORCE NETWORK ##########################

data(MisLinks)

################################################################

# Copy and tweak code from https://github.com/hrbrmstr/metricsgraphics

mjs_plot <- function(data, x, y,
                     show_rollover_text = TRUE,
                     linked = FALSE,
                     decimals=2, format="count",
                     missing_is_hidden=FALSE,
                     left = 80, right = 10,
                     top = 40, bottom = 60, buffer = 8,
                     width = NULL, height = NULL) {
  
  if (!format %in% c("percentage", "count")) {
    stop("'format' must be either 'percentage' or 'count'")
  }
  
  eid <- sprintf("mjs-%s",
                 paste(sample(c(letters[1:6], 0:9), 30, replace=TRUE), collapse=""))
  
  if (!missing(x)) {
    x <- substitute(x)
    res <- try(eval(x, data, parent.frame()), silent = TRUE)
    if (!inherits(res, "try-error") && inherits(res, "character"))
      x <- res
    else if (inherits(x, "name")) { x <- as.character(x) }
  } else {
    x <- as.character(substitute(x))
  }
  
  if (!missing(y)) {
    y <- substitute(y)
    res <- try(eval(y, data, parent.frame()), silent = TRUE)
    if (!inherits(res, "try-error") && inherits(res, "character"))
      y <- res
    else if (inherits(y, "name")) { y <- as.character(y) }
  } else {
    y <- as.character(substitute(y))
  }
  
  is_datetime <- function(x) {
    inherits(x, c('Date', 'POSIXct', 'POSIXlt'))
  }
  
  is_posix <- function(x) {
    inherits(x, c('POSIXct', 'POSIXlt'))
  }
  
  orig_posix <- FALSE
  if (is.null(dim(data))) {
    if (is_posix(data)) orig_posix <- TRUE
  } else if (is_posix(data[, x])) {
    orig_posix <- TRUE
  }
  
  if (is.null(dim(data))) {
    if (is_datetime(data)) data <- as.numeric(data)
  } else if (is_datetime(data[, x])) {
    data[, x] <- as.numeric(data[, x])
  }
  
  params = list(
    orig_posix=orig_posix,
    data=data,
    x_axis=TRUE,
    y_axis=TRUE,
    baseline_accessor=NULL,
    predictor_accessor=NULL,
    show_confidence_band=NULL,
    show_secondary_x_label=NULL,
    chart_type="line",
    xax_format="plain",
    x_label=NULL,
    y_label=NULL,
    markers=NULL,
    baselines=NULL,
    linked=linked,
    title=NULL,
    description=NULL,
    left=left,
    right=right,
    bottom=bottom,
    buffer=buffer,
    format=format,
    y_scale_type="linear",
    yax_count=5,
    xax_count=6,
    x_rug=FALSE,
    y_rug=FALSE,
    area=FALSE,
    missing_is_hidden=missing_is_hidden,
    size_accessor=NULL,
    color_accessor=NULL,
    color_type="number",
    color_range=c("blue", "red"),
    size_range=c(1, 5),
    bar_height=20,
    min_x=NULL,
    max_x=NULL,
    min_y=NULL,
    max_y=NULL,
    bar_margin=1,
    binned=FALSE,
    bins=NULL,
    least_squares=FALSE,
    interpolate="cardinal",
    decimals=decimals,
    show_rollover_text=show_rollover_text,
    x_accessor=x,
    y_accessor=y,
    multi_line=NULL,
    geom="line",
    yax_units="",
    legend=NULL,
    legend_target=NULL,
    y_extended_ticks=FALSE,
    x_extended_ticks=FALSE,
    target=sprintf("#%s", eid)
  )
  
  if (is.null(width)) params$full_width <- TRUE
  if (is.null(height)) params$full_height <- TRUE
  
  htmlwidgets::createWidget(
    name = 'metricsgraphics',
    x = params,
    width = width,
    height = height,
    package = 'metricsgraphics',
    elementId = eid
  )
  
}

##################################################################
##################################################################
##################################################################
##################################################################

getAllDF = function(path, geneal, ig, binVector=sample(1:12, 12)){
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
  
  pMPDF <- buildMinusPathDF(path, geneal, ig, binVector)
  eTDF <- buildEdgeTotalDF(geneal, ig, binVector)
  pTDF <- buildPlotTotalDF(path, geneal, ig, binVector)
  
  allDF = list(pMPDF=pMPDF, eTDF=eTDF, pTDF=pTDF)
  return(allDF)
}



######################## ANIMINT WORK ######################## 
###################################################################
###################################################################

p = plotPathOnAllAn(pathCC, statGeneal, ig, binVector = 1:100)
p2 = plotPathOnAllAn2(pathCC, statGeneal, ig, binVector = 1:100)

library(animint)
animint2dir(list(plot = p, plot2 = p2), out.dir = "animintAll", open.browser = FALSE)

########################## Tweak plotPathOnAll ########################## 

