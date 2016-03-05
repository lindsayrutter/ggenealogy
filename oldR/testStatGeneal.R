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
allNameOrig = unique(c(statGeneal$child,statGeneal$parent))
allName = allNameOrig
lengthVec=c()
for (i in 1:length(allName)){
  lengthVec = c(lengthVec, length(getChild(allName[i], statGeneal)))
}
childDat = data.frame(Name = allName, NumChild = lengthVec)

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
mypath=getPath("Peter Bloomfield", "Kay See Tan", ig, statGeneal, isDirected=FALSE)








buildAncDesTotalDF = function(v1, geneal, mAnc=3, mDes=3){
  
  gen <- type <- NULL
  vals = list()
  # Set data frame that we will plot
  gen.vars2 = v1
  vals$gen.vars = gen.vars2
  
  if(length(v1)>0){
    # This ldply statement is converting a list to a datatype. It takes the v1 variety and returns the
    # plot coordinates of all its parents and children.
    temp2 = plyr::ldply(vals$gen.vars, function(i){
      if(i %in% geneal$child | i %in% geneal$parent){
        # This appends the plot coordinates of the data frame constructed for all ancesteors and descendents of the variety
        temp = cbind(variety=i, buildAncDesCoordDF(rbind(nodeToDF(buildAncList(i, geneal)), nodeToDF(buildDesList(i, geneal)))))
        # This create an empty data frame in the event that there are no ancestors nor descendents
        temp$label2 = temp$label
      } 
      # If there is no genetic information, label2 will say "No Information Found" 
      else {
        temp = data.frame(variety=i, label=i, root=NA, root.gen=0, gen=0, type=0, branch=0, x=0, y=0, xstart=0, ystart=0, xend=0, yend=0, branchx=0, branchy=0, id=0, par.id=0, size=2, label2=paste(i, "-", "No Information Found", sep=""))
      }
      # We can remove the columns "id" and "Pair.id" because we do not need them in the actual plot.
      # They were only used to ensure the coordinates were all unique.
      unique(temp[,-which(names(temp)%in%c("id", "par.id"))])
    })
    
    # This creates a unique color for the variety label of interest
    cols = hcl(h=seq(0, 300, by=50), c=80, l=55, fixup=TRUE)
    temp2$color = "#000000"
    
    # The number 7 was selected as it is the average working memory maximum
    if(length(gen.vars2)<=7){
      var.list = which(temp2$label%in%gen.vars2)
      temp2$color[var.list] = cols[as.numeric(factor(temp2$label[var.list]))]
    }
    
    # This is stored separately in case this will be extended to be used for Shiny reactive programming.
    genDF = temp2
    
    temp = merge(data.frame(variety=v1,NewName=vals$gen.vars), plyr::ddply(genDF, "label", plyr::summarise, gen=mean(gen*c(-1,1)[(type=="descendant")+1])), by.x=2, by.y=1)
    vals$match = temp[order(temp$gen, temp$variety),]
  } else {
    genDF = data.frame()
    vals$match = data.frame()
  }
  removeAnc = length(which(genDF$gen > mAnc & genDF$type == "ancestor"))
  removeDes = length(which(genDF$gen > mDes & genDF$type == "descendant"))
  if (removeAnc > 0){
    genDF = genDF[-which(genDF$gen > mAnc & genDF$type == "ancestor"),]
  }
  if (removeDes > 0){
    genDF = genDF[-which(genDF$gen > mDes & genDF$type == "descendant"),] 
  }  
  genDF
}

plotAncDes = function(v1, geneal, mAnc=3, mDes=3, vColor="#D35C79"){
  color <- x <- y <- label2 <- size <- xstart <- ystart <- xend <- yend <- branchx <- branchy <- NULL
  # Plot the data frame, if it exists
  gDF = buildAncDesTotalDF(v1, geneal, mAnc, mDes)
  gDF[gDF$root.gen==0&gDF$gen==0,]$color = vColor
  if(nrow(gDF)>0){
    plotGenImage = ggplot2::qplot(data=gDF, x=x, y=y, label=label2, geom="text", vjust=-.25, hjust=.5, 
                                  size=size, colour=color) +
      ggplot2::geom_segment(ggplot2::aes(x=xstart, y=ystart, xend=xend, yend=yend),inherit.aes=F) + 
      # Draw the underline of the variety
      ggplot2::geom_segment(ggplot2::aes(x=xend, y=yend, xend=branchx, yend=branchy),inherit.aes=F) +
      ggplot2::facet_wrap(~variety, scales="free", ncol=2) +
      ggplot2::scale_size_continuous(range=c(3,3),guide="none") +
      ggplot2::scale_colour_identity() +
      ggplot2::theme_bw() +
      ggplot2::theme(axis.text=ggplot2::element_blank(), 
                     axis.ticks=ggplot2::element_blank()) + 
      ggplot2::scale_x_continuous(expand = c(.1, 1.075)) + 
      ggplot2::scale_y_continuous(expand = c(.1, 1.075)) + 
      ggplot2::labs(x="",y="")
  } else {
    plotGenImage = ggplot2::ggplot() + 
      ggplot2::geom_text(ggplot2::aes(x=0, y=0, label="Please select varieties\n\n Note: It may take a moment to process the v1")) +         
      ggplot2::theme_bw() + 
      ggplot2::theme(axis.text=ggplot2::element_blank(), 
                     axis.ticks=ggplot2::element_blank(), 
                     axis.title=ggplot2::element_blank()) +
      ggplot2::labs(x="",y="")
  }
  plotGenImage
}



















# Jan de Leeuw had path of 3
# Mike West had path of 5
# Dipak Dey had path of 3

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

