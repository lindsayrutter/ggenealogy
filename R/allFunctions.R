#' Returns the coordinate positions of all ancestors and descendants of a variety.
#' 
#' Calculates coordinates to plot each ancestors and descendant of a variety in a lineage. The x and y values describe the coordinates of the label, while the xstart, ystart, xend, and yend values describe the edges of the label.
#' 
#' @param df the data frame of the ancestors and descendants of a variety (from function buildAncDesTotalDF)
#' @seealso \code{\link{buildAncList}} for information on determining ancestors
#' @seealso \code{\link{buildDesList}} for information on determining descendants
buildAncDesCoordDF = function(df){
  id.offset <- NULL
  root.gen <- gen <- par.id <- label <- NULL 
  # This gets rid of redundancy and creates a "center"
  if(nrow(subset(df, root.gen==0 & gen==0))>1){
    temp = subset(df, root.gen==0 & gen==0)
    temp$type = "center"
    old.ids = temp$id[-1]
    df$par.id[which(df$par.id%in%old.ids)] = temp$id[1]
    temp$id[-1] = temp$id[1]
    temp = unique(temp)
    df = plyr::rbind.fill(subset(df, !(root.gen==0 & gen==0)), temp)
  }
  # This adds a leaf boolean column
  df$leaf = !df$id%in%df$par.id
  # Initialize x, y coords, and genside
  df$genside = df$gen*(df$type=="descendant")-df$gen*(df$type=="ancestor")
  df$x = 0
  df$y = 0
  # Ensure y coords are spread among all branches
  df$y[df$leaf & df$genside<0] = seq(1, sum(df$leaf), length.out=sum(df$leaf & df$genside<0))
  # but later, 2 ancestors are taken off b/c too previous
  df$y[df$leaf & df$genside>0] = seq(1, sum(df$leaf), length.out=sum(df$leaf & df$genside>0))
  # Ensure a single branch on one side is centrally placed (what if only 2 or 3?)
  if(sum(df$leaf & df$genside<0)==1) df$y[df$leaf & df$genside<0] = sum(df$leaf)/2
  if(sum(df$leaf & df$genside>0)==1) df$y[df$leaf & df$genside>0] = sum(df$leaf)/2
  
  df$xstart = 0
  df$ystart = 0
  df$xend = 0
  df$yend = 0
  df$branchx = 0
  df$branchy = 0
  
  # Sort data frame
  df = df[rev(order(df$type, df$root.gen, df$gen, df$par.id, df$branch)),]
  # set y coordinates to start furthest away from center
  #( Ex. -5 -4 -3  3 -2  2 -1  1  0 )
  for(i in unique(df$genside)[rev(order(abs(unique(df$genside))))]){
    kids = subset(df, df$genside==i)
    for(j in unique(kids$par.id)){
      df$y[df$id==j] = mean(subset(kids, par.id==j)$y)
    }    
  }
  
  yfac = diff(range(df$y))  
  df$ystart = df$y
  df$yend = df$y
  
  # Maximum character length of each generation
  widths = plyr::ddply(df, "genside", plyr::summarise, len=max(nchar(as.character(label))))
  # Create a padding for label length
  widths$len = widths$len*(1+yfac/(1+yfac))
  gap = mean(widths$len)/2
  
  # For each generation
  for(i in unique(df$genside)[order(abs(unique(df$genside)))]){
    # j is the rows (varieties) in that generation
    j = which(df$genside==i)
    # Center of the two lineages 
    if(i == 0){   
      df$x[j] = 0
      df$xstart[j] = -widths$len[widths$genside==i]/2
      df$xend[j] = widths$len[widths$genside==i]/2
      df$branchx[j] = df$xend[j] # make the "branch" for this node nonexistent
      df$branchy[j] = df$yend[j]
      # If first descendent
    } else if(i==1){
      for(k in j){
        par = df[which(df$label==as.character(df$root[k])),]
        # Only the center node has no "parents"         
        df$branchx[k] = par$xend
        df$branchy[k] = par$yend
        df$xend[k] = df$branchx[k]+gap*sign(i)
        df$xstart[k] = df$xend[k]+widths$len[widths$genside==i]*sign(i)
        df$x[k] = (df$xend[k]+df$xstart[k])/2
      } 
      # The positive side of lineage ("reversed" branch)
    } else if(i>1){
      for(k in j){
        par = df[which(df$id==df$par.id[k]),]
        df$branchx[k] = par$xstart
        df$branchy[k] = par$ystart
        df$xend[k] = df$branchx[k]+gap*sign(i)
        df$xstart[k] = df$xend[k]+widths$len[widths$genside==i]*sign(i)
        df$x[k] = (df$xend[k]+df$xstart[k])/2
      } 
      # The negative side of lineage
    } else {
      for(k in j){
        par = df[which(df$id==df$par.id[k]),]
        # Only the center node has no "parents" 
        df$branchx[k] = par$xstart
        df$branchy[k] = par$ystart
        df$xend[k] = df$branchx[k]+gap*sign(i)
        df$xstart[k] = df$xend[k]+widths$len[widths$genside==i]*sign(i)
        df$x[k] = (df$xend[k]+df$xstart[k])/2
      } 
    }
  }
  
  df$size = 1
  return(df)
}

#' Returns data frame with plot coordinates of all ancestors and descendants of a variety.
#' 
#' Returns the data frame that includes labels and plot coordinates of all ancestors and descendants of a variety. Users can specify the maximum number of ancestors and descendants to display.
#' 
#' @param v1 the label of the vertex of interest (in character string format)
#' @param mAnc the maximum number of generations of ancestors of v1 to be displayed (in numeric format)
#' @param mDes the maximum number of generations of descendants of v1 to be displayed (in numeric format)
#' @param geneal the full genealogy (in data frame format)
#' @seealso \code{\link{buildAncList}} for information on determining ancestors
#' @seealso \code{\link{buildDesList}} for information on determining descendants
#' @export
#' @examples
#' data(sbGeneal)
#' v1 <- "Essex"
#' buildAncDesTotalDF(v1, sbGeneal)
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
    cols = grDevices::hcl(h=seq(0, 300, by=50), c=80, l=55, fixup=TRUE)
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

#' Returns the ancestors of a particular variety (if they exist).
#' 
#' This function returns a nested list of the ancestors of the inputted variety.
#' 
#' @param v1 the label of the vertex of interest (in character string format)
#' @param geneal the full genealogy  (in data frame format)
#' @param gen the generation (note: This should be left as default, as any other input will not affect results anyway)
#' @seealso \code{\link{getParent}} for information on determining parents
#' @export
#' @examples
#' data(sbGeneal)
#' getParent("Essex", sbGeneal)
#' buildAncList("Essex", sbGeneal)
buildAncList = function(v1, geneal, gen = 0){
  if(is.na(v1)) return()
  
  temp = getParent(v1, geneal)
  if(length(temp)==0) return()
  
  res = lapply(temp[!is.na(temp)], function(i){
    # print(i)
    temp2 = buildAncList(i, geneal, gen=gen+1)
    if(length(temp2)<1) return(list(label=i, root=v1, root.gen=gen, gen=gen+1, type="ancestor"))
    return(c(label=i, root=v1, root.gen=gen, gen=gen+1, type="ancestor", temp2))
  })
  
  if(gen==0){
    return(c(label=v1, root=v1, root.gen=gen, gen=gen, type="ancestor", res))
  } else{
    return(res)
  } 
}

#' Returns the descendants of a particular variety (if they exist).
#' 
#' This function returns a nested list of the descendants of the inputted variety.
#' 
#' @param v1 the label of the vertex of interest (in character string format)
#' @param geneal the full genealogy  (in data frame format)
#' @param gen the generation (note: This should be left as default, as any other input will not affect results)
#' @seealso \code{\link{getChild}} for information on determining children
#' @export
#' @examples
#' data(sbGeneal)
#' getParent("Essex", sbGeneal)
#' buildDesList("Essex", sbGeneal, 3)
buildDesList = function(v1, geneal, gen=0){
  if(is.na(v1)) return()
  
  temp = getChild(v1, geneal)
  if(length(temp)==0) return()
  
  res = lapply(temp[!is.na(temp)], function(i){
    # print(i)
    temp2 = buildDesList(i, geneal, gen=gen+1)
    if(length(temp2)<1) return(list(label=i, root=v1, root.gen=gen, gen=gen+1, type="descendant"))
    return(c(label=i, root=v1, root.gen=gen, gen=gen+1, type="descendant", temp2))
  })
  
  if(gen==0){
    return(c(label=v1, root=v1, root.gen=gen, gen=gen, type="descendant", res))
  } else{
    return(res)
  } 
}

#' Build the edges in the genealogy graph.
#' 
#' This function takes the graph object and creates a data frame object of the edges between all parent-child relationships in the graph.
#' 
#' @param geneal the full genealogy  (in data frame format)
#' @param ig the graph representation of the data genealogy (in igraph format)
#' @param bin the number of bins to determine the vertical positions of nodes (default is 12). For more information on choosing bin size, please visit the ggenealogy vignette.
#' @param colName the name of the column of the data frame that contains the quantitative variable of interest (in character string format)
#' @seealso \code{\link{dfToIG}} for information on producing ig from the genealogy
#' @seealso \url{http://www.r-project.org} for iGraph information
buildEdgeTotalDF = function(geneal, ig, colName, bin = 12){
  
  if(class(ig)!="igraph"){
    stop("ig must be an igraph object")
  }
  
  if(class(bin) != "numeric"){
    stop("bin must contain a number")
  }
  
  tG <- buildSpreadTotalDF(geneal, ig, colName, bin)
  eG <- igraph::get.data.frame(ig, "edges")
  
  # edgeTotalDF used in function plotPathOnAll()
  numEdges = length(igraph::E(ig))
  x=as.numeric(rep("",numEdges))
  y=as.numeric(rep("",numEdges))
  xend=as.numeric(rep("",numEdges))
  yend=as.numeric(rep("",numEdges))
  name=as.numeric(rep("",numEdges))
  nameEnd=as.numeric(rep("",numEdges))
  # For each edge in the graph
  for (i in 1:numEdges){
    xname = as.character(eG[i,]$from)
    xendname = as.character(eG[i,]$to)
    x_i = getVariable(xname, tG, colName)
    xend_i = getVariable(xendname, tG, colName)
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
    name[i] = xname
    nameEnd[i] = xendname
  }
  # Create a dataframe containing the start and end positions of the x and y axes
  edgeTotalDF = as.data.frame(cbind(x, y, xend, yend, name, nameEnd))
  
  edgeTotalDF
}

#' Process the genealogy graph
#' 
#' This function takes the spreadTotalDF object (from the buildSpreadTotalDF function) and the path object
#' as inputs. From these objects, it creates a data frame object of the label, x, and y values of all nodes
#' in the ful genealogy. However, the data frame object does not include the labels of the path varieties, as they
#' will be treated differently.
#' @param path path as returned from getPath() or a vector of two variety names which exist in the ig object
#' @param geneal the full genealogy  (in data frame format)
#' @param ig the graph representation of the data genealogy (in igraph format)
#' @param colName the name of the column of the data frame that contains the quantitative variable of interest (in character string format)
#' @param colNameY the name of the second optional column of the data frame that contains the second optional quantitative variable of interest (in character string format). This optional quantitative variable will be plotted on the vertical axis.
#' @param bin the number of bins to determine the vertical positions of nodes (default is 12). For more information on choosing bin size, please visit the ggenealogy vignette.
#' @seealso \url{http://www.r-project.org} for iGraph information
#' @seealso \code{\link{getPath}} for information on input path building
buildMinusPathDF = function(path, geneal, ig, colName, colNameY, bin = 12){
  
  if(class(ig)!="igraph"){
    stop("ig must be an igraph object")
  }
  
  if(class(bin) != "numeric"){
    stop("bin must contain a number")
  }
  
  if(mode(path)=="character"){
    if(length(path)!=2){
      stop("path needs to contain two variety names")
    }
    varieties <- path
    path <- getPath(varieties[1], varieties[2], ig, colName)
  } else if(sum(names(path)%in%c("pathVertices", "variableVertices"))!=2){
    stop("path does not appear to be a result of the getPath() function")
  } 
  
  tG <- buildSpreadTotalDF(geneal, ig, colName, bin)
  #eG <- igraph::get.data.frame(ig, "edges")
  
  if (colNameY !=""){
    rowNametG <- rownames(tG)
    rownames(tG) <- 1:nrow(tG)
    rowNameG <- rownames(geneal)
    rownames(geneal) <- 1:nrow(geneal)
    tG$y <- geneal[match(tG$name, geneal$child),][[colNameY]]
    rownames(tG) <- rowNametG
    rownames(geneal) <- rowNameG
  }
  
  label=tG$name
  x=tG[[colName]]
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

#' Build data frame for path representation
#' 
#' This function builds a dataframe of information about the path object that can later be used
#' for visualization. The dataframe includes "label" (name of each variety) of each node,
#' "x" (the date of the variety, the x-axis value for which the label and
#' incoming/outgoing edges are centered), "y" (the y-axis value, which is the index
#' of the path, incremented by unity), "xstart" (the x-axis position of the 
#' outgoing edge (leaving to connect to the node at the next largest y-value)),
#' "xend" (the x-axis position of the outgoing edge (connected to the node at the
#' next largest y-value)), "ystart" (the y-axis position of the outgoing edge (leaving
#' to connect to the node at the next largest y-value), "yend" (the y-axis position
#' of the outgoing edge (connected to the node at the next largest y-value))).
#' @param path path object representing the path between two vertices
#' @param geneal the full genealogy  (in data frame format)
#' @param colName the name of the column of the data frame that contains the quantitative variable of interest (in character string format)
#' @param colNameY the name of the second optional column of the data frame that contains the second optional quantitative variable of interest (in character string format). This optional quantitative variable will be plotted on the vertical axis.
buildPathDF = function(path, geneal, colName, colNameY=""){
  if(length(path) > 0){
    # The labels of the nodes are the names of the varieties in the path
    label=path$pathVertices
    # The x-axis position of the node labels are the dates of the varieties in the path
    x=as.numeric(path$variableVertices)
    # The y-axis position of the node labels are incremented by unity for each new connected
    # node in the path
    if (colNameY==""){
      y=seq(1, length(label), 1)
    }
    else{
      y =geneal[match(label, geneal$child),][[colNameY]]
    }
    # The starting x-axis position of the edge between two nodes will be the x-axis position
    # of the label of the first node
    xstart=x
    xend=rep(0,length(label))
    yend=rep(0,length(label))
    for (i in 1:length(label)){
      xend[i] = xstart[i]
      if (i < length(label)){
        # The ending x-axis position of the edge between two nodes will be the x-axis position
        # of the label of the second node
        xend[i] = xstart[i+1]
        # The ending y-axis position of the edge between two nodes will be 0.1 below the y-axis
        # of the label of the second node
        yend[i] = y[i+1]-.1
      }
    }
    # The starting y-axis position of the edge between two nodes will be 0.1 above the y-axis of
    # the label of the first node
    ystart = y+.1
    # The last edge should have equal y-axis values of the ends of its segment because it should
    # not be drawn (as there are n-1 edges for n nodes of a graph)
    yend[length(label)] = ystart[length(label)]
    # Create the dataframe about the path that includes the 7 necessary parameters
    plotPathDF = data.frame(label,xstart,ystart,xend,yend,x,y)
    # Return the dataframe about the path
  }
  else{
    print("Warning: There was no plotPathDF object created")
    plotPathDF = NA
  }
  plotPathDF
}

#' Build all labels in the graph
#' 
#' This function takes the spreadTotalDF object (from the buildSpreadTotalDF function) and the path object
#' as inputs. From these objects, it creates a data frame object of the text label positions for the
#' varieties in the path, as well as the edges only in the varieties in the path.
#' @param path path as returned from getPath() or a vector of two variety names which exist in ig
#' @param geneal the full genealogy  (in data frame format)
#' @param ig the graph representation of the data genealogy (in igraph format)
#' @param colName the name of the column of the data frame that contains the quantitative variable of interest (in character string format)
#' @param colNameY the name of the second optional column of the data frame that contains the second optional quantitative variable of interest (in character string format). This optional quantitative variable will be plotted on the vertical axis.
#' @param bin the number of bins to determine the vertical positions of nodes (default is 12). For more information on choosing bin size, please visit the ggenealogy vignette
#' @seealso \url{http://www.r-project.org} for iGraph information
#' @seealso \url{http://www.r-project.org} for iGraph information
#' @seealso \code{\link{getPath}} for information on input path building
buildPlotTotalDF = function(path, geneal, ig, colName, colNameY = "", bin = 12){
  if(class(ig)!="igraph"){
    stop("ig must be an igraph object")
  }
  
  if(mode(path)=="character"){
    if(length(path)!=2){
      stop("path needs to contain two variety names")
    }
    varieties <- path
    path <- getPath(varieties[1], varieties[2], ig, colName)
  } else if(sum(names(path)%in%c("pathVertices", "variableVertices"))!=2){
    stop("path does not appear to be a result of the getPath() function")
  } 
  
  if(class(bin) != "numeric"){
    stop("bin must contain a number")
  }
  
  tG <- buildSpreadTotalDF(geneal, ig, colName, bin)
  
  if (colNameY !=""){
    rowNametG <- rownames(tG)
    rownames(tG) <- 1:nrow(tG)
    rowNameG <- rownames(geneal)
    rownames(geneal) <- 1:nrow(geneal)
    tG$y <- geneal[match(tG$name, geneal$child),][[colNameY]]
    rownames(tG) <- rowNametG
    rownames(geneal) <- rowNameG
  }
  
  label=path$pathVertices
  x=as.numeric(path$variableVertices)
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

#' Build a data frame where the varieties are spread so they do not overlap
#' 
#' Constructs a data frame object so that varieties are spread such that they do not overlap, even
#' though the x-axis position will represent dates.
#' @param geneal the full genealogy  (in data frame format)
#' @param ig the graph representation of the data genealogy (in igraph format)
#' @param colName the name of the column of the data frame that contains the quantitative variable of interest (in character string format)
#' @param bin the number of bins to determine the vertical positions of nodes (default is 12). For more information on choosing bin size, please visit the ggenealogy vignette
#' @seealso \url{http://www.r-project.org} for iGraph information
buildSpreadTotalDF = function(geneal, ig, colName, bin = 12){
  if(class(ig)!="igraph"){
    stop("ig must be an igraph object.")
  }
  
  if(class(bin) != "numeric"){
    stop("bin must contain a number")
  }
  
  totalDF = igraph::get.data.frame(ig, "vertices")
  #totalDF = totalDF[!is.na(totalDF$name),]
  
  dateVector = c()
  for (i in 1:dim(totalDF)[1]){
    currYear = getVariable(totalDF[i,],geneal,colName)
    dateVector = c(dateVector, currYear)
  }
  
  totalDF2 = cbind(totalDF, dateVector)
  colnames(totalDF2)[2] = colName
  totalDF = totalDF2
  
  totalDF = totalDF[order(totalDF[[colName]], decreasing=FALSE), ]
  
  numrows <- ceiling(nrow(totalDF)/bin)
  
  idx <- matrix(1:(numrows*bin), ncol=bin, nrow=numrows, byrow=TRUE)
  idx <- idx[, 1:bin]
  idx <- as.numeric(t(idx))[1:nrow(totalDF)]
  
  spreadTotalDF <- totalDF
  spreadTotalDF$y <- jitter(rep(1:numrows, length.out=nrow(totalDF)), amount=.5)[idx]
  
  spreadTotalDF
}

#' Returns a list of the ancestors of a particular variety (if they exist)
#' 
#' This function returns a list of the ancestors of the inputted variety within and including a given number of generations
#' 
#' @param v1 the label of the vertex of interest (in character string format)
#' @param geneal the full genealogy  (in data frame format)
#' @param gen the number of generations back to include as ancestors
#' @export
#' @examples
#' data(sbGeneal)
#' getParent("Essex", sbGeneal)
#' getAncestors("Essex", sbGeneal, 1)
#' getAncestors("Essex", sbGeneal, 5)
getAncestors = function(v1, geneal, gen = 3){
  id.offset <- NULL
  if (is.null(buildAncList(v1, geneal))){
    return(data.frame())
  }
  aDF = buildAncDesCoordDF(nodeToDF(buildAncList(v1, geneal)))
  subDF = aDF[aDF$gen <= gen & aDF$gen != 0,]
  keep = c("label","gen")
  subDF = subDF[keep]
  row.names(subDF) = NULL
  return(subDF[order(subDF$gen,subDF$label),])  
}

#' Determine basic statistics of the graph object
#' 
#' Returns basic statistics of the graph object (number of nodes, number of edges, whether or not the
#' whole graph is connected, number of components, average path length, graph diameter, etc.)
#' @param ig the graph representation of the data genealogy (in igraph format)
#' @examples
#' data(sbGeneal)
#' ig <- dfToIG(sbGeneal)
#' getBasicStatistics(ig)
#' @export
getBasicStatistics = function(ig){
  if(class(ig)!="igraph"){
    stop("ig must be an igraph object.")
  }
  retStats = list()
  # Get edge and node count from "structure.info" function of igraph
  numNodes = igraph::vcount(ig)
  numEdges = igraph::ecount(ig)
  # Determine if the graph is connected or not from "clusters" function of igraph
  isConnected = igraph::is.connected(ig)
  # Determine the number of connected components in the graph from "clusters"
  # function of igraph
  numComponents = igraph::no.clusters(ig)
  # Compute the average path length of the graph
  connected = FALSE
  if(isConnected)
  {
    connected = TRUE
  }
  avePathLength = igraph::average.path.length(ig, directed=F, unconnected= !isConnected)
  # Determine the log(N) value of the graph
  logN = log(numNodes)
  # Determine the network diameter
  graphDiameter = igraph::diameter(ig, directed = F, unconnected = !isConnected, weights = NULL)
  # Create a list of statistics
  retStats = list(isConnected = isConnected, numComponents = numComponents, avePathLength = avePathLength,
                  graphDiameter = graphDiameter, numNodes = numNodes, numEdges = numEdges, logN = logN)
  # Return the list of statistis
  retStats
}

#' Returns a list of the descendants of a particular variety (if they exist)
#' 
#' This function returns a list of the descendants of the inputted variety within and including a given number of generations
#' 
#' @param v1 the label of the vertex of interest (in character string format)
#' @param geneal the full genealogy  (in data frame format)
#' @param gen the number of generations back to include as descendants
#' @export
#' @examples
#' data(sbGeneal)
#' getChild("Essex", sbGeneal)
#' getDescendants("Essex", sbGeneal, 1)
#' getDescendants("Essex", sbGeneal, 3)
getDescendants = function(v1, geneal, gen=3){
  id.offset <- NULL
  if (is.null(buildDesList(v1, geneal))){
    return(data.frame())
  }
  dDF = buildAncDesCoordDF(nodeToDF(buildDesList(v1, geneal)))
  subDF = dDF[dDF$gen <= gen & dDF$gen != 0,]
  keep = c("label","gen")
  subDF = subDF[keep]
  row.names(subDF) = NULL
  return(subDF[order(subDF$gen,subDF$label),]) 
}

#' Returns edges (vertex names and edge weights) for the full genealogy
#'
#' Returns a matrix, where each row contains information about an edge (two vertex names and edge weight, if present) of the full genealogy.
#'   
#' @param ig the graph representation of the data genealogy (in igraph format)
#' @param geneal the full genealogy  (in data frame format)
#' @examples
#' data(sbGeneal)
#' ig <- dfToIG(sbGeneal)
#' getEdges(ig, sbGeneal)
#' @export
getEdges = function(ig, geneal){
  eList = igraph::get.edgelist(ig)
  for (i in 1:dim(eList)[1]){
    if (!isChild(eList[i,1],eList[i,2], geneal)){
      p = eList[i,1]
      c = eList[i,2]
      eList[i,1] = c
      eList[i,2] = p
    }
    colnames(eList) = c("child","parent")
  }
  eList
}

#' Returns the children of a particular variety (if they exist)
#' 
#' This function returns zero or more values that indicate the children of the inputted variety.
#' 
#' @param v1 the label of the vertex of interest (in character string format)
#' @param geneal the full genealogy  (in data frame format)
#' @examples
#' data(sbGeneal)
#' getChild("Tokyo", sbGeneal)
#' getChild("Essex", sbGeneal)
#' @export
getChild = function(v1, geneal){
  parent <- NULL
  sort(subset(geneal, parent==v1)$child)
}

#' Determine the degree between two varieties
#' 
#' Returns the degree (distance between unweighted edges) between two varieties, where an edge
#' represents a parent-child relationship
#' @param v1 the label of the first vertex of interest (in character string format)
#' @param v2 the label of the second vertex of interest (in character string format)
#' @param ig the graph representation of the data genealogy (in igraph format)
#' @param geneal the full genealogy  (in data frame format)
#'
#' @examples
#' data(sbGeneal)
#' ig <- dfToIG(sbGeneal)
#' getDegree("Brim", "Bedford", ig, sbGeneal)
#' @export
getDegree = function(v1, v2, ig, geneal){
  if(is.null(geneal)){
    stop("Please input a genealogy data frame")
  }
  if(is.null(ig)){
    stop("Please input an igraph object formatted by dfToIG()")
  }
  path <- getPathOnly(v1=v1, v2=v2, ig=ig, geneal = geneal, isDirected=F)
  # The degree between two vertices is equal to one less than the number of nodes in the shortest path
  return(length(path)-1)
}

#' Returns the nodes for a full genealogy
#'
#' Returns a character list, where rows contains names of the unique nodes in the full genealogy
#'   
#' @param geneal the full genealogy (in data frame format)
#' @examples
#' data(sbGeneal)
#' getNodes(sbGeneal)
#' @export
getNodes = function(geneal){
  nodes = unique(c(geneal$child, geneal$parent))
  nodes = nodes[!is.na(nodes)]
  return(nodes)
}

#' Returns the parents of a particular variety (if they exist)
#' 
#' This function returns up to two values that indicate the parents of the inputted variety.
#' 
#' @param v1 the label of the vertex of interest (in character string format)
#' @param geneal the full genealogy  (in data frame format)
#' @examples
#' data(sbGeneal)
#' getParent("Tokyo", sbGeneal)
#' getParent("Essex", sbGeneal)
#' @export
getParent = function(v1, geneal){
  child <- NULL
  sort(subset(geneal, child==v1)$parent)
}

#' Determine the path between two varieties
#' 
#' Determines the shortest path between the two inputted vertices, and takes into
#' account whether or not the graph is directed. If there is a path, the list of vertices of the
#' path will be returned. If there is not a path, a list of character(0) will be returned. Note:
#' For a directed graph, the direction matters. However, this function will check both directions
#' and return the path if it exists.
#' @param v1 the label of the first vertex of interest (in character string format)
#' @param v2 the label of the second vertex of interest (in character string format)
#' @param ig the graph representation of the data genealogy (in igraph format)
#' @param geneal the full genealogy  (in data frame format)
#' @param colName the name of the column of the data frame that contains the quantitative variable of interest (in character string format)
#' @param silent whether or not to print output (defaults to false) 
#' @param isDirected whether or not the graph is directed (defaults to false)
#' @examples
#' data(sbGeneal)
#' ig <- dfToIG(sbGeneal)
#' getPath("Brim", "Bedford", ig, sbGeneal, "devYear")
#' getPath("Tokyo", "Volstate", ig, sbGeneal, "yield")
#' @export
getPath = function(v1, v2, ig, geneal, colName, silent=FALSE, isDirected=FALSE){
  v1Year = getVariable(v1, geneal, colName)
  v2Year = getVariable(v2, geneal, colName)
  geneal = geneal[which(geneal$parent!=""),]
  ig = dfToIG(geneal)
  if(!is.character(v1) & !is.character(v2)){
    stop("First two arguments must be strings")
  } else {
    if(!v1%in%igraph::V(ig)$name){
      warning("v1 is not a graph vertex")
    }
    if(!v2%in%igraph::V(ig)$name){
      warning("v2 is not a graph vertex")
    }
  }
  if(is.null(geneal)){
    stop("Please input a genealogy data frame where the first two columns are nodes at least one other column is labeled `Year`")
  }
  if(is.null(ig)){
    stop("Please input an igraph object formatted by dfToIG()")
  }
  
  if(igraph::is.directed(ig) != isDirected){
    if(isDirected){
      stop("Cannot compute directed path on an undirected graph")
    }
    warning("Graph type does not match isDirected specification")
  }
  
  retPath = list()
  variableVertices = character()
  pathVertices = character()
  # If the genealogy is directed
  if (igraph::is.directed(ig)){
    # We need to look at both forward and reverse cases of directions, because the user may not know
    # the potential direction of a path between the two vertices
    pathVIndicesForward = igraph::get.shortest.paths(ig, v1, v2, weights = NA, output="vpath")$vpath[[1]]
    pathVIndicesReverse = igraph::get.shortest.paths(ig, v2, v1, weights = NA, output="vpath")$vpath[[1]]
    # If there is a path in the forward direction, then we save the names of the vertices in that order
    if (length(pathVIndicesForward) != 0){
      for (i in 1:length(pathVIndicesForward)){
        pathVertices = c(pathVertices, igraph::get.vertex.attribute(ig, "name", index=pathVIndicesForward[i]))
        variableVertices = c(variableVertices, getVariable(pathVertices[i], geneal, colName))
        if (is.na(getVariable(pathVertices[i], geneal, colName)) && pathVertices[i] == v1){
          variableVertices[i] = v1Year
        }
        if (is.na(getVariable(pathVertices[i], geneal, colName)) && pathVertices[i] == v2){
          variableVertices[i] = v2Year
        }
      }
      retPath = list(pathVertices = pathVertices, variableVertices = variableVertices)
    }
    # If there is a path in the reverse direction, then we save the names of the vertices in that order
    if (length(pathVIndicesReverse) != 0){
      for (i in 1:length(pathVIndicesReverse)){
        pathVertices = c(pathVertices, igraph::get.vertex.attribute(ig, "name", index=pathVIndicesReverse[i]))
        variableVertices = c(variableVertices, getVariable(pathVertices[i], geneal, colName))
        if (is.na(getVariable(pathVertices[i], geneal, colName)) && pathVertices[i] == v1){
          variableVertices[i] = v1Year
        }
        if (is.na(getVariable(pathVertices[i], geneal, colName)) && pathVertices[i] == v2){
          variableVertices[i] = v2Year
        }   
      }
      retPath = list(pathVertices = pathVertices, variableVertices = variableVertices)
    }
  } else {
    # The direction does not matter, any shortest path between the vertices will be listed
    pathVIndices = igraph::get.shortest.paths(ig, v1, v2, weights = NA, output="vpath")$vpath[[1]]
    if (length(pathVIndices) != 0){
      for (i in 1:length(pathVIndices)){
        pathVertices = c(pathVertices, igraph::get.vertex.attribute(ig, "name", index=pathVIndices[i]))
        variableVertices = c(variableVertices, getVariable(pathVertices[i], geneal, colName))
        if (is.na(getVariable(pathVertices[i], geneal, colName)) && pathVertices[i] == v1){
          variableVertices[i] = v1Year
        }
        if (is.na(getVariable(pathVertices[i], geneal, colName)) && pathVertices[i] == v2){
          variableVertices[i] = v2Year
        }
      }
      retPath = list(pathVertices = pathVertices, variableVertices = variableVertices)
    }
  }
  if(length(retPath)==0 & !silent){
    message("Warning: There is no path between those two vertices")
  }
  # Return the shortest path, if it exists
  retPath
}


#' Determine the path between two varieties
#' 
#' Determines the shortest path between the two inputted vertices, and takes into
#' account whether or not the graph is directed. If there is a path, the list of vertices of the
#' path will be returned. If there is not a path, a list of character(0) will be returned. Note:
#' For a directed graph, the direction matters. However, this function will check both directions
#' and return the path if it exists.
#' @param v1 the label of the first vertex of interest (in character string format)
#' @param v2 the label of the second vertex of interest (in character string format)
#' @param ig the graph representation of the data genealogy (in igraph format)
#' @param geneal the full genealogy  (in data frame format)
#' @param silent whether or not to print output (defaults to false) 
#' @param isDirected whether or not the graph is directed (defaults to false)
#' @export
getPathOnly = function(v1, v2, ig, geneal, silent=FALSE, isDirected=FALSE){
  geneal = geneal[which(geneal$parent!=""),]
  ig = dfToIG(geneal)
  if(!is.character(v1) & !is.character(v2)){
    stop("First two arguments must be strings")
  } else {
    if(!v1%in%igraph::V(ig)$name){
      warning("v1 is not a graph vertex")
    }
    if(!v2%in%igraph::V(ig)$name){
      warning("v2 is not a graph vertex")
    }
  }
  if(is.null(geneal)){
    stop("Please input a genealogy data frame where the first two columns are nodes at least one other column is labeled `Year`")
  }
  if(is.null(ig)){
    stop("Please input an igraph object formatted by dfToIG()")
  }
  
  if(igraph::is.directed(ig) != isDirected){
    if(isDirected){
      stop("Cannot compute directed path on an undirected graph")
    }
    warning("Graph type does not match isDirected specification")
  }
  
  retPath = list()
  pathVertices = character()
  # If the genealogy is directed
  if (igraph::is.directed(ig)){
    # We need to look at both forward and reverse cases of directions, because the user may not know
    # the potential direction of a path between the two vertices
    pathVIndicesForward = igraph::get.shortest.paths(ig, v1, v2, weights = NA, output="vpath")$vpath[[1]]
    pathVIndicesReverse = igraph::get.shortest.paths(ig, v2, v1, weights = NA, output="vpath")$vpath[[1]]
    # If there is a path in the forward direction, then we save the names of the vertices in that order
    if (length(pathVIndicesForward) != 0){
      for (i in 1:length(pathVIndicesForward)){
        pathVertices = c(pathVertices, igraph::get.vertex.attribute(ig, "name", index=pathVIndicesForward[i]))
      }
      retPath = pathVertices
    }
    # If there is a path in the reverse direction, then we save the names of the vertices in that order
    if (length(pathVIndicesReverse) != 0){
      for (i in 1:length(pathVIndicesReverse)){
        pathVertices = c(pathVertices, igraph::get.vertex.attribute(ig, "name", index=pathVIndicesReverse[i]))
      }
      retPath = pathVertices
    }
  } else {
    # The direction does not matter, any shortest path between the vertices will be listed
    pathVIndices = igraph::get.shortest.paths(ig, v1, v2, weights = NA, output="vpath")$vpath[[1]]
    if (length(pathVIndices) != 0){
      for (i in 1:length(pathVIndices)){
        pathVertices = c(pathVertices, igraph::get.vertex.attribute(ig, "name", index=pathVIndices[i]))
      }
      retPath = pathVertices
    }
  }
  if(length(retPath)==0 & !silent){
    message("Warning: There is no path between those two vertices")
  }
  # Return the shortest path, if it exists
  retPath
}

#' Determine the date of a variety
#' 
#' Returns the documented date of the inputted variety
#' @param v1 the label of the vertex of interest (in character string format)
#' @param geneal the full genealogy  (in data frame format)
#' @param colName the name of the column of the data frame that contains the quantitative variable of interest (in character string format)
#' @examples
#' data(sbGeneal)
#' getVariable("Essex", sbGeneal, "devYear")
#' getVariable("Tokyo", sbGeneal, "yield")
#' @export
getVariable = function(v1, geneal, colName){
  rowIndex = which(geneal[,1] == v1)[1]
  return(geneal[[colName]][rowIndex])
}

#' Determine if a variety is a child of another
#' 
#' Returns a boolean variable for whether the first variety is a child of the second variety
#' @param child possible child variety
#' @param parent possible parent variety
#' @param geneal the full genealogy  (in data frame format)
#' @examples
#' data(sbGeneal)
#' isChild("Essex", "Young", sbGeneal)
#' isChild("Young", "Essex", sbGeneal)
#' @export
isChild = function(child, parent, geneal){
  for (i in 1:length(which(geneal$parent==parent))){
    # Only consider if the parent is indicated in the genealogy
    if (sum(which(geneal$parent==parent))!=0){
      if (geneal[which(geneal$parent==parent),]$child[i] == child){
        return (TRUE)
      }      
    }
  }
  return (FALSE)
}

#' Determine if a variety is a parent of another
#' 
#' Returns a boolean variable for whether the second variety is a parent of the first variety
#' @param child possible child variety
#' @param parent possible parent variety
#' @param geneal the full genealogy  (in data frame format)
#' @examples
#' data(sbGeneal)
#' isParent("Essex", "Young", sbGeneal)
#' isParent("Young", "Essex", sbGeneal)
#' @export
isParent = function(child, parent, geneal){
  return (geneal[which(geneal$child==child),]$parent[1] == parent
          || geneal[which(geneal$child==child),]$parent[2] == parent)
}

#' Returns the data frame representation of all ancestors and descendants of a variety
#'
#' Converts the list-style-genealogy to a data frame, where each variety has an id value
#' and references its' parent's id value. ID value ranges correspond to generation.
#' It is possible that with more complex genealogical structures the range of id values may need to
#' expand to reduce the probability of two varieties being assigned the same id value.
#'
#' @param tlist list of varieties
#' @param branch of particular variety in the genealogy
#' @param par.id the id of the parent
#' @param id id offset
nodeToDF = local({
  id.offset <- 0
  function(tlist, branch=0, par.id = NA, id=1){
    listidx = which(sapply(tlist, mode)=="list")
    # This is a terminal node
    if(length(listidx)==0){
      temp = as.data.frame(tlist)
      if(nrow(temp)==0) return(data.frame())
      # If gen (not followed by a number), then it does not exist
      if(!"gen"%in%names(temp)){
        id.offset <<- id.offset+1
        return(cbind(as.data.frame(tlist), branch=branch, par.id=par.id, id=id.offset))
      }
      id.offset <<- id.offset+1
      return(cbind(as.data.frame(tlist), branch=branch, par.id=par.id, id=id.offset))
    } else {
      # Grabs everything that does not have children.
      temp = as.data.frame(tlist[-listidx])
      # First time, branchidx = 1 2
      branchidx = listidx-min(listidx)+1
      if(length(branchidx)>1){
        # First time, branchidx = -0.5 0.5
        branchidx = seq(-.5, .5, length.out=length(branchidx))
        # branchidx = equals either -.5 (if temp$gen is even) or .5 (if temp$gen is odd)
      } else branchidx = c(-.5, .5)[temp$gen%%2+1]
      id.offset <<- id.offset+1
      # Creates a unique id
      id = id.offset
      return(plyr::rbind.fill(cbind(temp, branch=branch, id=id, par.id=par.id),
                              plyr::ldply(1:length(listidx), function(i)
                                nodeToDF(tlist[[listidx[i]]], branch=branchidx[i], par.id=id))))
    }
  }
})

#' Returns the image object to show the ancestors and descendants of a variety
#'
#' Returns the image object to show the ancestors and descendants of a variety, with the variety highlighted, if desired
#' 
#' @param v1 the label of the vertex of interest (in character string format)
#' @param mAnc the maximum number of generations of ancestors of v1 to be displayed (in numeric format)
#' @param mDes the maximum number of generations of descendants of v1 to be displayed (in numeric format)
#' @param geneal the full genealogy  (in data frame format)
#' @param vColor the color of the text of the main variety
#' 
#' @export
#' @examples
#' data(sbGeneal)
#' plotAncDes("Tokyo", sbGeneal, vColor = "red")
#' plotAncDes("Essex", sbGeneal, 2, 3, "blue") + ggplot2::labs(x = "Generation index", y = "")
plotAncDes = function(v1, geneal, mAnc=3, mDes=3, vColor="#D35C79"){
  color <- x <- y <- label2 <- size <- xstart <- ystart <- xend <- yend <- branchx <- branchy <- NULL
  # Plot the data frame, if it exists
  geneal = geneal[which(geneal$parent!=""),]
  gDF = buildAncDesTotalDF(v1, geneal, mAnc, mDes)
  gDF[gDF$root.gen==0&gDF$gen==0,]$color = vColor
  if(nrow(gDF)>0){
    plotGenImage = ggplot2::qplot(data=gDF, x=x, y=y, label=label2, geom="text", vjust=-.25, hjust=.5, 
                                  size=size, colour=color) +
      ggplot2::geom_segment(ggplot2::aes(x=xstart, y=ystart, xend=xend, yend=yend),inherit.aes=F) + 
      # Draw the underline of the variety
      ggplot2::geom_segment(ggplot2::aes(x=xend, y=yend, xend=branchx, yend=branchy),inherit.aes=F) +
      # ggplot2::facet_wrap(~variety, scales="free", ncol=2) +
      ggplot2::scale_size_continuous(range=c(3,3),guide="none") +
      ggplot2::scale_colour_identity() +
      ggplot2::theme_bw() +
      ggplot2::theme(panel.grid.major = ggplot2::element_blank(),
                     panel.grid.minor = ggplot2::element_blank(),
                     axis.text=ggplot2::element_blank(), 
                     axis.ticks=ggplot2::element_blank()) +
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

#' Returns the image object to show the heat map of degrees between the inputted set of vertices
#' 
#' Returns the image object to show the heat map of degrees between the inputted set of vertices
#' 
#' @param varieties subset of varieties used to generate the heat map
#' @param ig the graph representation of the data genealogy (in igraph format)
#' @param geneal the full genealogy  (in data frame format)
#' 
#' @seealso \url{http://www.r-project.org} for iGraph information
#' @examples
#' data(sbGeneal)
#' ig <- dfToIG(sbGeneal)
#' varieties <- c("Bedford", "Calland", "Narow", "Pella", "Tokyo", "Young", "Zane")
#' p <- plotDegMatrix(varieties, ig, sbGeneal)
#' p + ggplot2::scale_fill_continuous(low = "white", high = "darkgreen")
#' 
#' @export
plotDegMatrix = function(varieties,ig,geneal){
  Var1 <- Var2 <- value <- NULL
  matVar = matrix(, nrow = length(varieties), ncol = length(varieties))
  for (i in 1:length(varieties)){
    for (j in 1:length(varieties)){
      matVar[i,j]=getDegree(varieties[i],varieties[j],ig,geneal)
    }
  }
  
  tdm <- reshape2::melt(matVar)
  
  heatMap = ggplot2::ggplot(tdm, ggplot2::aes(x = Var1, y = rev(Var2), fill = value)) +
    ggplot2::labs(x = "Variable", y = "Variable", fill = "Degree") +
    ggplot2::geom_raster() +
    ggplot2::scale_x_continuous(breaks=seq(1, length(varieties), 1), labels=varieties) +
    ggplot2::scale_y_continuous(breaks=seq(1, length(varieties), 1), labels=rev(varieties)) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, hjust = 1, size = 15), axis.text.y = ggplot2::element_text(size = 15), axis.title = ggplot2::element_text(size = 15), legend.title = ggplot2::element_text(size = 15), legend.text = ggplot2::element_text(size = 15)) + ggplot2::coord_equal()
  heatMap
}

#' Construct the graphic object of the path
#' 
#' This function takes the path as input and outputs an ggplot2 object. The
#' image will correctly position the node labels with x-axis representing the node
#' date, and y-axis representing the node path index. Edges between two nodes represent
#' parent-child relationships between those nodes. For visual appeal, there is a grey
#' box that outlines the node label, as well as an underline and overline for each label.
#' @param path object created from function getPath
#' @param geneal the full genealogy  (in data frame format)
#' @param colName the name of the column of the data frame that contains the quantitative variable of interest (in character string format)
#' @param colNameY the name of the second optional column of the data frame that contains the second optional quantitative variable of interest (in character string format). This optional quantitative variable will be plotted on the vertical axis.
#' @param fontFace fontface for the two nodes of interest (1=plain, 2=bold, 3=italic, 4=bold-italic), DEFAULT is 1 
#' @seealso \code{\link{getPath}} for information on input path building
#' @export
#' @examples
#' data(sbGeneal)
#' ig <- dfToIG(sbGeneal)
#' pathTN <- getPath("Tokyo", "Narow", sbIG, sbGeneal, "devYear")
#' plotPath(pathTN, sbGeneal, "devYear")
#' 
#' sbFilt <- sbGeneal[complete.cases(sbGeneal[1:3]),]
#' sbFiltIG <- dfToIG(sbFilt)
#' pathCL <- getPath("Clark", "Lawrence", sbFiltIG, sbFilt, "yield")
#' plotPath(pathCL, sbFilt, "devYear", "yield") + ggplot2::xlab("Dev Year") + ggplot2::ylab("Yield")
plotPath = function(path, geneal, colName, colNameY="", fontFace = 1){
  x <- y <- label <- xstart <- ystart <- xend <- yend <- NULL
  if(sum(names(path)%in%c("pathVertices", "variableVertices"))!=2){
    stop("path does not appear to be a result of the getPath() function")
  }
  
  pPDF <- buildPathDF(path, geneal, colName, colNameY)
  pPDF$fontface = rep(1,each=length(path$pathVertices))
  pPDF[pPDF$label==path$pathVertices[1],]$fontface = fontFace
  pPDF[pPDF$label==path$pathVertices[length(path$pathVertices)],]$fontface = fontFace
  
  if (colNameY!=""){
    pPDF$y <- geneal[match(pPDF$label, geneal$child),][[colNameY]]
  }
  
  if (length(dim(pPDF))>1){ # check to make sure pPDF is a data frame
    if (colNameY==""){
      plotPathImage = ggplot2::ggplot(data = pPDF,ggplot2::aes(x = xstart, y = y, label=label)) +
        ggplot2::geom_segment(ggplot2::aes(x=xstart, y=ystart, xend=xend, yend=yend)) +
        ggplot2::geom_label(fill = "grey80", size = 3, fontface=pPDF$fontface) +
        ggplot2::xlab(colName) +
        ggplot2::theme(axis.text.y=ggplot2::element_blank(),axis.ticks.y=ggplot2::element_blank(),
                       axis.title.y=ggplot2::element_blank(),legend.position="none",
                       panel.grid.major.y=ggplot2::element_blank(),
                       panel.grid.minor=ggplot2::element_blank())      
    }
    else{
      plotPathImage = ggplot2::ggplot(data = pPDF,ggplot2::aes(x = xstart, y = y, label=label)) +
        ggplot2::geom_segment(ggplot2::aes(x=xstart, y=ystart, xend=xend, yend=yend)) +
        ggplot2::geom_label(fill = "grey80", size = 3, fontface=pPDF$fontface) +
        ggplot2::xlab(colName) + ggplot2::ylab(colNameY) +
        ggplot2::theme(legend.position="none", panel.grid.minor=ggplot2::element_blank())
    }

  }
  else{
    plotPathImage = print("There is no path to display between the two inputted vertices.")
    plotPathImage = NA
  }
  # Return the plotImage
  plotPathImage
}

#' Plot a path between two vertices over the full genealogy
#' 
#' This function requires a path and the ig object, and plots the full genealogy 
#' with the path highlighted.
#' The image will correctly position the node labels with x-axis representing the node
#' date, and y-axis representing the node path index. Light grey edges between two nodes
#' represent parent-child relationships between those nodes. To enhance the visual
#' understanding of how the path-of-interest fits into the entire graph structure, the
#' nodes within the path are labelled in boldface, and connected with light-green
#' boldfaced edges.
#' @param path path as returned from getPath() or a vector of two variety names which exist in ig
#' @param geneal the full genealogy  (in data frame format)
#' @param ig the graph representation of the data genealogy (in igraph format)
#' @param colName the name of the column of the data frame that contains the quantitative variable of interest (in character string format)
#' @param colNameY the name of the second optional column of the data frame that contains the second optional quantitative variable of interest (in character string format). This optional quantitative variable will be plotted on the vertical axis.
#' @param bin the number of bins to determine the vertical positions of nodes (default is 12). For more information on choosing bin size, please visit the ggenealogy vignette
#' @param edgeCol color of the non-path edges, default is "gray84"
#' @param pathEdgeCol color of the path edges, default is "seagreen"
#' @param nodeSize text size of the non-path node labels, default is 3
#' @param pathNodeSize text size of the path node labels, default is 3
#' @param pathNodeFont font face of text of the path node labels ("plain", "italic", "bold", "bold.italic"), default is "bold"
#' @param animate if the plot will have interactive capabilities, default is FALSE
#' @param nodeCol color of the non-path node labels, default is black
#' @examples
#' data(sbGeneal)
#' sb <- sbGeneal[complete.cases(sbGeneal[1:3]),]
#' ig <- dfToIG(sb)
#' pathCL <- getPath("Clark", "Lawrence", ig, sb, "yield")
#' plotPathOnAll(pathCL, sb, ig, "yield", bin = 3, pathEdgeCol = "red") + ggplot2::xlab("Yield")
#' plotPathOnAll(pathCL, sb, ig, "yield", "devYear") + ggplot2::xlab("Yield") + ggplot2::ylab("Year")
#' @seealso \url{http://www.r-project.org} for iGraph information
#' @seealso \code{\link{getPath}} for information on input path building
#' @export
#' 
plotPathOnAll = function(path, geneal, ig, colName, colNameY = "", bin = 12, edgeCol = "gray84", pathEdgeCol = "seagreen", nodeSize = 3, pathNodeSize = 3, pathNodeFont = "bold", nodeCol = "black", animate = FALSE){
  x <- y <- xend <- yend <- xstart <- ystart <- label <- NULL
  if(class(ig)!="igraph"){
    stop("ig must be an igraph object")
  }
  
  if(mode(path)=="character"){
    if(length(path)!=2){
      stop("path needs to contain two variety names")
    }
    varieties <- path
    path <- getPath(varieties[1], varieties[2], ig, colName)
  } else if(sum(names(path)%in%c("pathVertices", "variableVertices"))!=2){
    stop("path does not appear to be a result of the getPath() function")
  } 
  
  pMPDF <- buildMinusPathDF(path, geneal, ig, colName, colNameY, bin)
  eTDF <- buildEdgeTotalDF(geneal, ig, colName, bin)
  pTDF <- buildPlotTotalDF(path, geneal, ig, colName, colNameY, bin)
  
  eTDF <- stats::na.omit(eTDF) #remove any row that has at least one NA
  
  textFrame = data.frame(x = pMPDF$x, y = pMPDF$y, label = pMPDF$label)
  textFrame = transform(textFrame,
                        w = graphics::strwidth(pMPDF$label, 'inches') + 0.25,
                        h = graphics::strheight(pMPDF$label, 'inches') + 0.25
  )
  
  textFrame <- stats::na.omit(textFrame) #remove any row that has at least one NA
  eTDF <- eTDF[(eTDF$x %in% textFrame$x) & (eTDF$xend %in% textFrame$x),]
  
  rowETDF <- rownames(eTDF)
  rowTextFrame <- rownames(textFrame)
  rownames(eTDF) <- 1:nrow(eTDF)
  rownames(textFrame) <- 1:nrow(textFrame)
  eTDF$y <- textFrame[match(eTDF$name, textFrame$label),]$y
  eTDF$yend <- textFrame[match(eTDF$nameEnd, textFrame$label),]$y
  eTDF <- eTDF[,-c(5,6)]
  rownames(eTDF) <- rowETDF
  rownames(textFrame) <- rowTextFrame
  eTDF$x <- as.integer(as.character(eTDF$x))
  eTDF$xend <- as.integer(as.character(eTDF$xend)) 
  
  # The plotTotalImage object creates two line segments (geom_segment), one to create grey
  # edges for non-path connections between pairs of nodes, the other to create light-green
  # edges for path connections between pairs of nodes; and two labels (geom_text), one to
  # create labels of size 2 for non-path connections between pairs of nodes, the other to
  # create labels of size 2.5 and boldfaced for path connections between pairs of nodes.
  plotTotalImage = ggplot2::ggplot(data = pMPDF, ggplot2::aes(x = x, y = y)) +
    ggplot2::geom_segment(data = eTDF, ggplot2::aes(x=x, y=y, xend=xend, yend=yend), colour = edgeCol) +
    ggplot2::geom_segment(data = pTDF, ggplot2::aes(x=xstart, y=ystart, xend=xend, yend=yend), colour = pathEdgeCol, size = 1) +
      ggplot2::geom_text(data = textFrame, ggplot2::aes(x = x, y = y, label = label), size = nodeSize, colour = nodeCol)

  plotTotalImage = plotTotalImage + ggplot2::geom_text(data = pTDF,ggplot2::aes(x = x, y = y, label = label), size = pathNodeSize, fontface=pathNodeFont) +
    ggplot2::xlab(colName) +
    ggplot2::theme(legend.position="none", panel.grid.minor=ggplot2::element_blank())
  
  if (colNameY == ""){
    plotTotalImage = plotTotalImage + ggplot2::theme(axis.text.y=ggplot2::element_blank(),axis.ticks.y=ggplot2::element_blank(), axis.title.y=ggplot2::element_blank(), panel.grid.major.y=ggplot2::element_blank())
  }
  
  if (colNameY != ""){
    plotTotalImage = plotTotalImage + ggplot2::ylab(colNameY)
  }
  
  # Return the plotTotalImage, if animate is FALSE
  if (animate==FALSE){
    plotTotalImage    
  }
  # Return the animatePlotTotalImage, if animate is TRUE
  else{
    if (colNameY == ""){
      animatePlotTotalImage <- plotly::plotly_build(plotly::ggplotly(plotTotalImage, tooltip = c("x", "label")))
      animatePlotTotalImage$data[[1]]$hoverinfo <- "none"
      animatePlotTotalImage$data[[2]]$hoverinfo <- "none"
      animatePlotTotalImage$data[[3]]$hoverinfo <- c("x+text")
      animatePlotTotalImage$data[[4]]$hoverinfo <- c("x+text")
      animatePlotTotalImage 
    }
    else{
      animatePlotTotalImage <- plotly::plotly_build(plotly::ggplotly(plotTotalImage, tooltip = c("x", "label")))
      animatePlotTotalImage$data[[1]]$hoverinfo <- "none"
      animatePlotTotalImage$data[[2]]$hoverinfo <- "none"
      animatePlotTotalImage$data[[3]]$hoverinfo <- c("x+y+text")
      animatePlotTotalImage$data[[4]]$hoverinfo <- c("x+y+text")
      animatePlotTotalImage       
    }
  }
}

#' Returns the image object to show the heat map of dates between the inputted set of vertices
#' 
#' Returns the image object to show the heat map of dates between the inputted set of vertices
#' 
#' @param varieties subset of varieties used to generate the heat map
#' @param geneal the full genealogy  (in data frame format)
#' @param colName the name of the column of the data frame that contains the quantitative variable of interest (in character string format)
#' @param xLab string label on the x axis (default is "Variety")
#' @param yLab string label on the y axis (default is "Variety")
#' @param legendLab string label on the legend (default is "Degree")
#' @examples
#' data(sbGeneal)
#' varieties <- c("Bedford", "Calland", "Narow", "Pella", "Tokyo", "Young", "Zane")
#' p <- plotVariableMatrix(varieties, sbGeneal, "devYear", "Variety", "Variety", "Difference")
#' p + ggplot2::scale_fill_continuous(low = "white", high = "darkgreen")
#' 
#' @export
plotVariableMatrix = function(varieties, geneal, colName, xLab = "Variety", yLab = "Variety", legendLab = "Difference in variable"){
  Var1 <- Var2 <- value <- NULL
  matVar = matrix(, nrow = length(varieties), ncol = length(varieties))
  for (i in 1:length(varieties)){
    for (j in 1:length(varieties)){
      matVar[i,j]=abs(getVariable(varieties[i],geneal,colName)-getVariable(varieties[j],geneal,colName))
    }
  }
  
  tdm <- reshape2::melt(matVar)
  
  heatMap = ggplot2::ggplot(tdm, ggplot2::aes(x = Var1, y = rev(Var2), fill = value)) +
    ggplot2::labs(x = xLab, y = yLab, fill = legendLab) +
    ggplot2::geom_raster() +
    ggplot2::scale_x_continuous(breaks=seq(1, length(varieties), 1), labels=varieties) +
    ggplot2::scale_y_continuous(breaks=seq(1, length(varieties), 1), labels=rev(varieties)) +
    ggplot2::theme_bw() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, hjust = 1)) + ggplot2::coord_equal()
  heatMap
}

#' Process the genealogy graph
#' 
#' Processes the genealogy into an igraph object with appropriate vertex information, graph type, and edge weights.
#' 
#' @param geneal the full genealogy  (in data frame format)
#' @param vertexinfo (default NULL) either names of columns in the genealogy which should be added to the database as vertex information or a data frame with information for all vertices such that the first column contains vertex names.
#' @param edgeweights (default 1) name of a column which contains edge weights
#' @param isDirected (default FALSE) should the graph be a directed graph?
#' @seealso \url{http://www.r-project.org} for iGraph information
#' @export
dfToIG = function(geneal, vertexinfo = NULL, edgeweights = 1, isDirected=FALSE){
  parent <- child <- NULL
  if(!is.data.frame(geneal)){
    stop("The input must be of type data frame")
  }
  
  if(!("parent"%in%names(geneal) & "child"%in%names(geneal))){
    stop("The geneal must contain columns named 'parent' and 'child'")
  }
  
  if(is.null(vertexinfo)){
    nodes <- unique(c(geneal$child, geneal$parent))
    nodes <- nodes[!is.na(nodes)]
  } else if(is.character(vertexinfo)){
    nodes <- geneal[,c("child", vertexinfo)]
    # add in any parents who are not in the list of children, sans any vertex information
    if(sum(!geneal$parent%in%geneal$child & !is.na(geneal$parent))>0){
      absentparents <- unique(geneal$parent[!geneal$parent%in%geneal$child & !is.na(geneal$parent)])
      nodes <- plyr::rbind.fill(nodes, data.frame(child=absentparents, stringsAsFactors = FALSE))
    }
    nodes <- unique(nodes)
  } else if(is.data.frame(vertexinfo)) {
    nodes <- unique(vertexinfo)
  } else {
    stop("vertexinfo should be either NULL, a character vector, or a data frame")
  }
  
  edges <- subset(geneal, !is.na(parent) & !is.na(child))[,c("child", "parent")]
  edges$weight <- edgeweights
  
  igraph::graph.data.frame(d=edges, directed=isDirected, vertices=nodes)
}

#' Descendant branch calculations for quantitative variable 
#' 
#' Returns a data frame containing the names of all children of an individual of interest ("Name"). The mean and standard deviation ("Mean" and "SD") of a quantitative variable across all descendents of each child is reported. In addition, for each child, the number of its descendants is reported ("Count"), the number of its descendants who do not have a value for the quantitative variable ("NACount") is reported, and the names of all of its descendants is reported ("DesNames").
#' 
#' @param v1 the label of the vertex of interest (in character string format)
#' @param geneal the full genealogy  (in data frame format)
#' @param colName the name of the column of the data frame that contains the quantitative variable of interest (in character string format)
#' @param gen the number of generations back to include as ancestors
#' @examples
#' data(statGeneal)
#' DC_Year <- getBranchQuant("David Cox", statGeneal, "gradYear", 15)
#' 
#' @export
getBranchQuant = function(v1, geneal, colName, gen=3){
  id.offset <- NULL
  if (is.null(getChild(v1, geneal))){
    return(data.frame())
  }
  childList <- getChild(v1,geneal)
  datRet <- data.frame()
  for (i in 1:length(childList)){
    dat <- getDescendants(childList[i], geneal, gen)
    dat[[colName]] <- geneal[match(dat$label, geneal$child),][[colName]]
    DesNames <- dat$label
    Count <- length(DesNames)
    Mean <- mean(dat[[colName]], na.rm=TRUE)
    SD <- stats::sd(dat[[colName]], na.rm = TRUE)
    NACount <- sum(is.na(dat[[colName]]))
    datRet <- rbind(datRet, data.frame(Name=childList[i], Mean=Mean, SD=SD, Count=Count, NACount=NACount, DesNames=paste(DesNames, collapse = ',')))
  }
  datRet <- datRet[order(-datRet$Mean),]
  rownames(datRet) <- 1:nrow(datRet)
  return(datRet)
}

#' Descendant branch calculations for quantitative variable 
#' 
#' Returns a data frame containing the names of all children of an individual of interest ("Name"). The mean and standard deviation ("Mean" and "SD") of a quantitative variable across all descendents of each child is reported. In addition, for each child, the number of its descendants is reported ("Count"), the number of its descendants who do not have a value for the quantitative variable ("NACount") is reported, and the names of all of its descendants is reported ("DesNames").
#' 
#' @param v1 the label of the vertex of interest (in character string format)
#' @param geneal the full genealogy  (in data frame format)
#' @param colName the name of the column of the data frame that contains the qualitative variable of interest (in character string format)
#' @param rExpr regular expression to be applied to the column that contains the qualitative variable of interest (in character string format). The regular expression syntax must work on a data frame column of type character. The term geneal$colName must be used in the regular expression.
#' @param gen the number of generations back to include as ancestors
#' @examples
#' data(statGeneal)
#' rExpr = "geneal$colName=='The Johns Hopkins University'"
#' DC_JHU = getBranchQual("David Cox", statGeneal, "school", rExpr, 15)
#' rExpr = "geneal$colName=='UnitedKingdom'"
#' DC_UK = getBranchQual("David Cox", statGeneal, "country", rExpr, 15)
#' rExpr = "grepl('(?i)Stochastic', geneal$colName)"
#' DC_Stochastic = getBranchQual("David Cox", statGeneal, "thesis", rExpr, 15)
#' @export
#' 
getBranchQual = function(v1, geneal, colName, rExpr, gen=3){
  id.offset <- NULL
  if (is.null(getChild(v1, geneal))){
    return(data.frame())
  }
  childList <- getChild(v1,geneal)
  datRet <- data.frame()
  for (i in 1:length(childList)){
    dat <- getDescendants(childList[i], geneal, gen)
    dat[[colName]] <- geneal[match(dat$label, geneal$child),][[colName]]
    DesNames <- dat$label
    Count <- length(DesNames)
    rExpr = gsub("geneal[$]colName", "dat[[colName]]", rExpr)
    CountTrue <- sum(eval(parse(text=rExpr)),na.rm=TRUE)
    NACount <- sum(dat[[colName]]=="")
    datRet <- rbind(datRet, data.frame(Name=childList[i], CountTrue = CountTrue, Count=Count, NACount=NACount, DesNames=paste(DesNames, collapse = ',')))
  }
  datRet <- datRet[order(-datRet$CountTrue),]
  rownames(datRet) <- 1:nrow(datRet)
  return(datRet)
}
